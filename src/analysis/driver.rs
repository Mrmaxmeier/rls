// src/cli_driver.rs -- Command-line driver for the Tectonic engine.
// Copyright 2016-2017 the Tectonic Project
// Licensed under the MIT License.

// #[macro_use] extern crate tectonic;

use aho_corasick::{Automaton, AcAutomaton};
use std::collections::{HashMap, HashSet};
use std::ffi::{OsStr, OsString};
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process;
use std::sync::Arc;
use std::sync::mpsc::{channel, Sender, Receiver};
use std::thread;

use parking_lot::Mutex;

use tectonic::config::PersistentConfig;
use tectonic::digest::DigestData;
use tectonic::engines::IoEventBackend;
use tectonic::errors::{ErrorKind, Result, ResultExt};
use tectonic::io::{FilesystemIo, GenuineStdoutIo, InputOrigin, IoProvider, IoStack, MemoryIo, OpenResult};
use tectonic::status::NoopStatusBackend;
use tectonic::{TexEngine, TexResult};
use vfs::Vfs;

use analysis::build_queue::{BuildContext, BuildResult};
use analysis::vfs_wrapper::VfsIoProvider;

/// The CliIoSetup struct encapsulates, well, the input/output setup used by
/// the Tectonic engines in this CLI session.
///
/// The IoStack struct must necessarily erase types (i.e., turn I/O layers
/// into IoProvider trait objects) while it lives. But, between invocations of
/// various engines, we want to look at our individual typed I/O providers and
/// interrogate them (i.e., see what files were created in the memory layer.
/// The CliIoSetup struct helps us maintain detailed knowledge of types while
/// creating an IoStack when needed. In principle we could reuse the same
/// IoStack for each processing step, but the borrow checker doesn't let us
/// poke at (e.g.) io.mem while the IoStack exists, since the IoStack keeps a
/// mutable borrow of it.

struct CliIoSetup {
    filesystem: FilesystemIo,
    mem: MemoryIo,
    vfs: VfsIoProvider,
    bundle: Box<IoProvider>,
    genuine_stdout: Option<GenuineStdoutIo>,
}

impl CliIoSetup {
    fn new(bundle: Box<IoProvider>, vfs: Arc<Vfs>) -> Result<CliIoSetup> {
        Ok(CliIoSetup {
            mem: MemoryIo::new(true),
            bundle: bundle,
            vfs: VfsIoProvider::new(vfs),
            filesystem: FilesystemIo::new(Path::new(""), false, true, HashSet::new()),
            genuine_stdout: None, // Some(GenuineStdoutIo::new()),
        })
    }

    fn as_stack<'a> (&'a mut self, with_filesystem: bool) -> IoStack<'a> {
        let mut providers: Vec<&mut IoProvider> = Vec::new();

        if let Some(ref mut p) = self.genuine_stdout {
            providers.push(p);
        }

        providers.push(&mut self.vfs);
        providers.push(&mut self.mem);

        if with_filesystem {
            providers.push(&mut self.filesystem);
        }

        providers.push(&mut *self.bundle);

        IoStack::new(providers)
    }
}


/// Different patterns with which files may have been accessed by the
/// underlying engines. Once a file is marked as ReadThenWritten or
/// WrittenThenRead, its pattern does not evolve further.
#[derive(Clone,Copy,Debug,Eq,PartialEq)]
enum AccessPattern {
    /// This file is only ever read.
    Read,

    /// This file is only ever written. This suggests that it is
    /// a final output of the processing session.
    Written,

    /// This file is read, then written. We call this a "circular" access
    /// pattern. Multiple passes of an engine will result in outputs that
    /// change if this file's contents change, or if the file did not exist at
    /// the time of the first pass.
    ReadThenWritten,

    /// This file is written, then read. We call this a "temporary" access
    /// pattern. This file is likely a temporary buffer that is not of
    /// interest to the user.
    WrittenThenRead,
}


/// A summary of the I/O that happened on a file. We record its access
/// pattern; where it came from, if it was used as an input; the cryptographic
/// digest of the file when it was last read; and the cryptographic digest of
/// the file as it was last written.
#[derive(Clone,Debug,Eq,PartialEq)]
struct FileSummary {
    access_pattern: AccessPattern,
    input_origin: InputOrigin,
    read_digest: Option<DigestData>,
    write_digest: Option<DigestData>,
    got_written_to_disk: bool,
}

impl FileSummary {
    fn new(access_pattern: AccessPattern, input_origin: InputOrigin) -> FileSummary {
        FileSummary {
            access_pattern: access_pattern,
            input_origin: input_origin,
            read_digest: None,
            write_digest: None,
            got_written_to_disk: false,
        }
    }
}


/// The CliIoEvents type implements the IoEventBackend. The CLI uses it to
/// figure out when to rerun the TeX engine; to figure out which files should
/// be written to disk; and to emit Makefile rules.
struct CliIoEvents(HashMap<OsString, FileSummary>);

impl CliIoEvents {
    fn new() -> CliIoEvents { CliIoEvents(HashMap::new()) }
}

impl IoEventBackend for CliIoEvents {
    fn output_opened(&mut self, name: &OsStr) {
        if let Some(summ) = self.0.get_mut(name) {
            summ.access_pattern = match summ.access_pattern {
                AccessPattern::Read => AccessPattern::ReadThenWritten,
                c => c, // identity mapping makes sense for remaining options
            };
            return;
        }

        self.0.insert(name.to_os_string(), FileSummary::new(AccessPattern::Written, InputOrigin::NotInput));
    }

    fn stdout_opened(&mut self) {
        // Life is easier if we track stdout in the same way that we do other
        // output files.

        if let Some(summ) = self.0.get_mut(OsStr::new("")) {
            summ.access_pattern = match summ.access_pattern {
                AccessPattern::Read => AccessPattern::ReadThenWritten,
                c => c, // identity mapping makes sense for remaining options
            };
            return;
        }

        self.0.insert(OsString::from(""), FileSummary::new(AccessPattern::Written, InputOrigin::NotInput));
    }

    fn output_closed(&mut self, name: OsString, digest: DigestData) {
        let mut summ = self.0.get_mut(&name).expect("closing file that wasn't opened?");
        summ.write_digest = Some(digest);
    }

    fn input_not_available(&mut self, name: &OsStr) {
        // For the purposes of file access pattern tracking, an attempt to
        // open a nonexistent file counts as a read of a zero-size file. I
        // don't see how such a file could have previously been written, but
        // let's use the full update logic just in case.

        if let Some(summ) = self.0.get_mut(name) {
            summ.access_pattern = match summ.access_pattern {
                AccessPattern::Written => AccessPattern::WrittenThenRead,
                c => c, // identity mapping makes sense for remaining options
            };
            return;
        }

        // Unlike other cases, here we need to fill in the read_digest. `None`
        // is not an appropriate value since, if the file is written and then
        // read again later, the `None` will be overwritten; but what matters
        // is the contents of the file the very first time it was read.
        let mut fs = FileSummary::new(AccessPattern::Read, InputOrigin::NotInput);
        fs.read_digest = Some(DigestData::of_nothing());
        self.0.insert(name.to_os_string(), fs);
    }

    fn input_opened(&mut self, name: &OsStr, origin: InputOrigin) {
        if let Some(summ) = self.0.get_mut(name) {
            summ.access_pattern = match summ.access_pattern {
                AccessPattern::Written => AccessPattern::WrittenThenRead,
                c => c, // identity mapping makes sense for remaining options
            };
            return;
        }

        self.0.insert(name.to_os_string(), FileSummary::new(AccessPattern::Read, origin));
    }

    fn input_closed(&mut self, name: OsString, digest: Option<DigestData>) {
        let mut summ = self.0.get_mut(&name).expect("closing file that wasn't opened?");

        // It's what was in the file the *first* time that it was read that
        // matters, so don't replace the read digest if it's already got one.

        if summ.read_digest.is_none() {
            summ.read_digest = digest;
        }
    }
}


/// The AnalysisDriver struct runs the whole show when we're actually
/// processing a file. It merges the command-line arguments and the persistent
/// configuration to figure out what exactly we're going to do.

pub struct AnalysisDriver {
    io: CliIoSetup,
    events: CliIoEvents,
    format_path: String,
    notes: Vec<String>, // current compilation notes
    status: NoopStatusBackend
}


const DEFAULT_MAX_TEX_PASSES: usize = 6;

impl AnalysisDriver {
    pub fn new(vfs: Arc<Vfs>) -> Result<AnalysisDriver> {

        let config = PersistentConfig::open(false)?;
        let mut status = NoopStatusBackend {};

        // Set up I/O.

        let config = PersistentConfig::open(false).unwrap();
        let bundle = config.default_io_provider(&mut status).unwrap();
        let io = CliIoSetup::new(bundle, vfs)?;

        // Ready to roll.

        Ok(AnalysisDriver {
            io: io,
            events: CliIoEvents::new(),
            format_path: "latex".to_owned(),
            status: status,
            notes: Vec::new()
        })
    }

    pub fn spawn(vfs: Arc<Vfs>) -> Sender<BuildContext> {
        let (tx, rx) = channel();
        thread::spawn(move|| {
            let driver = AnalysisDriver::new(vfs).unwrap();
            let (rtx, rrx) = channel();
            tx.send(rtx).unwrap();
            driver.serve(rrx);
        });
        rx.recv().unwrap()
    }

    fn serve(mut self, request_chan: Receiver<BuildContext>) {
        for request in &request_chan {
            eprintln!("new build request: {:?}", request);
            self.io.vfs.set_base(request.directory.as_path());
            let result = match self.run(&request.file) {
                Ok(_) => BuildResult::Success(self.notes.clone(), None), // TODO
                Err(err) => {
                    eprintln!("Build error: {:?}", err);
                    BuildResult::Err
                },
            };
            eprintln!("reporting result: {:?}", result);
            request.result_chan.send(result).unwrap();
        }
    }

    /// Assess whether we need to rerun an engine. This is the case if there
    /// was a file that the engine read and then rewrote, and the rewritten
    /// version is different than the version that it read in.
    fn rerun_needed(&mut self) -> Option<String> {
        // TODO: we should probably wire up diagnostics since I expect this
        // stuff could get finicky and we're going to want to be able to
        // figure out why rerun detection is breaking.

        for (name, info) in &self.events.0 {
            if info.access_pattern == AccessPattern::ReadThenWritten {
                let file_changed = match (&info.read_digest, &info.write_digest) {
                    (&Some(ref d1), &Some(ref d2)) => d1 != d2,
                    (&None, &Some(_)) => true,
                    (_, _) => {
                        panic!("internal consistency problem when checking if {} changed",
                               name.to_string_lossy());
                    }
                };

                if file_changed {
                    return Some(name.to_string_lossy().into_owned());
                }
            }
        }

        None
    }

    pub fn run(&mut self, tex_path: &PathBuf) -> Result<()> {
        // Do the meat of the work.
        self.notes = Vec::new();

        let generate_format = {
            let fmt_result = {
                let mut stack = self.io.as_stack(true);
                stack.input_open_format(OsStr::new(&self.format_path), &mut self.status)
            };

            match fmt_result {
                OpenResult::Ok(_) => false,
                OpenResult::NotAvailable => true,
                OpenResult::Err(e) => {
                    return Err(e).chain_err(|| format!("could not open format file {}", self.format_path));
                },
            }
        };

        if generate_format {
            self.notes.push(format!("generating format '{}'", self.format_path));
            self.make_format_pass()?;
        }

        self.default_pass(tex_path)?;
        self.notes.push(format!("successfully ran default pass on {:?}", tex_path));


        let mut n_skipped_intermediates = 0;

        for (name, contents) in &*self.io.mem.files.borrow() {
            if name == self.io.mem.stdout_key() {
                continue;
            }

            let sname = name.to_string_lossy();
            let mut summ = self.events.0.get_mut(name).unwrap();

             if summ.access_pattern != AccessPattern::Written {
                n_skipped_intermediates += 1;
                continue;
            }

            if sname.ends_with(".log") || sname.ends_with(".blg") || sname.ends_with(".aux") {
                continue;
            }

            if contents.len() == 0 {
                self.notes.push(format!("Writing {}: it would be empty", &sname));
                continue;
            }

            self.notes.push(format!("Writing {} ({} bytes)", &sname, contents.len()));

            let mut f = File::create(Path::new(name))?;
            f.write_all(contents)?;
            summ.got_written_to_disk = true;
        }

        if n_skipped_intermediates > 0 {
            self.notes.push(format!("Skipped writing {} intermediate files", n_skipped_intermediates));
        }

        // All done.

        Ok(())
    }

    /// Use the TeX engine to generate a format file.
    fn make_format_pass(&mut self) -> Result<i32> {
        // PathBuf.file_stem() doesn't do what we want since it only strips
        // one extension. As of 1.17, the compiler needs a type annotation for
        // some reason, which is why we use the `r` variable.
        let r: Result<&str> = self.format_path.splitn(2, ".").next().ok_or_else(
            || ErrorKind::Msg(format!("incomprehensible format file name \"{}\"", self.format_path)).into()
        );
        let stem = r?;

        let result = {
            let mut stack = self.io.as_stack(false);
            let mut engine = TexEngine::new();
            engine.set_halt_on_error_mode(true);
            engine.set_initex_mode(true);
            engine.process(&mut stack, &mut self.events, &mut self.status, "UNUSED.fmt.gz",
                           &format!("tectonic-format-{}.tex", stem))
        };

        match result {
            Ok(TexResult::Spotless) => {},
            Ok(TexResult::Warnings) => {
                // tt_warning!(status, "warnings were issued by the TeX engine; use --print and/or --keep-logs for details.");
            },
            Ok(TexResult::Errors) => {
                // tt_error!(status, "errors were issued by the TeX engine; use --print and/or --keep-logs for details.");
                return Err(ErrorKind::Msg("unhandled TeX engine error".to_owned()).into());
            },
            Err(e) => {
                if let Some(output) = self.io.mem.files.borrow().get(self.io.mem.stdout_key()) {
                    /*
                    tt_error!(status, "something bad happened inside TeX; its output follows:\n");
                    tt_error_styled!(status, "===============================================================================");
                    status.dump_to_stderr(&output);
                    tt_error_styled!(status, "===============================================================================");
                    tt_error_styled!(status, "");
                    */
                    eprintln!("{}", ::std::str::from_utf8(output).unwrap());
                    self.notes.push(::std::str::from_utf8(output).unwrap().to_owned());
                }

                return Err(e);
            }
        }

        // Now we can write the format file to its special location. In
        // principle we could stream the format file directly to the staging
        // area as we ran the TeX engine, but we don't bother.

        let bundle = &mut *self.io.bundle.as_mut();

        for (name, contents) in &*self.io.mem.files.borrow() {
            if name == self.io.mem.stdout_key() {
                continue;
            }

            let sname = name.to_string_lossy();

            if !sname.ends_with(".fmt.gz") {
                continue;
            }

            // Note that we intentionally pass 'stem', not 'name'.
            bundle.write_format(stem, contents, &mut self.status).chain_err(|| format!("cannot write format file {}", sname))?;
        }

        // All done. Clear the memory layer since this was a special preparatory step.
        self.io.mem.files.borrow_mut().clear();

        Ok(0)
    }

    /// The "default" pass really runs a bunch of sub-passes. It is a "Do What
    /// I Mean" operation.
    fn default_pass(&mut self, tex_path: &PathBuf) -> Result<()> {
        self.tex_pass(tex_path, None)?;

        let mut aux_path = tex_path.clone();
        aux_path.set_extension("aux");

        let mut rerun_result = self.rerun_needed();

        // Now we enter the main rerun loop.

        let pass_count = DEFAULT_MAX_TEX_PASSES;

        for i in 0..pass_count {
            let rerun_explanation = 
                match rerun_result {
                    Some(ref s) => {
                        if s == "" {
                            "bibtex was run".to_owned()
                        } else {
                            format!("\"{}\" changed", s)
                        }
                    },
                    None => {
                        eprintln!("no rerun needed");
                        break;
                }
            };

            // We're restarting the engine afresh, so clear the read inputs.
            // We do *not* clear the entire HashMap since we want to remember,
            // e.g., that bibtex wrote out the .bbl file, since that way we
            // can later know that it's OK to delete. I am not super confident
            // that the access_pattern data can just be left as-is when we do
            // this, but, uh, so far it seems to work.
            {
                for summ in self.events.0.values_mut() {
                    summ.read_digest = None;
                }
            }

            self.tex_pass(tex_path, Some(&rerun_explanation))?;

            rerun_result = self.rerun_needed();

            if rerun_result.is_some() && i == DEFAULT_MAX_TEX_PASSES - 1 {
                break;
            }
        }

        Ok(())
    }

    /// Run one pass of the TeX engine.

    fn tex_pass(&mut self, tex_path: &PathBuf, rerun_explanation: Option<&str>) -> Result<()> {
        eprintln!("TeX_pass({:?})", tex_path);
        self.notes.push(format!("TeX pass on file: {:?}", tex_path));
        let result = {
            let mut stack = self.io.as_stack(true);
            let mut engine = TexEngine::new();
            engine.set_halt_on_error_mode(true);
            engine.set_initex_mode(false);
            if let Some(s) = rerun_explanation {
                self.notes.push(format!("Rerunning TeX because {} ...", s));
            } else {
                self.notes.push("Rerunning Tex".to_owned());
            }

            engine.process(&mut stack, &mut self.events, &mut self.status,
                           &self.format_path, &tex_path.to_str().unwrap())
        };

        match result {
            Ok(TexResult::Spotless) => {},
            Ok(TexResult::Warnings) => {},
            Ok(TexResult::Errors) => {},
            Err(e) => {
                if let Some(output) = self.io.mem.files.borrow().get(self.io.mem.stdout_key()) {
                    // status.dump_to_stderr(&output);
                    eprintln!("{}", ::std::str::from_utf8(output).unwrap());
                    self.notes.push(::std::str::from_utf8(output).unwrap().to_owned());
                }

                return Err(e);
            }
        }

        Ok(())
    }
}
