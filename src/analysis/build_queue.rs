// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![allow(dead_code)]

use data::Analysis;
use analysis::AnalysisDriver;
use vfs::Vfs;

use std::collections::HashMap;
use std::env;
use std::ffi::OsString;
// use std::fs::{read_dir, remove_file};
use std::io::{self, Write};
use std::mem;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{channel, Sender, Receiver};
use std::thread;
use std::time::Duration;


#[derive(Debug)]
pub enum BuildResult {
    // Build was succesful, argument is warnings.
    Success(Vec<String>, Option<Analysis>),
    // Build finished with errors, argument is errors and warnings.
    Failure(Vec<String>, Option<Analysis>),
    // Build was coelesced with another build.
    Squashed,
    // There was an error attempting to build.
    Err,
}

#[derive(Debug)]
pub struct BuildContext {
    directory: PathBuf,
    file: PathBuf,
    result_chan: Sender<BuildResult>
}

/// Priority for a build request.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BuildPriority {
    /// Run this build as soon as possible (e.g., on save or explicit build request).
    Immediate,
    /// A regular build request (e.g., on a minor edit).
    Normal,
}

// Minimum time to wait before starting a `BuildPriority::Normal` build.
const WAIT_TO_BUILD: u64 = 750;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Signal {
    Build,
    Skip,
}

/// Manages builds.
///
/// The IDE will request builds quickly (possibly on every keystroke), there is
/// no point running every one. We also avoid running more than one build at once.
/// We cannot cancel builds. It might be worth running builds in parallel or
/// cancelling a started build.
///
/// `BuildPriority::Immediate` builds are started straightaway. Normal builds are
/// started after a timeout. A new build request cancels any pending build requests.
///
/// From the client's point of view, a build request is not guaranteed to cause
/// a build. However, a build is guaranteed to happen and that build will begin
/// after the build request is received (no guarantee on how long after), and
/// that build is guaranteed to have finished before the build reqest returns.
///
/// There is no way for the client to specify that an individual request will
/// result in a build. However, you can tell from the result - if a build
/// was run, the build result will contain any errors or warnings and an indication
/// of success or failure. If the build was not run, the result indicates that
/// it was squashed.
pub struct BuildQueue {
    build_dir: Mutex<Option<PathBuf>>,
    tex_path: Mutex<Option<PathBuf>>,
    request_chan: Mutex<Sender<BuildContext>>,
    // True if a build is running.
    // Note I have been conservative with Ordering when accessing this atomic,
    // we might be able to do better.
    running: AtomicBool,
    // A vec of channels to pending build threads.
    pending: Mutex<Vec<Sender<Signal>>>,
    vfs: Arc<Vfs>,
}

impl BuildQueue {
    pub fn connected(vfs: Arc<Vfs>) -> (BuildQueue, AnalysisDriver) {
        let (req_tx, req_rx) = channel();
        let bq = BuildQueue {
            request_chan: Mutex::new(req_tx),
            build_dir: Mutex::new(None),
            tex_path: Mutex::new(None),
            running: AtomicBool::new(false),
            pending: Mutex::new(vec![]),
            vfs: vfs,
        };
        let driver = AnalysisDriver::new(req_rx).unwrap();
        (bq, driver)
    }

    pub fn request_build(&self, build_dir: &Path, build_file: &Path, priority: BuildPriority) -> BuildResult {
        eprintln!("request_build, {:?} {:?} {:?}", build_dir, build_file, priority);

        // If there is a change in the project directory, then we can forget any
        // pending build and start straight with this new build.
        {
            let mut prev_build_dir = self.build_dir.lock().unwrap();

            if prev_build_dir.as_ref().map_or(true, |dir| dir != build_dir) {
                *prev_build_dir = Some(build_dir.to_owned());
                self.cancel_pending();
            }
        }

        self.cancel_pending();

        match priority {
            BuildPriority::Immediate => {
                // There is a build running, wait for it to finish, then run.
                if self.running.load(Ordering::SeqCst) {
                    let (tx, rx) = channel();
                    self.pending.lock().unwrap().push(tx);
                    // Blocks.
                    // println!("blocked on build");
                    let signal = rx.recv().unwrap_or(Signal::Build);
                    if signal == Signal::Skip {
                        return BuildResult::Squashed;
                    }
                }
            }
            BuildPriority::Normal => {
                let (tx, rx) = channel();
                self.pending.lock().unwrap().push(tx);
                thread::sleep(Duration::from_millis(WAIT_TO_BUILD));

                if self.running.load(Ordering::SeqCst) {
                    // Blocks
                    // println!("blocked until wake up");
                    let signal = rx.recv().unwrap_or(Signal::Build);
                    if signal == Signal::Skip {
                        return BuildResult::Squashed;
                    }
                } else if rx.try_recv().unwrap_or(Signal::Build) == Signal::Skip {
                    // Doesn't block.
                    return BuildResult::Squashed;
                }
            }
        }

        // If another build has started already, we don't need to build
        // ourselves (it must have arrived after this request; so we don't add
        // to the pending list). But we do need to wait for that build to
        // finish.
        if self.running.swap(true, Ordering::SeqCst) {
            let mut wait = 100;
            while self.running.load(Ordering::SeqCst) && wait < 50000 {
                // println!("loop of death");
                thread::sleep(Duration::from_millis(wait));
                wait *= 2;
            }
            return BuildResult::Squashed;
        }

        let result = self.build();
        self.running.store(false, Ordering::SeqCst);

        // If there is a pending build, run it now.
        let mut pending = self.pending.lock().unwrap();
        let pending = mem::replace(&mut *pending, vec![]);
        if !pending.is_empty() {
            // Kick off one build, then skip the rest.
            let mut pending = pending.iter();
            while let Some(next) = pending.next() {
                if next.send(Signal::Build).is_ok() {
                    break;
                }
            }
            for t in pending {
                let _ = t.send(Signal::Skip);
            }
        }

        result
    }

    // Cancels all pending builds without running any of them.
    fn cancel_pending(&self) {
        let mut pending = self.pending.lock().unwrap();
        let pending = mem::replace(&mut *pending, vec![]);
        for t in pending {
            let _ = t.send(Signal::Skip);
        }
    }

    // Build the project.
    fn build(&self) -> BuildResult {
        let build_dir = &self.build_dir.lock().unwrap();
        let build_dir = build_dir.as_ref().unwrap();
        let tex_path = &self.tex_path.lock().unwrap();
        let tex_path = tex_path.as_ref().unwrap();

        let (result_tx, result_rx) = channel();
        {
            let request_chan = self.request_chan.lock().unwrap();
            request_chan.send(BuildContext {
                directory: build_dir.clone(),
                file: tex_path.clone(),
                result_chan: result_tx,
            }).unwrap();
        }
        result_rx.recv().unwrap()
    }
}
