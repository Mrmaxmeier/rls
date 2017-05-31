use std::ffi::{OsStr, OsString};
use std::fs::File;
use std::io::{self, Cursor, Read, Seek, SeekFrom, Write, BufReader};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use tectonic::io::{IoProvider, OpenResult, InputHandle, InputOrigin};
use tectonic::status::StatusBackend;
use vfs::{Vfs, FileContents};

pub struct VfsIoProvider {
    vfs: Arc<Vfs>,
    base: PathBuf,
}

impl VfsIoProvider {
    pub fn new(vfs: Arc<Vfs>) -> Self {
        VfsIoProvider {
            vfs: vfs,
            base: Path::new("").to_path_buf()
        }
    }

    pub fn set_base(&mut self, path: &Path) {
        self.base = path.to_path_buf();
    }
}

impl IoProvider for VfsIoProvider {
    fn input_open_name(&mut self, name: &OsStr, _status: &mut StatusBackend) -> OpenResult<InputHandle> {
        let path = if Path::new(name).is_relative() {
            self.base.join(Path::new(name))
        } else {
            Path::new(name).to_path_buf()
        };
        eprintln!("input_open_name({:?})", path);
        eprintln!("cached files: {:#?}", self.vfs.get_cached_files().keys().collect::<Vec<_>>());

        if self.vfs.file_is_synced(path.as_path()).is_err() {
            // The result of file_is_synced is not important, Error::FileNotCached is.
            // We don't want to bloat the VFS by caching files with load_file.
            return OpenResult::NotAvailable;
        }

        let contents = match self.vfs.load_file(path.as_path()) {
            Ok(contents) => match contents {
                FileContents::Text(string) => string.into_bytes(),
                FileContents::Binary(data) => data
            },
            Err(e) => return OpenResult::NotAvailable
        };

        let file = Cursor::new(contents);

        OpenResult::Ok(InputHandle::new(name, file, InputOrigin::Filesystem))
    }
}