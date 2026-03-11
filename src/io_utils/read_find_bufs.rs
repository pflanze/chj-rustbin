use std::{fs::ReadDir, mem::swap, os::unix::ffi::OsStrExt, path::PathBuf};

use anyhow::{anyhow, Context, Result};

use crate::io_utils::{
    read_buf::ReadBufStreamError,
    read_dir_bufs::{MAX_EXPECTED_PATH_LENGTH, RECORD_SEPARATOR},
};

pub struct FindBufStream {
    buf_size: usize,
    errors: Vec<ReadBufStreamError>,
    dir: PathBuf,
    pending_dirs: Vec<PathBuf>,
    input: ReadDir,
    buf: Vec<u8>,
}

impl FindBufStream {
    pub fn new(
        buf_size: usize,
        dir: PathBuf,
        include_top: bool,
    ) -> Result<Self> {
        let errors = Vec::new();
        let pending_dirs = Vec::new();
        let mut buf = Vec::with_capacity(buf_size + MAX_EXPECTED_PATH_LENGTH);
        let input = std::fs::read_dir(&dir)
            .with_context(|| anyhow!("directory {dir:?}"))?;
        if include_top {
            buf.extend_from_slice(dir.as_os_str().as_bytes());
            buf.push(RECORD_SEPARATOR);
        }
        Ok(Self {
            buf_size,
            errors,
            dir,
            pending_dirs,
            input,
            buf,
        })
    }
}

impl Iterator for FindBufStream {
    type Item = Result<Vec<u8>, ReadBufStreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let Self {
            buf_size,
            ref mut errors,
            ref mut dir,
            ref mut pending_dirs,
            ref mut input,
            ref mut buf,
        } = self;
        (|| -> Result<Option<Vec<u8>>, ReadBufStreamError> {
            'outer: while buf.len() < *buf_size {
                if let Some(item) = input.next() {
                    let item = item?;
                    let path = item.path();
                    buf.extend_from_slice(path.as_os_str().as_bytes());
                    buf.push(RECORD_SEPARATOR);
                    let ft = item.file_type()?;
                    if ft.is_dir() {
                        pending_dirs.push(path);
                    }
                } else {
                    loop {
                        if let Some(new_dir) = pending_dirs.pop() {
                            *dir = new_dir;
                            match std::fs::read_dir(&dir) {
                                Ok(d) => {
                                    *input = d;
                                    break;
                                }
                                Err(e) => match e.kind() {
                                    std::io::ErrorKind::NotFound => (),
                                    _ => {
                                        errors.push(
                                            anyhow!("directory {dir:?}: {e}")
                                                .into(),
                                        );
                                    }
                                },
                            }
                        } else {
                            break 'outer;
                        }
                    }
                }
            }
            if buf.is_empty() {
                Ok(None)
            } else {
                let mut buf2 =
                    Vec::with_capacity(*buf_size + MAX_EXPECTED_PATH_LENGTH);
                swap(&mut buf2, buf);
                Ok(Some(buf2))
            }
        })()
        .transpose()
    }
}
