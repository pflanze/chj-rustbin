//! Almost-copy-paste of `read_buf.rs`

use std::{fs::ReadDir, os::unix::ffi::OsStrExt};

use rayon::iter::{plumbing::Folder, ParallelIterator};

use crate::io_utils::read_buf::ReadBufStreamError;

/// If a path is longer than this it could trigger a reallocation of a
/// buffer (i.e. no big deal)
pub const MAX_EXPECTED_PATH_LENGTH: usize = 1000;

// Always, since this guarantees separation as paths cannot contain
// this byte.
pub const RECORD_SEPARATOR: u8 = 0;

pub struct ReadDirBufStream {
    input: ReadDir,
    buf_size: usize,
    buf: Vec<u8>,
}

impl ReadDirBufStream {
    /// Unlike for `ReadBufStream` where `buf_size` is the maximum
    /// buffer size, here it is the minimal: paths are added to a
    /// buffer until it is at least the given size.
    pub fn new(input: ReadDir, buf_size: usize) -> Self {
        Self {
            input,
            buf_size,
            buf: Vec::with_capacity(buf_size + MAX_EXPECTED_PATH_LENGTH),
        }
    }
}

impl Iterator for ReadDirBufStream {
    /// Use `ReadBufStreamError` to stay compatible with `ParReadBufStream`
    type Item = Result<Vec<u8>, ReadBufStreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        (|| -> Result<Option<Vec<u8>>, ReadBufStreamError> {
            while self.buf.len() < self.buf_size {
                if let Some(item) = self.input.next().transpose()? {
                    // XX parameterize whether file_name or path
                    let filename = item.file_name();
                    self.buf.extend_from_slice(filename.as_bytes());
                    self.buf.push(RECORD_SEPARATOR);
                } else {
                    break;
                }
            }

            if self.buf.is_empty() {
                Ok(None)
            } else {
                let mut new_buf = Vec::with_capacity(
                    self.buf_size + MAX_EXPECTED_PATH_LENGTH,
                );
                std::mem::swap(&mut self.buf, &mut new_buf);
                Ok(Some(new_buf))
            }
        })()
        .transpose()
    }
}

impl ParallelIterator for ReadDirBufStream {
    type Item = Result<Vec<u8>, ReadBufStreamError>;

    fn drive_unindexed<C>(mut self, consumer: C) -> C::Result
    where
        C: rayon::iter::plumbing::UnindexedConsumer<Self::Item>,
    {
        let folder = consumer.into_folder();
        // XX surely evil, held across whole duration, or what?
        // Disabling concurrency?
        folder.consume_iter(&mut self).complete()
    }
}
