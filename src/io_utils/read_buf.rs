use std::{io::Read, sync::Mutex};

#[derive(thiserror::Error, Debug)]
pub enum ReadBufStreamError {
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),
    #[error("no record separator found within the buffer size of {0} bytes")]
    RecordTooLarge(usize),
}

/// Fill a buffer vector which should have its capacity set to some
/// desired buffer size (the resulting Vec can be smaller); data it
/// already contains is retained (i.e. if len() is not zero, new data
/// is appended to it, up to the vector's capacity). Returns the
/// number of bytes in `buf` after the operation, which will be 0 if
/// eof was reached (nothing could be read) and the given buffer was
/// empty. Panics if `buf`'s capacity is zero.
pub fn read_buf(
    buf: &mut Vec<u8>,
    input: &mut impl Read,
) -> Result<usize, ReadBufStreamError> {
    let left_over_len = buf.len();
    assert!(buf.capacity() > 0);
    let buf_size = buf.capacity();

    // Extend data area for `read` to fill.
    unsafe {
        // Safe because it's the capacity, and we'll be truncating
        // back to the number of bytes overwritten.
        buf.set_len(buf_size)
    }
    let write_area = &mut buf[left_over_len..];
    if write_area.is_empty() {
        Err(ReadBufStreamError::RecordTooLarge(buf_size))
    } else {
        let n = input.read(write_area)?;
        let ntotal = left_over_len + n;
        if n > 0 {
            assert!(ntotal <= buf_size); // but set_len checks that anyway
            unsafe {
                // Safe because this is the range of bytes that were
                // overwritten, hence not undefined data
                buf.set_len(ntotal);
            }
        }
        Ok(ntotal)
    }
}

/// Read a byte stream as a stream of buffers, split on a given byte
/// boundary
pub struct ReadBufStream<S: Read> {
    input: S,
    buf_size: usize,
    record_separator: u8,
    left_over: Vec<u8>,
}

impl<S: Read> ReadBufStream<S> {
    pub fn new(input: S, buf_size: usize, record_separator: u8) -> Self {
        Self {
            input,
            buf_size,
            record_separator,
            left_over: Vec::with_capacity(buf_size),
        }
    }
}

impl<S: Read> Iterator for ReadBufStream<S> {
    type Item = Result<Vec<u8>, ReadBufStreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        (|| -> Result<Option<Vec<u8>>, ReadBufStreamError> {
            let nbytes = read_buf(&mut self.left_over, &mut self.input)?;
            if nbytes == 0 {
                return Ok(None);
            }
            let (ri, _b) = self
                .left_over
                .iter()
                .rev()
                .enumerate()
                .find(|(_i, b)| **b == self.record_separator)
                .ok_or_else(|| {
                    ReadBufStreamError::RecordTooLarge(self.buf_size)
                })?;
            let i = self.left_over.len() - ri;
            let mut new_buf = Vec::with_capacity(self.buf_size);
            new_buf.extend_from_slice(&self.left_over[i..]);
            std::mem::swap(&mut self.left_over, &mut new_buf);
            new_buf.truncate(i);
            Ok(Some(new_buf))
        })()
        .transpose()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_() {
        let input = &[1, 2, 3, 0, 4, 5, 0, 6, 7, 0, 8, 0, 9, 11, 13, 0];
        let mut rbs = ReadBufStream::new(input.as_slice(), 7, 0);
        let b0 = (&mut rbs).next().unwrap().unwrap();
        assert_eq!(b0, [1, 2, 3, 0, 4, 5, 0]);
        let b1 = (&mut rbs).next().unwrap().unwrap();
        assert_eq!(b1, [6, 7, 0, 8, 0]);
        let b2 = (&mut rbs).next().unwrap().unwrap();
        assert_eq!(b2, [9, 11, 13, 0]);
        assert!((&mut rbs).next().is_none());
    }
}

pub struct ParReadBufStream<S: Read>(Mutex<ReadBufStream<S>>);

impl<S: Read> From<ReadBufStream<S>> for ParReadBufStream<S> {
    fn from(value: ReadBufStream<S>) -> Self {
        Self(Mutex::new(value))
    }
}

// XX why is this necessary, shouldn't Mutex shield S automatically?
unsafe impl<S: Read> Send for ParReadBufStream<S> {}

impl<S: Read> Iterator for ParReadBufStream<S> {
    type Item = Result<Vec<u8>, ReadBufStreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut lock = self.0.lock().expect("no panics");
        (&mut *lock).next()
    }
}

// impl<S: Read> ParallelIterator for ParReadBufStream<S> {
//     type Item = Result<Vec<u8>, ReadBufStreamError>;
//     fn drive_unindexed<C>(self, consumer: C) -> C::Result
//     where
//         C: rayon::iter::plumbing::UnindexedConsumer<Self::Item>,
//     {
//         let folder = consumer.into_folder();
//         // XX surely evil, held across whole duration, or what?
//         // Disabling concurrency? -- yes, it's bad
//         let mut lock = self.0.lock().expect("no panics");
//         folder.consume_iter(&mut *lock).complete()
//     }
// }
