use std::io::Read;

/// Read a buffer of `BUF_SIZE` (the resulting Vec can be smaller);
/// `left_over` is copied into the buffer at the start, then the
/// remainder is attempted to be filled from `input`. Returns None if
/// eof was reached (nothing could be read) and `left_over` is empty.
/// Panics if `left_over.len() >= `BUF_SIZE`.
pub fn read_buf<const BUF_SIZE: usize>(
    left_over: &[u8],
    input: &mut impl Read,
) -> std::io::Result<Option<Vec<u8>>> {
    if left_over.len() >= BUF_SIZE {
        panic!("`left_over_` must be shorter than `BUF_SIZE`")
    }
    let mut buf = Vec::with_capacity(BUF_SIZE);
    buf.extend_from_slice(left_over);
    unsafe {
        // Safe because we're truncating back to the number of bytes
        // overwritten.
        buf.set_len(BUF_SIZE)
    }
    let n = input.read(&mut buf[left_over.len()..])?;
    if n == 0 {
        Ok(None)
    } else {
        let num_bytes_overwritten = left_over.len() + n;
        assert!(num_bytes_overwritten <= BUF_SIZE); // but set_len checks that anyway
        unsafe {
            // Safe because this is the range of bytes that were
            // overwritten, hence not undefined data
            buf.set_len(num_bytes_overwritten);
        }
        Ok(Some(buf))
    }
}
