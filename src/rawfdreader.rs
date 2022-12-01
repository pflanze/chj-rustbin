// copy from https://stackoverflow.com/questions/55812291/bufreader-from-a-raw-fd

use libc;
use std::io::{Error, Read, Result};
use std::os::unix::io::{FromRawFd, RawFd};

pub struct RawFdReader {
    fd: RawFd,
}

impl FromRawFd for RawFdReader {
    unsafe fn from_raw_fd(fd: RawFd) -> Self {
        Self { fd }
    }   
}

impl Read for RawFdReader {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        assert!(buf.len() <= isize::max_value() as usize);
        match unsafe { libc::read(self.fd, buf.as_mut_ptr() as _, buf.len()) } { 
            x if x < 0 => Err(Error::last_os_error()),
            x => Ok(x as usize),
        }
    }   
}

