use std::{fs::File, os::fd::FromRawFd};

use nix::unistd::pipe;

/// Return a pair of filehandles (read, write) that make up a unix
/// pipe
pub fn file_pipe() -> std::io::Result<(File, File)> {
    let (r, w) = pipe()?;
    unsafe { Ok((File::from_raw_fd(r), File::from_raw_fd(w))) }
}
