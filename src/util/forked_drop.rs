use std::{mem::ManuallyDrop, process::exit};

use anyhow::{bail, Result};
use derive_more::Deref;
use nix::{
    errno::Errno,
    unistd::{close, fork, pipe, setsid, write, ForkResult},
};
use tempfile::TempDir;

/// The following conditions must be upheld for safety:
///
/// - T::drop must be able to run in a forked process, i.e. with all
///   other threads stopped and the potential for deadlock (probably
///   including memory allocation)
///
/// - T::drop must not panic -- aha, will abort in that case, never
///   caught? Then it's OK. Custom panic handler? Must satisfy the
///   same conditions.
///
#[allow(drop_bounds)]
pub unsafe trait ForkedDrop: Drop {}

/// Owns `T` in a forked child process, and drops it there once the
/// parent copy of itself drops / disappears
///
/// The disappearance of the parent is detected by the closing of an
/// embedded unix pipe.
///
/// Deref / DerefMut allows to still access the value by reference,
/// but drop is not called in the parent (meaning, a memory leak will
/// occur if T has associated heap memory).
///
/// Note that `ForkedDropper::new` only forks once, which means that
/// if this object is dropped and the child exits, the parent will get
/// a SIGCHILD signal (this is usually harmless, but it does lead to
/// EINTR on system calls and a possibly unexpected child collect in
/// `wait`).
#[derive(Deref)]
pub struct ForkedDropper<T: ForkedDrop + 'static> {
    #[deref(forward)]
    value: ManuallyDrop<T>,
    pipe_write: i32,
}

impl<T: ForkedDrop> Drop for ForkedDropper<T> {
    fn drop(&mut self) {
        _ = close(self.pipe_write);
    }
}

/// Write directly to stderr, avoiding to take any locks or make
/// allocations. Ignores errors.
fn werr(s: &str) {
    _ = write(2, s.as_bytes());
}

impl<T: ForkedDrop> ForkedDropper<T> {
    pub fn new(value: T) -> Result<Self> {
        let (pipe_read, pipe_write) = pipe()?;

        match unsafe {
            // Safe because we're not going to allocate any memory or do anything
            // involving locks in the child (XX what about other things?, check POSIX),
            // and ForkedDrop guarantees the same for T::drop.
            fork()
        } {
            Ok(ForkResult::Parent { child: _ }) => (),
            Ok(ForkResult::Child) => {
                // Do not use anything that allocates, e.g. anyhow or stderr()
                match close(pipe_write) {
                    Ok(()) => (),
                    Err(e) => {
                        werr("ForkedDropper child: close(pipe_write): ");
                        werr(e.desc());
                        werr("\n");
                    }
                }
                match setsid() {
                    Ok(_pid) => (),
                    Err(e) => {
                        werr("ForkedDropper child: setsid(): ");
                        werr(e.desc());
                        werr("\n");
                    }
                }
                let mut buf = [0; 1];
                while let Err(e) = nix::unistd::read(pipe_read, &mut buf) {
                    if e != Errno::EINTR {
                        break;
                    }
                }
                // werr("time to drop!\n");
                drop(value);
                exit(0);
            }
            Err(e) => bail!("failure to fork: {e}"),
        }

        close(pipe_read)?;
        let value = ManuallyDrop::new(value);

        Ok(Self { value, pipe_write })
    }
}

// XX haven't investigated in detail whether that is actually safe! It
// does work so far, though.
unsafe impl ForkedDrop for TempDir {}
