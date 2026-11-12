use std::process::exit;

/// Bash on Linux adds the signal number for SIGPIPE, 13, to 128 (the minimum
/// exit code to report signal exits according to POSIX?). Let's do the same.
pub const DEFAULT_EXIT_CODE: i32 = 128 + 13;

pub trait BrokenPipeToExit<T, E> {
    /// If `E` is indicating broken pipe (on writing to a filehandle on Unix),
    /// exit with the given exit code, or the default exit code (141) if not
    /// given.
    ///
    /// This does not clean up, i.e. not call any drop implementations.
    fn broken_pipe_to_exit(self, exit_code: Option<i32>) -> Result<T, E>;
}

impl<T> BrokenPipeToExit<T, std::io::Error> for Result<T, std::io::Error> {
    fn broken_pipe_to_exit(
        self,
        exit_code: Option<i32>,
    ) -> Result<T, std::io::Error> {
        match self {
            Ok(v) => Ok(v),
            Err(e) => match e.kind() {
                std::io::ErrorKind::BrokenPipe => {
                    exit(exit_code.unwrap_or(DEFAULT_EXIT_CODE))
                }
                _ => Err(e),
            },
        }
    }
}
