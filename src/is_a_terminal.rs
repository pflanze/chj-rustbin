use libc::{c_int, isatty};

pub fn is_a_terminal(fd: c_int) -> bool {
    (unsafe { isatty(fd) }) > 0
}
