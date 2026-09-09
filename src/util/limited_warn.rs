/// Like `eprintln!` but is turned off after 100 outputs
// Wasteful amount of code, todo: reduce code generation
#[macro_export]
macro_rules! limited_eprintln {
    { $($arg:tt)* } => {
        {
            use std::{io::{stderr, Write}, sync::atomic::{AtomicU32, Ordering}};

            const MAX_WARNS: u32 = 100;
            static NUM_WARNS: AtomicU32 = AtomicU32::new(0);
            let n = NUM_WARNS.load(Ordering::Relaxed);
            if n <= MAX_WARNS {
                let mut out = stderr().lock();
                _ = write!(out, $($arg)*);
                if n == MAX_WARNS {
                    _ = out.write_all(b"; too many of these warnings, turning them off now");
                }
                _ = out.write_all(b"\n");
                NUM_WARNS.fetch_add(1, Ordering::Relaxed);
            }
        }
    }
}
