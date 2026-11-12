use std::thread::{self, JoinHandle};

use anyhow::anyhow;

pub fn spawnt<F, T>(name: &str, f: F) -> std::io::Result<JoinHandle<T>>
where
    F: FnOnce() -> T,
    F: Send + 'static,
    T: Send + 'static,
{
    thread::Builder::new().name(name.into()).spawn(f)
}

pub trait Joint<T> {
    fn joint(self) -> anyhow::Result<T>;
}

impl<T: Send + 'static> Joint<T> for JoinHandle<T> {
    fn joint(self) -> anyhow::Result<T> {
        self.join().map_err(|e| -> anyhow::Error {
            let r: Result<Box<String>, _> = e.downcast();
            let e = match r {
                Ok(v) => return anyhow!("thread panicked with String: {v:#}"),
                Err(e) => e,
            };
            let r: Result<Box<anyhow::Error>, _> = e.downcast();
            let e = match r {
                Ok(v) => {
                    return anyhow!("thread panicked with anyhow::Error: {v:#}")
                }
                Err(e) => e,
            };
            anyhow!("thread panicked with: {e:#?}")
        })
    }
}
