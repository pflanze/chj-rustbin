use std::{
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    thread::{self, JoinHandle},
};

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

pub struct Stopper(Arc<AtomicBool>);

impl Stopper {
    pub fn new() -> Self {
        Self(Arc::new(AtomicBool::new(false)))
    }
    pub fn reader(&self) -> StopperReader {
        StopperReader(self.0.clone())
    }
}

pub struct StopperReader(Arc<AtomicBool>);

impl StopperReader {
    pub fn get(&self) -> bool {
        self.0.load(Ordering::SeqCst)
    }
}

impl Drop for Stopper {
    fn drop(&mut self) {
        self.0.store(true, Ordering::SeqCst);
    }
}
