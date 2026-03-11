use std::{
    mem::swap,
    os::unix::ffi::OsStrExt,
    path::PathBuf,
    sync::{
        mpsc::{channel, Receiver, Sender},
        Arc, Mutex,
    },
};

use anyhow::{anyhow, Context, Result};

use crate::io_utils::{
    read_buf::ReadBufStreamError,
    read_dir_bufs::{MAX_EXPECTED_PATH_LENGTH, RECORD_SEPARATOR},
};

pub struct FindBufStream {
    recv: Receiver<Vec<u8>>,
    // Half-finished buffers, and errors
    spares_and_errors: Arc<Mutex<(Vec<Vec<u8>>, Vec<ReadBufStreamError>)>>,
    // After recv is done, move `spares` here: `spares_end:
    // Option<Vec<Vec<u8>>>`--no, race condition between Receiver and
    // Arc would require retrying, which will probably waste the
    // benefits.
}

struct TaskContext {
    buf_size: usize,
    send: Sender<Vec<u8>>,
    spares_and_errors: Arc<Mutex<(Vec<Vec<u8>>, Vec<ReadBufStreamError>)>>,
    path: PathBuf,
    buf: Vec<u8>,
}

impl TaskContext {
    fn new(
        buf_size: usize,
        send: Sender<Vec<u8>>,
        spares_and_errors: Arc<Mutex<(Vec<Vec<u8>>, Vec<ReadBufStreamError>)>>,
        path: PathBuf,
    ) -> Self {
        let buf = Vec::with_capacity(buf_size + MAX_EXPECTED_PATH_LENGTH);
        TaskContext {
            buf_size,
            send,
            spares_and_errors,
            path,
            buf,
        }
    }

    fn spawn(self, include_cwd: bool) {
        rayon::spawn({
            move || {
                let TaskContext {
                    buf_size,
                    send,
                    spares_and_errors: spares,
                    path,
                    mut buf,
                } = self;
                if include_cwd {
                    buf.extend_from_slice(path.as_os_str().as_bytes());
                    buf.push(RECORD_SEPARATOR);
                }
                match (|| -> Result<()> {
                    match std::fs::read_dir(&path) {
                        Ok(input) => {
                            for item in input {
                                let item = item?;
                                let path = item.path();
                                let ft = item.file_type()?;
                                // Include `path` in the child task;
                                // this way we can include the
                                // top-level directory
                                // (conditionally), too.
                                if ft.is_dir() {
                                    TaskContext::new(
                                        buf_size,
                                        send.clone(),
                                        spares.clone(),
                                        path,
                                    )
                                    .spawn(include_cwd);
                                } else {
                                    buf.extend_from_slice(
                                        path.as_os_str().as_bytes(),
                                    );
                                    buf.push(RECORD_SEPARATOR);
                                }
                                if buf.len() >= buf_size {
                                    let mut new_buf = {
                                        let mut lock =
                                            spares.lock().expect("no panics");
                                        if let Some(vec) = lock.0.pop() {
                                            vec
                                        } else {
                                            drop(lock);
                                            Vec::with_capacity(
                                                buf_size
                                                    + MAX_EXPECTED_PATH_LENGTH,
                                            )
                                        }
                                    };
                                    std::mem::swap(&mut buf, &mut new_buf);
                                    send.send(new_buf)?;
                                }
                            }
                            // Not caring about proper buffer capacity
                            // here (it will be dropped right after),
                            // just getting it out without move to
                            // allow for the `.push(buf)` outside to
                            // work.
                            let mut new_buf = Vec::new();
                            swap(&mut new_buf, &mut buf);
                            let usage_percentage = (buf.len() * 100) / buf_size;
                            if usage_percentage < 66 {
                                let mut lock =
                                    spares.lock().expect("no panics");
                                lock.0.push(new_buf);
                            } else {
                                send.send(new_buf)?;
                            }
                        }
                        Err(e) => match e.kind() {
                            std::io::ErrorKind::NotFound => (),
                            _ => {
                                Err(e).with_context(|| {
                                    anyhow!("directory {path:?}")
                                })?;
                            }
                        },
                    }
                    Ok(())
                })() {
                    Ok(()) => (),
                    Err(e) => {
                        let mut lock = spares.lock().expect("no panics");
                        // buf: check percentage stuff as above? Sigh,
                        // just rely on recycling, OK? It will
                        // normally just be the directory entry
                        // itself, and hence definitely warrant reuse.
                        lock.0.push(buf);
                        lock.1.push(e.into());
                    }
                }
            }
        });
    }
}

impl Iterator for FindBufStream {
    type Item = Result<Vec<u8>, ReadBufStreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.recv.recv() {
            Ok(v) => Some(Ok(v)),
            // Should we set a flag on Err to yield None forever
            // afterwards?
            Err(_) => {
                let mut lock =
                    self.spares_and_errors.lock().expect("no panics");
                // First return the spare buffers, then the errors
                lock.0
                    .pop()
                    .map(|v| Ok(v))
                    .or_else(|| lock.1.pop().map(|v| Err(v)))
            }
        }
    }
}

impl FindBufStream {
    pub fn new(buf_size: usize, dir: PathBuf) -> Self {
        // Do not use a sync_channel since that could block the rayon
        // threads (all of them!)
        let (send, recv) = channel();
        let spares = Arc::new(Mutex::new((Vec::new(), Vec::new())));
        TaskContext::new(buf_size, send, spares.clone(), dir).spawn(true);
        Self {
            recv,
            spares_and_errors: spares,
        }
    }
}
