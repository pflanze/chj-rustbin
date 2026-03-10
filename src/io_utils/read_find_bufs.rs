use std::{
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
    recv: Receiver<Result<Vec<u8>, ReadBufStreamError>>,
    // Half-finished buffers
    spares: Arc<Mutex<Vec<Vec<u8>>>>,
    // After recv is done, move `spares` here: `spares_end:
    // Option<Vec<Vec<u8>>>`--no, race condition between Receiver and
    // Arc would require retrying, which will probably waste the
    // benefits.
}

struct TaskContext {
    buf_size: usize,
    send: Sender<Result<Vec<u8>, ReadBufStreamError>>,
    spares: Arc<Mutex<Vec<Vec<u8>>>>,
    path: PathBuf,
    buf: Vec<u8>,
}

impl TaskContext {
    fn new(
        buf_size: usize,
        send: Sender<Result<Vec<u8>, ReadBufStreamError>>,
        spares: Arc<Mutex<Vec<Vec<u8>>>>,
        path: PathBuf,
    ) -> Self {
        let buf = Vec::with_capacity(buf_size + MAX_EXPECTED_PATH_LENGTH);
        TaskContext {
            buf_size,
            send,
            spares,
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
                    spares,
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
                                        if let Some(vec) = lock.pop() {
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
                                    send.send(Ok(new_buf))?;
                                }
                            }
                            let usage_percentage = (buf.len() * 100) / buf_size;
                            if usage_percentage < 66 {
                                let mut lock =
                                    spares.lock().expect("no panics");
                                lock.push(buf);
                            } else {
                                send.send(Ok(buf))?;
                            }
                        }
                        Err(e) => match e.kind() {
                            std::io::ErrorKind::NotFound => (),
                            _ => {
                                Err(e).with_context(|| {
                                    anyhow!("opening directory {path:?}")
                                })?;
                            }
                        },
                    }
                    Ok(())
                })() {
                    Ok(()) => (),
                    Err(e) => {
                        _ = send.send(Err(e.into()));
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
            Ok(v) => Some(v),
            // Should we set a flag on Err to yield None forever
            // afterwards?
            Err(_) => {
                let mut lock = self.spares.lock().expect("no panics");
                lock.pop().map(|v| Ok(v))
            }
        }
    }
}

impl FindBufStream {
    pub fn new(buf_size: usize, dir: PathBuf) -> Self {
        // Do not use a sync_channel since that could block the rayon
        // threads (all of them!)
        let (send, recv) = channel();
        let spares = Arc::new(Mutex::new(Vec::new()));
        TaskContext::new(buf_size, send, spares.clone(), dir).spawn(true);
        Self { recv, spares }
    }
}
