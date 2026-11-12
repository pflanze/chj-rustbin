//! This is a Unix (and probably Linux) only program to verify that
//! `chj_rustbin::io::write_all_vectored` works properly.

use std::{
    ffi::{c_int, c_void},
    io::IoSlice,
    os::fd::AsRawFd,
    thread::sleep,
    time::Duration,
};

use anyhow::{bail, Result};
use chj_rustbin::{
    io::{
        close::Close,
        unix::file_pipe::file_pipe,
        write_all_vectored::{_write_all_vectored, io_slices_tot_size},
    },
    util::thread::{spawnt, Joint, Stopper},
};
use libc::kill;
use nix::{
    sys::signal::{sigaction, SaFlags, SigAction, SigHandler, SigSet, Signal},
    unistd::{getpid, write},
};
use rand::{thread_rng, Rng};

fn garbage(start: &str, len: usize) -> Vec<u8> {
    let mut vec = Vec::from(start.as_bytes());
    vec.resize(len, 1);
    vec
}

extern "C" fn my_handler(_sig: c_int) {
    write(2, b"Got signal\n").expect(
        "assume the user doesn't close stderr or \
         redirect to a place that errors",
    );
}

fn main() -> Result<()> {
    let (r, mut w) = file_pipe()?;

    let sig = Signal::SIGHUP;
    let pid = getpid();

    let act = SigAction::new(
        SigHandler::Handler(my_handler),
        SaFlags::empty(),
        SigSet::empty(),
    );
    unsafe {
        sigaction(sig, &act)?;
    }

    let signaller_stop = Stopper::new();

    let signaller_stop_reader = signaller_stop.reader();
    let _signaller = spawnt("signaller", move || {
        let mut rng = thread_rng();
        while !signaller_stop_reader.get() {
            sleep(Duration::from_micros(rng.gen_range(5000..20000)));
            eprintln!("killing");
            unsafe {
                kill(pid.as_raw(), sig as i32);
            };
        }
    });

    let reader = spawnt("reader", move || -> Result<Vec<u8>> {
        let mut collected = Vec::new();
        let mut buf = [0; 500];
        loop {
            // What about EINTR *here*? Automatically handles it.
            // let n = r.read(&mut buf)?;

            // But oddly, this also never experiences EINTR, why?:
            let n = {
                let r: isize = unsafe {
                    let ptr = buf.as_mut_ptr() as *mut c_void;
                    libc::read(r.as_raw_fd(), ptr, buf.len())
                };
                if r < 0 {
                    bail!("error from libc::read");
                }
                r as usize
            };
            if n == 0 {
                break;
            }
            collected.extend_from_slice(&buf[0..n]);
        }
        Ok(collected)
    })?;

    let mut rng = thread_rng();
    let data: Vec<Vec<u8>> = (0..1000)
        .map(|i| garbage(&i.to_string(), rng.gen_range(0..1500000)))
        .collect();

    let original_collected: Vec<u8> =
        data.iter().flat_map(|b| b.iter().copied()).collect();
    let original_len: usize = data.iter().map(|sl| sl.len()).sum();

    let mut sls: Vec<_> = data.iter().map(|s| IoSlice::new(s)).collect();

    let tot = io_slices_tot_size(&sls);
    assert_eq!(tot, original_len);
    _write_all_vectored::<false, true>(&mut sls, tot, &mut w)?;

    w.close()?;

    let reader_collected = reader.joint()??;
    assert_eq!(original_collected.len(), reader_collected.len());
    assert!(original_collected == reader_collected);

    Ok(())
}
