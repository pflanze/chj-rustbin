use anyhow::{Context, Result, anyhow, bail}; 
use std::{env, writeln};
use std::io::{stdin, stderr, Write, BufReader, BufRead};
use libc::{_exit, srand};
use nix::unistd::{getpid, pipe, fork, ForkResult,
                  close, setsid, dup2, execvp, read, write, getuid };
use nix::time::{clock_gettime, ClockId};
use nix::sys::time::time_t;
use nix::fcntl::{open, OFlag};
use nix::sys::stat::{mode_t, Mode};
use nix::sys::wait::{wait, waitpid, WaitStatus};
use std::os::unix::io::{FromRawFd, RawFd};
use std::ffi::{CString, OsString, OsStr};
use std::os::unix::ffi::{OsStringExt};
use nix::sys::signal::Signal;
use bstr_parse::{BStrParse, ParseIntError, FromBStr};
use nix::errno::Errno;
use thiserror::Error;
use nix::unistd::Pid;
use std::collections::HashMap;

use alsa::card::{Iter, Card};
use alsa::hctl::HCtl;
use alsa::rawmidi::Rawmidi;
use alsa::poll::Descriptors as PollDescriptors;
use alsa::mixer::Mixer;
use alsa::seq::Seq;

use alsa::{Direction, ValueOr};
use alsa::pcm::{PCM, HwParams, Format, Access, State};
use rand;

use std::f64::consts::PI;

fn set_start_threshold(pcm: PCM) -> Result<()> {
    // Make sure we don't start the stream too early
    let hwp = pcm.hw_params_current()?;
    let swp = pcm.sw_params_current()?;
    swp.set_start_threshold(hwp.get_buffer_size()?)?;
    pcm.sw_params(&swp)?;
    Ok(())
}


pub fn main() -> Result<()> {
    for i in Iter::new() {
        let i = i?;
        println!("Getting i={:?}", i);
    }
    let inp = (|| -> Result<_, anyhow::Error> {
        let inp = PCM::new("default", alsa::Direction::Capture, false)?;
        {
            let p = HwParams::any(&inp)?;
            p.set_channels(1)?;
            p.set_rate(48000, ValueOr::Nearest)?;
            p.set_format(Format::s16())?;
            p.set_access(Access::RWInterleaved)?; //?
            inp.hw_params(&p)?;
        }
        Ok(inp)
    })().with_context(|| "preparing audio input")?;

    let outp = (|| -> Result<_, anyhow::Error> {
        let outp = PCM::new("plughw:CARD=Device,DEV=0", alsa::Direction::Playback, false)?;
        {
            let p = HwParams::any(&outp)?;
            p.set_channels(2)?; // 1 doesn't work!!
            p.set_rate(48000, ValueOr::Nearest)?;
            p.set_format(Format::s16())?;
            p.set_access(Access::RWInterleaved)?;
            outp.hw_params(&p)?;
        }
        Ok(outp)
    })().with_context(|| "preparing audio output")?;

    set_start_threshold(inp)?;
    
    let o = outp.io_i16()?;

    const SAMPLES_PER_BUF : usize = 256; // per channel
    let mut buf = [0i16; SAMPLES_PER_BUF * 2];
    let mut t : f64 = 0.0;
    let d = 2.0 * PI;

    // Play back for n seconds.
    let n = 600;
    for _ in 0..n*48000/SAMPLES_PER_BUF {

        for i in 0..SAMPLES_PER_BUF {
            // left
            buf[i*2 + 1] = (
                ((t / 200.000123 + (t / 301000.438).sin() * 400.).sin()
                 * (t / 200.0).sin() + rand::random::<f64>()
                ) * 4000.0
            ) as i16;

            // right
            buf[i*2] = (
                ((t / 201.000123 + (t / 300000.).sin() * 400.).sin()
                 * (t / 201.0).sin() + rand::random::<f64>()
                ) * 4000.0
            ) as i16;

            t = t + d;
        }

        assert_eq!(o.writei(&buf)?, SAMPLES_PER_BUF); // not values in buf because stereo
    }

    // In case the buffer was larger than 2 seconds, start the stream manually.
    if outp.state() != State::Running { outp.start()? };
    // Wait for the stream to finish playback.
    outp.drain()?;

    Ok(())
}
