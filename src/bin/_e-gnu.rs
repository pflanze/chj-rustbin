// Same functionality as _e-gnu from
// https://github.com/pflanze/chj-scripts

// #[macro_use]
// extern crate log;
#[path = "../rawfdreader.rs"]
mod rawfdreader;
use rawfdreader::RawFdReader;
use anyhow::{Result, anyhow, bail, Context}; 
use std::{env, writeln};
use std::io::{Write, BufReader, BufRead}; //Read, 
//use nix::NixPath;
use libc::_exit;
use nix::unistd::{getpid, pipe, fork, ForkResult,
                  close, setsid, dup2, execvp, read, write};
use nix::time::{clock_gettime, ClockId};
use nix::sys::time::time_t;
use nix::fcntl::{open, OFlag};
use nix::sys::stat::{mode_t, Mode};
use nix::sys::wait::{waitpid, WaitStatus};
use std::os::unix::io::{FromRawFd, RawFd};
use std::ffi::{CString, OsString}; //CStr, OsStr, 
use std::os::unix::ffi::{OsStringExt}; //OsStrExt, 
use nix::sys::signal::Signal;
//use std::clone::Clone;
//use std::io::Read;
use bstr_parse::*; //XX
use nix::errno::Errno;
use thiserror::Error;
use nix::unistd::Pid;


// There's no try_map, so:
fn cstrings_from_osstrings(osstrs: Vec<OsString>) -> Result<Vec<CString>> {
    let mut v : Vec<CString> = Vec::new();
    for s in osstrs {
        v.push(CString::new(s.into_vec())?);
    }
    Ok(v)
}

fn mode_from_bits(mode: mode_t) -> Result<Mode> {
    Mode::from_bits(mode).ok_or_else(
        || anyhow!("invalid mode: {}", mode))
}

fn write_all(out: RawFd, s: &[u8]) -> Result<()> {
    let mut lentotal // : size_t
        = 0;
    let end = s.len();
    while lentotal < end {
        let len = write(out, &s[lentotal..end])?;
        lentotal += len;
    }
    Ok(())
}

fn string_matches_start(s: &str, pat: &str) -> bool {
    pat.len() <= s.len() && &s[0..pat.len()] == pat
}

fn string_remove_start<'ts>(s: &'ts str, pat: &str) -> &'ts str {
    if string_matches_start(s, pat) {
        &s[pat.len()..s.len()]
    } else {
        s
    }
}

fn time() -> Result<time_t> {
    Ok(clock_gettime(ClockId::CLOCK_REALTIME)?.tv_sec())
}


// Really wait until the given process has ended, and return a
// simpler enum.
enum Status { Normalexit(i32), Signalexit(Signal) }

//fn easy_waitpid<P: Into<Option<Pid>>>(pid: P) -> Result<Status> {
fn easy_waitpid(pid: Pid) -> Result<Status> {
    loop {
        let st = waitpid(pid, None)?;
        match st {
            WaitStatus::Exited(_pid, exitcode)
                => return Ok(Status::Normalexit(exitcode)),
            WaitStatus::Signaled(_pid, signal, _bool)
                => return Ok(Status::Signalexit(signal)),
            _ => {} // retry
        }
    }
}

// Treat non-exit(0) cases as errors.
fn xeasy_waitpid(pid: Pid) -> Result<()> {
    match easy_waitpid(pid)? {
        Status::Normalexit(0) =>
            Ok(()),
        Status::Normalexit(exitcode) =>
            bail!("process exited with error code {}", exitcode),
        Status::Signalexit(signal) =>
            bail!("process exited via signal {}", signal)
    }
}

// Don't make it overly complicated, please. The original API is
// simple enough. If a Pid is given, it's the parent.
//
// Swallow the unsafe as long as this is in the same file: it should
// be safe even with allocation in the child as:
//  - we should not be using threading in this program (libs, though?)
//  - isn't libc's malloc safe anyway with fork?
//  - and we're not (consciously) touching any other mutexes in the children.
//
fn easy_fork() -> Result<Option<Pid>> {
    match unsafe { fork()? } {
        ForkResult::Parent { child, .. } => Ok(Some(child)),
        ForkResult::Child => Ok(None)
    }
}

#[derive(Error, Debug)]
enum Slurp256ParseError {
    #[error("I/O error: {0}")]
    Io(Errno),
    #[error("input is too large")]
    InputTooLarge,
    #[error("parse error: {0}")]
    Pie(ParseIntError)
}

fn slurp256_parse<T: FromBStr<Err = Slurp256ParseError>>
    (fd: RawFd) -> Result<T, Slurp256ParseError> {
    let mut buf : [u8; 257] = [0; 257];
    let len = read(fd, &mut buf).map_err(Slurp256ParseError::Io)?;
    if len == 257 {
        Err(Slurp256ParseError::InputTooLarge)
    } else {
        buf[0..len].parse()
    }
}

fn main() -> Result<()> {

    let alternate_editor = env::var_os("EMACS_ALTERNATE_EDITOR")
        .unwrap_or(OsString::from(""));
    // println!("alternate_editor={:?}", alternate_editor);

    let cmd = {
        let mut c = OsString::from("--alternate-editor=");
        c.push(alternate_editor);
        let mut argv = vec!(
            CString::new("emacsclient")?,
            CString::new("-c")?,
            CString::new(c.into_vec())?);
        argv.append(&mut cstrings_from_osstrings(
            env::args_os().skip(1).collect())?);
        argv
    };

    let logpath = {
        let mut home = env::var_os("HOME").ok_or_else(
            || anyhow!("missing HOME var"))?;
        home.push("/._e-gnu_rs.log");
        home
    };
    
    let (sigr, sigw) = pipe()?;

    if let Some(daemonizerpid) = easy_fork()? {
        // println!("in parent, child={}", child);
        xeasy_waitpid(daemonizerpid)?;
        close(sigw)?;
        // block until buffer is closed, or more precisely, emacsclient is
        // finished, and receive its status:

        // let exitcode : i32 = slurp256_parse(sigr)?;
        // inline, XX figure out how to make this^ work.
        let exitcode = {
            let fd = sigr;
            let mut buf : [u8; 257] = [0; 257];
            let len = read(fd, &mut buf).map_err(Slurp256ParseError::Io)?;
            if len == 257 {
                bail!("input too large")
            }
            let exitcode = buf[0..len].parse()
                .with_context(|| format!("input: `{:?}`", &buf[0..len]))?;
            exitcode
        };
        
        // my $statuscode= $1+0;
        // my $exitcode= $statuscode >> 8;
        // my $signal= $statuscode & 255;
        // # print "exited with exitcode=$exitcode and signal=$signal\n";
        // if ($signal) {
        //     kill $signal, $$;
        //     exit 99; # whatever, just in case we're not being terminated
        // } else {
        //     exit $exitcode;
        // }
        unsafe { _exit(exitcode) };
    } else {
        close(sigr)?;
        setsid()?; // prevent signals from crossing over (stop ctl-c)
        let (streamr, streamw) = pipe()?;
        if let Some(pid) = easy_fork()? {
            close(streamw)?;
            {
                // XX does RawFd have a drop that closes? Should it?
                let log : RawFd = open(
                    logpath.as_os_str(),
                    OFlag::O_CREAT |
                    OFlag::O_WRONLY |
                    OFlag::O_APPEND,
                    mode_from_bits(0o600)?)?;
                let reader = BufReader::new(
                    unsafe { RawFdReader::from_raw_fd(streamr) });
                for line in reader.lines() {
                    let line = line?;
                    let line = string_remove_start(&line, "Waiting for Emacs...");
                    if line.len() > 0 {
                        let mut buf = Vec::new();
                        writeln!(&mut buf, "{}\t({})\t{}",
                                 time()?, getpid(), line)?;
                        write_all(log, &buf)?;
                    }
                }
	        close(streamr)?;
                close(log)?;
            }
            
            let status = easy_waitpid(pid)?;
            // What's the best exit code to report a signal?
            let exitcode = if let Status::Normalexit(code) = status {
                code
            } else {
                13
            };
            let mut buf = Vec::new();
            write!(&mut buf, "{}", exitcode)?;
            // Ignore PIPE errors (in case the front process was
            // killed by user; the Perl version was simply killed by
            // SIGPIPE before it had a chance to print the error
            // message):
            let _ = write_all(sigw, &buf);
            let _ = close(sigw);
            unsafe { _exit(0) };
        } else {
	    close(streamr)?;
	    close(sigw)?;
	    dup2(streamw, 1)?;
	    dup2(streamw, 2)?;
            close(streamw)?;
	    execvp(&cmd[0], &cmd)?;
        }
    }

    Ok(())
}
