/// This is a re-implementation and combination of the `e`, `r`, `_e`,
/// and `_e-gnu` scripts from https://github.com/pflanze/chj-scripts

#[path = "../rawfdreader.rs"]
mod rawfdreader;
use rawfdreader::RawFdReader;
use anyhow::{Result, anyhow, bail}; 
use std::fs::OpenOptions;
use std::{env, writeln};
use std::io::{stderr, Write, BufReader, BufRead};
use libc::_exit;
use nix::unistd::{getpid, pipe, fork, ForkResult,
                  close, setsid, dup2, execvp, read, write, getuid };
use nix::time::{clock_gettime, ClockId};
use nix::sys::time::time_t;
use nix::fcntl::{open, OFlag};
use nix::sys::stat::{mode_t, Mode};
use nix::sys::wait::{wait, waitpid, WaitStatus};
use std::os::unix::io::{FromRawFd, RawFd};
use std::ffi::{CString, OsString, OsStr};
use std::os::unix::ffi::OsStringExt;
use nix::sys::signal::Signal;
use bstr_parse::{BStrParse, ParseIntError, FromBStr};
use nix::errno::Errno;
use thiserror::Error;
use nix::unistd::Pid;
use std::collections::HashMap;

fn do_debug() -> bool {
    false
}

// There's no try_map, so:
fn cstrings_from_osstrings(osstrs: &mut dyn Iterator<Item = OsString>)
                           -> Result<Vec<CString>> {
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
    pat.len() <= s.len() && pat.as_bytes() == &s.as_bytes()[0..pat.len()]
}

fn string_remove_start<'ts>(s: &'ts str, pat: &str) -> &'ts str {
    if string_matches_start(s, pat) {
        &s[pat.len()..]
    } else {
        s
    }
}

fn time() -> Result<time_t> {
    Ok(clock_gettime(ClockId::CLOCK_REALTIME)?.tv_sec())
}


// waitpid_until_gone: really wait until the given process has ended,
// and return a simpler enum.

enum Status { Normalexit(i32), Signalexit(Signal) }

//fn waitpid_until_gone<P: Into<Option<Pid>>>(pid: P) -> Result<Status> {
fn waitpid_until_gone(pid: Pid) -> Result<Status> {
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

fn wait_until_gone() -> Result<(Pid, Status)> {
    loop {
        let st = wait()?;
        match st {
            WaitStatus::Exited(pid, exitcode)
                => return Ok((pid, Status::Normalexit(exitcode))),
            WaitStatus::Signaled(pid, signal, _bool)
                => return Ok((pid, Status::Signalexit(signal))),
            _ => {} // retry
        }
    }
}




fn xcheck_status(status: Status, cmd: &[CString]) -> Result<()> {
    match status {
        Status::Normalexit(exitcode) =>
            if exitcode == 0 {
                Ok(())
            } else {
                bail!("command ended with error exit code {}: {:?}",
                      exitcode, cmd)
            },
        Status::Signalexit(signal) =>
            bail!("command ended with signal {}: {:?}",
                  signal, cmd)
    }
}

// Treat non-exit(0) cases as errors.
fn xwaitpid_until_gone(pid: Pid, cmd: &[CString]) -> Result<()> {
    xcheck_status(waitpid_until_gone(pid)?, cmd)
}

// Don't make it overly complicated, please. The original API is
// simple enough. If a Pid is given, it's the parent.
//
// Do not swallow the unsafe. Fork should be safe in our usage
// though: it should be safe even with allocation in the child as:
//  - we should not be using threading in this program (libs, though?)
//  - isn't libc's malloc safe anyway with fork?
//  - and we're not (consciously) touching any other mutexes in the children.
//
unsafe fn easy_fork() -> Result<Option<Pid>> {
    match fork()? {
        ForkResult::Parent { child, .. } => Ok(Some(child)),
        ForkResult::Child => Ok(None)
    }
}

// The return value of proc is the desired exitcode.
fn fork_proc(proc: impl FnOnce() -> Result<i32>) -> Result<Pid> {
    if let Some(pid) = unsafe { easy_fork() }? {
        Ok(pid)
    } else {
        match proc() {
            Ok(exitcode) => {
                unsafe { _exit(exitcode) }
            },
            Err(err) => {
                let _ = stderr().write(
                    format!("e: fork_proc: error in child {}: {}\n",
                            getpid(), err).as_bytes());
                unsafe { _exit(1) }
            }
        }
    }
}

// Fork proc in a new session (calls `setsid` in the child), to
// prevent signals from crossing over (stop ctl-c).
fn fork_session_proc(
    proc: impl FnOnce() -> Result<i32>
) -> Result<Pid> {
    fork_proc(|| {
        setsid()?;
        proc()
    })
}

// Run proc in a new session (a child process that calls `setsid`
// before doing work), to prevent signals from crossing over (stop
// ctl-c).
fn run_session_proc(
    proc: impl FnOnce() -> Result<i32>
) -> Result<Status> {
    waitpid_until_gone(fork_session_proc(proc)?)
}

fn is_running_in_terminal() -> bool {
    env::var_os("DISPLAY").is_none()
}


fn ask_yn(question: &str) -> Result<bool> {
    let mut opts = OpenOptions::new();
    opts.read(true).write(true).create(false);
    let opn = || opts.open("/dev/tty");
    let mut inp = BufReader::new(opn()?);
    let mut outp = opn()?;
    for n in (1..5).rev() {
        write!(outp, "{} (y/n) ", question)?;
        let mut ans = String::new();
        inp.read_line(&mut ans)?;
        if ans.len() > 1 && ans.starts_with("y") {
            return Ok(true)
        } else if ans.len() > 1 && ans.starts_with("n") {
            return Ok(false)
        }
        writeln!(outp, "Please answer with y or n, {} tries left", n)?;
    }
    bail!("Could not get an answer to the question {:?}",
          question)
}


#[derive(Error, Debug)]
enum Slurp256Error {
    #[error("I/O error: {0}")]
    Io(Errno),
    #[error("input is too large")]
    InputTooLarge,
    #[error("parse error: {0} for input: {1:?}")]
    NoParse(ParseIntError, Vec<u8>),
}

fn slurp256_parse<T: FromBStr<Err = bstr_parse::ParseIntError>>(
    fd: RawFd,
    do_chomp: bool,
) -> Result<T, Slurp256Error> {
    let mut buf : [u8; 257] = [0; 257];
    let len = read(fd, &mut buf).map_err(Slurp256Error::Io)?;
    close(fd).or_else(|e| Err(Slurp256Error::Io(e)))?;
    if len == 257 {
        return Err(Slurp256Error::InputTooLarge)
    }
    let end =
        if do_chomp && len > 0 {
            (|| {
                for i in (0..len-1).rev() {
                    if buf[i] != b'\n' {
                        return i+1
                    }
                }
                0
            })()
        } else {
            len
        };
    let s = &buf[0..end];
    s.parse().or_else(
        |e| Err(Slurp256Error::NoParse(e, Vec::from(s))))
}


fn backtick<T: 'static + Send + Sync + std::fmt::Debug + std::fmt::Display
            + FromBStr<Err = bstr_parse::ParseIntError>>(
    cmd: &Vec<CString>,
    do_chomp: bool,
    do_redir_stderr: bool,
) -> Result<T> {
    let (streamr, streamw) = pipe()?;
    if let Some(pid) = unsafe { easy_fork() }? {
        close(streamw)?;
        let pres = slurp256_parse(streamr, do_chomp);
        xwaitpid_until_gone(pid, cmd)?;
        Ok(pres?)
    } else {
        close(streamr)?;

        if do_debug() { eprintln!("e: backtick child {} {:?}", getpid(), cmd) }

        dup2(streamw, 1)?;
        if do_redir_stderr {
            dup2(streamw, 2)?;
        }
        close(streamw)?;

        execvp(&cmd[0], &cmd)?;
        unsafe { _exit(123) }; // never reached, to satisfy type system
    }
}

// Verify that env vars aren't anything unexpected
fn verify_env() -> Result<()> {
    // Emacs warns about that one, so verify it before ignoring its
    // warning:
    if let Some(got) = env::var_os("XDG_RUNTIME_DIR") {
        let uid = getuid().as_raw();
        let expected = OsString::from(format!("/run/user/{}", uid));
        if got == expected {
            Ok(())
        } else {
            bail!("expected XDG_RUNTIME_DIR env var, if set, to be {:?}, but got {:?}",
                  expected, got)
        }
    } else {
        Ok(())
    }
}

// Run cmd, waiting for its exit and logging its output.
fn run_cmd_with_log(cmd: &Vec<CString>, logpath: &OsStr) -> Result<i32> {
    let (streamr, streamw) = pipe()?;
    if let Some(pid) = unsafe { easy_fork() }? {
        close(streamw)?;
        {
            // XX does RawFd have a drop that closes? Should it?
            let log : RawFd = open(
                logpath,
                OFlag::O_CREAT |
                OFlag::O_WRONLY |
                OFlag::O_APPEND,
                mode_from_bits(0o600)?)?;
            let reader = BufReader::new(
                unsafe { RawFdReader::from_raw_fd(streamr) });
            let mut have_written = false;
            let mut pass_through = false; // print message to stdout
            for line in reader.lines() {
                let line = line?;
                let line = string_remove_start(
                    // emacsclient *always* prints this (to
                    // indicate that the buffer needs to be
                    // closed)
                    &line, "Waiting for Emacs...");
                if line.len() > 0 {
                    let mut buf = Vec::new();
                    writeln!(&mut buf, "{}\t({})\t{}",
                             time()?, getpid(), line)?;
                    write_all(log, &buf)?;
                    if !have_written {
                        if line.contains("have you started the server?")
                            || line.contains("due to a long standing Gtk+ bug")
                        /* for some reason, sometimes it says this
                         * first instead (when the previous instance
                         * was killed by way of Xorg being
                         * killed?): */
                            || line.contains("emacsclient: connect: Connection refused")
                        /* this is new as of Feb 2023 */
                            || line.contains("Should XDG_RUNTIME_DIR=")
                        {
                            eprintln!("e: starting Emacs instance");
                        } else {
                            pass_through = true;
                        }
                        have_written = true;
                    }
                    if pass_through {
                        buf.clear();
                        writeln!(&mut buf, "{}", line)?;
                        stderr().write_all(&buf)?;
                    }
                }
            }
            close(streamr)?;
            close(log)?;
        }

        let status = waitpid_until_gone(pid)?;
        // What's the best exit code to report a signal?
        let exitcode =
            if let Status::Normalexit(code) = status {
                code
            } else {
                13
            };
        Ok(exitcode)
    } else {
        close(streamr)?;
        // close(sigw)?; -- XX should close that, where?
        dup2(streamw, 1)?;
        dup2(streamw, 2)?;
        close(streamw)?;

        execvp(&cmd[0], &cmd)?;
        Ok(0) // in child, never reached, just to satisfy type system
    }
}


fn main() -> Result<()> {

    let (args, args_is_all_files) = (|| -> Result<(Vec<CString>, bool)> {
        let args = cstrings_from_osstrings(&mut env::args_os().skip(1))?;
        let mut files : Vec<CString> = Vec::new();
        let mut iargs = args.clone().into_iter();
        for arg in &mut iargs {
            let a = arg.to_bytes();
            if a == b"--" {
                files.extend(&mut iargs);
                return Ok((files, true))
            } else if a.starts_with(b"-") {
                eprintln!("e: can't currently deal with options, falling \
                           back to single emacsclient call (not opening \
                           a separate frame per file)");
                return Ok((args, false))
            } else if a.starts_with(b"+") {
                eprintln!("e: can't currently deal with positions, falling \
                           back to single emacsclient call (not opening \
                           a separate frame per file)");
                return Ok((args, false))
            } else if a.ends_with(b"~") {
                // Simply always ignore (for now? I'm not sure I've
                // ever opened backup files via `e`)
            } else {
                files.push(arg.clone());
            }
        }
        Ok((files, true))
    })()?;

    verify_env()?;
    
    let logpath = {
        let mut home = env::var_os("HOME").ok_or_else(
            || anyhow!("missing HOME env var"))?;
        home.push("/._e-gnu_rs.log");
        home
    };

    if None == env::var_os("ALTERNATE_EDITOR") {
        // Make sure emacsclient will not try to exec the file
        // argument (from PATH)! (Genuine bug?)
        env::set_var("ALTERNATE_EDITOR", "/usr/bin/false");
    }

    if args.len() > 8 {
        if ! ask_yn(&format!("e: got {} arguments, do you really want to open \
                              so many windows?",
                             args.len()))? {
            eprintln!("e: cancelled.");
            return Ok(());
        }
    }
    
    // Check if emacs daemon is up, if not, start it. Then
    // open each file (args is just files here) with a
    // separate emacsclient call, so that each is opened in a
    // separate frame.

    let start_emacs = || -> Result<()> {
        let cmd = vec!(CString::new("emacs")?,
                       CString::new("--daemon")?);
        xcheck_status(
            run_session_proc(
                || {
                    if do_debug() { eprintln!("e: child {} {:?}", getpid(), cmd) }
                    run_cmd_with_log(&cmd, &logpath)
                })?,
            &cmd)
    };
    
    let res : Result<i32> = backtick(
        &vec!(CString::new("emacsclient")?,
              CString::new("-e")?,
              CString::new("(+ 3 2)")?),
        true,
        true);
    // eprintln!("res= {:?}", res);
    match res {
        Err(_) => {
            start_emacs()?
        },
        Ok(val) => {
            if val == 5 {
                // Emacs is already up
            } else {
                start_emacs()?
            }
        }
    }

    if args_is_all_files && !is_running_in_terminal() {
        // Open each file separately, collecting the pids that
        // we then wait on.
        let mut pids : HashMap<Pid, Vec<CString>> = HashMap::new();
        for file in args {
            let cmd = vec!(
                CString::new("emacsclient")?,
                CString::new("-c")?,
                CString::new("--")?,
                file);
            let pid = fork_session_proc(|| {
                if do_debug() { eprintln!("e: child {} {:?}", getpid(), cmd) }
                run_cmd_with_log(&cmd, &logpath)?;
                Ok(0)
            })?;
            if let Some(oldcmd) = pids.insert(pid, cmd) {
                bail!("bug?: got same pid again, previously cmd {:?}",
                      oldcmd)
            }
        }
        while pids.len() > 0 {
            let (pid, status) = wait_until_gone()?;
            if let Some(cmd) = pids.remove(&pid) {
                xcheck_status(status, &cmd)?;
            } else {
                eprintln!("e: bug?: ignoring unknown pid {}", pid);
            }
        }
    } else {
        let mut cmd = vec!(
            CString::new("emacsclient")?,
            CString::new("-c")?,
        );
        cmd.append(&mut args.clone());

        if is_running_in_terminal() {
            // Need to run direcly, can't redirect log
            execvp(&cmd[0], &cmd)?;
            unsafe { _exit(123) }; // never reached, to satisfy type system
        } else {
            xcheck_status(
                run_session_proc(
                    || run_cmd_with_log(&cmd, &logpath))?,
                &cmd)?;
        }
    }
    Ok(())
}
