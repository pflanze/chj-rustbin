/// This is a re-implementation and combination of the `e`, `r`, `_e`,
/// and `_e-gnu` scripts from <https://github.com/pflanze/chj-scripts>
use std::borrow::Cow;
use std::collections::HashMap;
use std::env::VarError;
use std::ffi::{CStr, CString, OsStr, OsString};
use std::fs::OpenOptions;
use std::io::{stderr, BufRead, BufReader, Write};
use std::os::unix::io::{FromRawFd, RawFd};
use std::os::unix::prelude::OsStrExt;
use std::path::{Path, PathBuf};
use std::{env, writeln};

use anyhow::{anyhow, bail, Context, Result};
use bstr_parse::{BStrParse, FromBStr, ParseIntError};
use chj_rustbin::path_from::{MoreFrom, MoreTryFrom};
use chj_rustbin::whichgit::git_working_dir;
use libc::_exit;
use nix::errno::Errno;
use nix::fcntl::{open, OFlag};
use nix::sys::signal::Signal;
use nix::sys::stat::{mode_t, Mode};
use nix::sys::time::time_t;
use nix::sys::wait::{wait, waitpid, WaitStatus};
use nix::time::{clock_gettime, ClockId};
use nix::unistd::Pid;
use nix::unistd::{
    close, dup2, execvp, fork, getpid, getuid, pipe, read, setsid, write,
    ForkResult,
};
use regex::Regex;
use thiserror::Error;

use chj_rustbin::io::rawfdreader::RawFdReader;
use chj_rustbin::io::unix_fs::path_is_normal_file;
use chj_rustbin::path_util::extension_from_ascii_or_utf8_bytes;

fn do_debug() -> bool {
    false
}

const PATH_SEPARATOR: u8 = b'/';

// -----------------------------------------------------------------------------
// Utils, todo: move out

// There's no try_map, so:
fn cstrings_from_osstrings(osstrs: &[OsString]) -> Result<Vec<CString>> {
    let mut v: Vec<CString> = Vec::new();
    for s in osstrs {
        v.push(CString::more_try_from(s)?);
    }
    Ok(v)
}

fn mode_from_bits(mode: mode_t) -> Result<Mode> {
    Mode::from_bits(mode).ok_or_else(|| anyhow!("invalid mode: {}", mode))
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

enum Status {
    Normalexit(i32),
    Signalexit(Signal),
}

//fn waitpid_until_gone<P: Into<Option<Pid>>>(pid: P) -> Result<Status> {
fn waitpid_until_gone(pid: Pid) -> Result<Status> {
    loop {
        let st = waitpid(pid, None)?;
        match st {
            WaitStatus::Exited(_pid, exitcode) => {
                return Ok(Status::Normalexit(exitcode))
            }
            WaitStatus::Signaled(_pid, signal, _bool) => {
                return Ok(Status::Signalexit(signal))
            }
            _ => {} // retry
        }
    }
}

fn wait_until_gone() -> Result<(Pid, Status)> {
    loop {
        let st = wait()?;
        match st {
            WaitStatus::Exited(pid, exitcode) => {
                return Ok((pid, Status::Normalexit(exitcode)))
            }
            WaitStatus::Signaled(pid, signal, _bool) => {
                return Ok((pid, Status::Signalexit(signal)))
            }
            _ => {} // retry
        }
    }
}

fn xcheck_status(status: Status, cmd: &[CString]) -> Result<()> {
    match status {
        Status::Normalexit(exitcode) => {
            if exitcode == 0 {
                Ok(())
            } else {
                bail!(
                    "command ended with error exit code {}: {:?}",
                    exitcode,
                    cmd
                )
            }
        }
        Status::Signalexit(signal) => {
            bail!("command ended with signal {}: {:?}", signal, cmd)
        }
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
        ForkResult::Child => Ok(None),
    }
}

// The return value of proc is the desired exitcode.
fn fork_proc(
    program_name: &str,
    proc: impl FnOnce() -> Result<i32>,
) -> Result<Pid> {
    if let Some(pid) = unsafe { easy_fork() }? {
        Ok(pid)
    } else {
        match proc() {
            Ok(exitcode) => unsafe { _exit(exitcode) },
            Err(err) => {
                let _ = stderr().write(
                    format!(
                        "{program_name}: fork_proc: error in child {}: {}\n",
                        getpid(),
                        err
                    )
                    .as_bytes(),
                );
                unsafe { _exit(1) }
            }
        }
    }
}

// Fork proc in a new session (calls `setsid` in the child), to
// prevent signals from crossing over (stop ctl-c).
fn fork_session_proc(
    program_name: &str,
    proc: impl FnOnce() -> Result<i32>,
) -> Result<Pid> {
    fork_proc(program_name, || {
        setsid()?;
        proc()
    })
}

// Run proc in a new session (a child process that calls `setsid`
// before doing work), to prevent signals from crossing over (stop
// ctl-c).
fn run_session_proc(
    program_name: &str,
    proc: impl FnOnce() -> Result<i32>,
) -> Result<Status> {
    waitpid_until_gone(fork_session_proc(program_name, proc)?)
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
            return Ok(true);
        } else if ans.len() > 1 && ans.starts_with("n") {
            return Ok(false);
        }
        writeln!(outp, "Please answer with y or n, {} tries left", n)?;
    }
    bail!("Could not get an answer to the question {:?}", question)
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
    let mut buf: [u8; 257] = [0; 257];
    let len = read(fd, &mut buf).map_err(Slurp256Error::Io)?;
    close(fd).or_else(|e| Err(Slurp256Error::Io(e)))?;
    if len == 257 {
        return Err(Slurp256Error::InputTooLarge);
    }
    let end = if do_chomp && len > 0 {
        (|| {
            for i in (0..len - 1).rev() {
                if buf[i] != b'\n' {
                    return i + 1;
                }
            }
            0
        })()
    } else {
        len
    };
    let s = &buf[0..end];
    s.parse()
        .or_else(|e| Err(Slurp256Error::NoParse(e, Vec::from(s))))
}

fn backtick<
    T: 'static
        + Send
        + Sync
        + std::fmt::Debug
        + std::fmt::Display
        + FromBStr<Err = bstr_parse::ParseIntError>,
>(
    program_name: &str,
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

        if do_debug() {
            eprintln!("{program_name}: backtick child {} {:?}", getpid(), cmd)
        }

        dup2(streamw, 1)?;
        if do_redir_stderr {
            dup2(streamw, 2)?;
        }
        close(streamw)?;

        execvp(&cmd[0], &cmd)?;
        unsafe { _exit(123) }; // never reached, to satisfy type system
    }
}

// -----------------------------------------------------------------------------

#[derive(Debug, Clone, Copy)]
enum ProgramMode {
    Emacs,
    VSCodium,
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
fn run_cmd_with_log(
    program_name: &str,
    cmd: &Vec<CString>,
    logpath: &OsStr,
) -> Result<i32> {
    let (streamr, streamw) = pipe()?;
    if let Some(pid) = unsafe { easy_fork() }? {
        close(streamw)?;
        {
            // XX does RawFd have a drop that closes? Should it?
            let log: RawFd = open(
                logpath,
                OFlag::O_CREAT | OFlag::O_WRONLY | OFlag::O_APPEND,
                mode_from_bits(0o600)?,
            )?;
            let reader =
                BufReader::new(unsafe { RawFdReader::from_raw_fd(streamr) });
            let mut have_written = false;
            let mut pass_through = false; // print message to stdout
            for line in reader.lines() {
                let line = line?;
                let line = string_remove_start(
                    // emacsclient *always* prints this (to indicate
                    // that the buffer needs to be closed). VSCodium
                    // will never print that, doesn't matter (hacky
                    // though).
                    &line,
                    "Waiting for Emacs...",
                );
                if line.len() > 0 {
                    let mut buf = Vec::new();
                    writeln!(
                        &mut buf,
                        "{}\t({})\t{}",
                        time()?,
                        getpid(),
                        line
                    )?;
                    write_all(log, &buf)?;
                    // VSCodium will never print these, doesn't matter
                    // (hacky though).
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
                            eprintln!(
                                "{program_name}: starting editor instance"
                            );
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
        let exitcode = if let Status::Normalexit(code) = status {
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

fn is_num(s: &str) -> bool {
    (!s.is_empty()) && s.chars().all(|c| c.is_ascii_digit())
}

// "Garbage" is a ':' and any following string that is empty or
// contains non-digit characters (except for ':').
fn remove_trailing_garbage(s: &str) -> &str {
    if let Some((i, c)) = s
        .char_indices()
        .rev()
        .find(|(_i, c)| *c == ':' || *c == '/')
    {
        if c == '/' {
            return s;
        }
        let rest = &s[i + 1..];
        if rest.is_empty() || rest.contains(|c: char| !c.is_ascii_digit()) {
            &s[0..i]
        } else {
            s
        }
    } else {
        s
    }
}

#[cfg(test)]
#[test]
fn t_remove_trailing_garbage() {
    let t = remove_trailing_garbage;
    assert_eq!(t("foo"), "foo");
    assert_eq!(t("foo:"), "foo");
    assert_eq!(t("foo:53"), "foo:53");
    assert_eq!(t("foo:bar"), "foo");
    assert_eq!(t("foo:b53"), "foo");
    assert_eq!(t("foo:5b3"), "foo");
    assert_eq!(t("foo:5b3:"), "foo:5b3");
    assert_eq!(t("a:1:1/foo:5b3"), "a:1:1/foo");
    assert_eq!(t("a:1:1/foo:5b3:"), "a:1:1/foo:5b3");
    assert_eq!(t("a:1:1/fwef"), "a:1:1/fwef");
}

/// If `s` ends with ":" and some digits for line, and optionally
/// another ":" and more digits for column, then split off this part
/// and return the digits (and optionally ":" and more digits) as
/// second value. Also, as the first step, any trailing ":" along with
/// any non-digit string after it is removed, same for a leading
/// "file://".
fn parse_file_description(s: &str) -> (&str, Option<&str>) {
    let s = remove_trailing_garbage(s);
    let s = if s.starts_with("file://") { &s[7..] } else { s };
    if let Some((pos, _)) = s.char_indices().rev().find(|(_, c)| *c == ':') {
        let (path, num) = (&s[0..pos], &s[pos + 1..]);
        if is_num(num) {
            // stupid nested copy
            if let Some((pos, _)) =
                path.char_indices().rev().find(|(_, c)| *c == ':')
            {
                let (path2, num2) = (&path[0..pos], &path[pos + 1..]);
                if is_num(num2) {
                    (path2, Some(&s[pos + 1..]))
                } else {
                    (path, Some(num))
                }
            } else {
                (path, Some(num))
            }
        } else {
            (s, None)
        }
    } else {
        (s, None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_parse_file_description() {
        let t = parse_file_description;
        assert_eq!(t("/foo/bar"), ("/foo/bar", None));
        assert_eq!(t("/foo/bar:"), ("/foo/bar", None));
        assert_eq!(t("/foo/bar::"), ("/foo/bar:", None));
        assert_eq!(t("/foo/ba:r"), ("/foo/ba", None));
        assert_eq!(t(""), ("", None));
        assert_eq!(t(":"), ("", None));
        assert_eq!(t("foo:123"), ("foo", Some("123")));
        assert_eq!(t("foo:123:"), ("foo", Some("123")));
        assert_eq!(t("foo:123:abc"), ("foo", Some("123")));
        assert_eq!(t("foo:abc:"), ("foo:abc", None));
        assert_eq!(t("foo:abc:def"), ("foo:abc", None));
        assert_eq!(t("file:foo:123:"), ("file:foo", Some("123")));
        assert_eq!(t("file://foo:123:"), ("foo", Some("123")));
        assert_eq!(t("file:///foo:123:"), ("/foo", Some("123")));
        assert_eq!(t("file://123:"), ("123", None));
        assert_eq!(t("foo:"), ("foo", None));
        assert_eq!(t(":123"), ("", Some("123")));
        assert_eq!(t("foo:12a3"), ("foo", None));
        assert_eq!(t("foo:12:a3"), ("foo", Some("12")));
        assert_eq!(t("foo:12a:3"), ("foo:12a", Some("3")));
        assert_eq!(t("foo::3"), ("foo:", Some("3")));
        assert_eq!(t("foo:3:"), ("foo", Some("3")));
        assert_eq!(t("foo:12:3"), ("foo", Some("12:3")));
        assert_eq!(t("foo:12:3:"), ("foo", Some("12:3")));
        assert_eq!(t("foo:12:3:abc"), ("foo", Some("12:3")));
    }
}

// Returns `(prefixed_dir, path)` if `s` is a path coming from git
// diff.
fn starts_with_a_b(s: &str) -> Option<(&str, &str)> {
    match s.as_bytes() {
        &[b'a' | b'b', b'/', r, ..] if r != b'/' => Some((&s[0..1], &s[2..])),
        _ => None,
    }
}

#[cfg(test)]
mod tests2 {
    use super::*;

    #[test]
    fn t_starts_with_a_b() {
        let t = starts_with_a_b;
        assert_eq!(t(""), None);
        assert_eq!(t("a"), None);
        assert_eq!(t("a/"), None);
        assert_eq!(t("a//be"), None);
        assert_eq!(t("a/be"), Some(("a", "be")));
        assert_eq!(t("a/c/d.x"), Some(("a", "c/d.x")));
        assert_eq!(t("c/c/d.x"), None);
        assert_eq!(t("b/."), Some(("b", ".")));
    }
}

/// A file argument string, parsed into file path and optional
/// position info.
enum PathOrMore<'s> {
    OnlyPathFallback(Cow<'s, CStr>),
    /// The strings here came from a `CStr`, hence can always be
    /// converted back to a (`CStr`? or) `CString`.
    Parsed {
        path: Cow<'s, str>,
        pos: Option<Cow<'s, str>>,
    },
}

impl<'s> PathOrMore<'s> {
    fn with_path(&self, path: PathBuf) -> Result<Self> {
        match self {
            PathOrMore::OnlyPathFallback(_) => {
                Ok(PathOrMore::OnlyPathFallback(Cow::Owned(
                    CString::more_try_from(path)?,
                )))
            }
            PathOrMore::Parsed { path: _, pos } => Ok(PathOrMore::Parsed {
                path: path
                    .to_str()
                    .ok_or_else(|| {
                        anyhow!(
                            "given path can't be represented as string: {path:?}"
                        )
                    })?
                    .to_owned()
                    .into(),
                pos: pos.clone(),
            }),
        }
    }

    /// Tries to decode `s` as UTF-8 string (if not successful,
    /// returns None).  If the string starts with `a/` or `b/` and a
    /// non-'/' character afterwards, check if a directory of the same
    /// name exists, if not, strip it (it's then assumed to be
    /// left-overs from ). Then process (what remains) via
    /// `parse_file_description`. (This means that line numbering
    /// etc. is only detected for paths that are UTF-8, which is
    /// probably OK, at least on Linux.) This was supposed to not do
    /// any file IO, except it actually calls `std::fs::metadata`
    /// (todo: refactor). Careful: you should use `new` instead to get
    /// the full logic based on doing file IO!
    fn parse_from_cstring(path_or_more: &'s CStr) -> Self {
        if let Some(s) = path_or_more.to_str().ok() {
            if let Some((path, pos)) =
                if let Some((prefix, rest)) = starts_with_a_b(s) {
                    match std::fs::metadata(prefix) {
                        Ok(m) => {
                            if m.is_dir() {
                                Some(parse_file_description(s))
                            } else {
                                Some(parse_file_description(rest))
                            }
                        }
                        Err(_) => Some(parse_file_description(rest)),
                    }
                } else {
                    Some(parse_file_description(s))
                }
            {
                PathOrMore::Parsed {
                    path: path.into(),
                    pos: pos.map(Into::into),
                }
            } else {
                // Not cleaned, so can just as well use the fallback
                // FileOrMore::OnlyPathCleaned(s)
                PathOrMore::OnlyPathFallback(path_or_more.into())
            }
        } else {
            PathOrMore::OnlyPathFallback(path_or_more.into())
        }
    }

    /// Construct the right kind of `PathOrMore`, depending on whether
    /// the original path or parsed one exist--does file IO to find
    /// out.
    fn new(path_or_more: &'s CStr) -> Result<Self> {
        if path_is_normal_file(&path_or_more) {
            return Ok(PathOrMore::OnlyPathFallback(Cow::Borrowed(
                &path_or_more,
            )));
        }
        let slf = PathOrMore::parse_from_cstring(&path_or_more);
        if path_is_normal_file(slf.path_cstr().as_ref()) {
            // found the right match
            return Ok(slf);
        }
        // Might the path be based on the Git working directory?
        if let Some(path_from_git_base) = git_working_dir()? {
            let path_from_git_base = PathBuf::from(path_from_git_base);
            {
                let mut path_from_git_base = path_from_git_base.clone();
                path_from_git_base.push(slf.path());
                // dbg!(&path_from_git_base);
                let cstring_path = CString::more_try_from(&path_from_git_base)?;
                if path_is_normal_file(&cstring_path) {
                    return slf.with_path(path_from_git_base);
                }
            }
            if let Ok(subpath) = str::from_utf8(path_or_more.to_bytes()) {
                let mut path_from_git_base = path_from_git_base.clone();
                path_from_git_base.push(subpath);
                // dbg!(&path_from_git_base);
                let cstring_path = CString::more_try_from(&path_from_git_base)?;
                if path_is_normal_file(&cstring_path) {
                    return slf.with_path(path_from_git_base);
                }
            }
        }

        // There's no reason a non-existing path would
        // have line/column numbers added, thus assume
        // the user wants to edit the original path.
        Ok(PathOrMore::OnlyPathFallback(Cow::Borrowed(path_or_more)))
    }

    // Returns a Cow because in one branch it allocates internally.
    fn path_cstr(&self) -> Cow<'_, CStr> {
        match self {
            PathOrMore::OnlyPathFallback(p) => (&**p).into(),
            PathOrMore::Parsed { path, pos: _ } => {
                CString::new(path.as_bytes())
                    .expect("came from CStr thus no problem with \0 possible")
                    .into()
            }
        }
    }

    fn path(&self) -> PathBuf {
        PathBuf::more_from(&*self.path_cstr())
    }

    /// Only returns a suffix if it is decodeable as &str.
    fn extension(&self) -> Option<&str> {
        let bytes = match self {
            PathOrMore::OnlyPathFallback(p) => p.to_bytes(),
            PathOrMore::Parsed { path, pos: _ } => path.as_bytes(),
        };
        extension_from_ascii_or_utf8_bytes(bytes, PATH_SEPARATOR)
    }

    fn append_to_cmd_for_mode(
        &self,
        mode: ProgramMode,
        cmd: &mut Vec<CString>,
    ) {
        let cstring = |s: &str| {
            CString::new(s.as_bytes()).expect(
                "`file` came from CStr thus no problem with \0 possible",
            )
        };
        let mut append_unchanged = |path: CString| {
            cmd.append(&mut vec![CString::new("--").expect("ok"), path]);
        };
        match self {
            PathOrMore::OnlyPathFallback(path) => {
                append_unchanged(Cow::into_owned(path.clone()));
            }
            PathOrMore::Parsed { path, pos: None } => {
                let path_cstr = CString::new(path.as_bytes()).expect(
                    "`file` came from CStr thus no problem with \0 possible",
                );
                append_unchanged(path_cstr);
            }
            PathOrMore::Parsed {
                path,
                pos: Some(pos),
            } => match mode {
                ProgramMode::Emacs => {
                    cmd.append(&mut vec![
                        cstring(&format!("+{pos}")),
                        cstring("--"),
                        cstring(path.as_ref()),
                    ]);
                }
                ProgramMode::VSCodium => {
                    cmd.append(&mut vec![
                        cstring("--goto"),
                        cstring(&format!("{path}:{pos}")),
                    ]);
                }
            },
        }
    }
}

/// Check if an argument is a 'line' like "----".
fn is_hr(s: &[u8]) -> bool {
    s.len() >= 3 && s.iter().all(|b| *b == b'-')
}

fn emacs_possibly_start_daemon(
    program_name: &str,
    logpath: &OsStr,
) -> Result<()> {
    let emacs_is_up = {
        let res: Result<i32> = backtick(
            program_name,
            &vec![
                CString::new("emacsclient")?,
                CString::new("-e")?,
                CString::new("(+ 3 2)")?,
            ],
            true,
            true,
        );
        match res {
            Err(_) => false,
            Ok(val) => val == 5,
        }
    };
    if !emacs_is_up {
        let cmd = vec![CString::new("emacs")?, CString::new("--daemon")?];
        xcheck_status(
            run_session_proc(program_name, || {
                if do_debug() {
                    eprintln!("{program_name}: child {} {:?}", getpid(), cmd)
                }
                run_cmd_with_log(program_name, &cmd, &logpath)
            })?,
            &cmd,
        )?;
    }
    Ok(())
}

fn get_env(name: &str) -> Result<Option<String>> {
    match std::env::var(name) {
        Ok(s) => Ok(Some(s)),
        Err(e) => match e {
            VarError::NotPresent => Ok(None),
            VarError::NotUnicode(_) => bail!("can't read E_IS env var: {e}"),
        },
    }
}

fn main() -> Result<()> {
    let all_args_: Vec<OsString> = env::args_os().collect();
    let (program_path, program_args) = (&all_args_[0], &all_args_[1..]);
    let program_path: &Path = program_path.as_ref();
    let e_is = get_env("E_IS")?;
    let actual_program_name = program_path
        .file_name()
        .expect("program path argument has file name")
        .to_str()
        .expect("program name can be utf8-decoded");

    let program_name = if actual_program_name == "e" {
        if let Some(s) = e_is.as_ref() {
            if s.is_empty() {
                actual_program_name
            } else {
                s
            }
        } else {
            actual_program_name
        }
    } else {
        actual_program_name
    };

    let (program_mode, do_wait) = match program_name.as_bytes() {
        b"e" => (ProgramMode::Emacs, true),
        b"ew" => (ProgramMode::Emacs, true),
        b"eg" => (ProgramMode::Emacs, false),
        b"g" => (ProgramMode::Emacs, false),
        b"v" => (ProgramMode::VSCodium, true),
        b"vg" => (ProgramMode::VSCodium, false),
        b"f" => (ProgramMode::VSCodium, false),
        b"fw" => (ProgramMode::VSCodium, true),
        _ => bail!("program called by unknown name {program_name:?}, path {program_path:?}")
    };

    let f_suffices_re: Option<Regex> = if let Some(re) = get_env("F_SUFFICES")?
    {
        // First try to catch bugs in the re itself, before adding
        // more 'markup'
        Regex::new(&re).with_context(|| anyhow!("parsing {re:?} as regex"))?;
        let full_re = format!("^({re})$");
        let re_re = Regex::new(&full_re)
            .with_context(|| anyhow!("parsing {full_re:?} as regex"))?;
        Some(re_re)
    } else {
        None
    };

    match program_args
        .iter()
        .map(|s| s.as_bytes())
        .collect::<Vec<_>>()
        .as_slice()
    {
        [b"-h"] | [b"--help"] => {
            eprintln!("Usage: {program_name} [path | path:line | path:line:col ]...\n\
                       \n \
                       The arguments can also be followed by ':' and some sorts of garbage,\n \
                       or surrounded by '---..' from copy-pastes from e.g. gitk.\n \
                       Options are passed on.\n \
                       \n \
                       Variants of this program are:\n \
                       \n \
                       e    emacs, wait\n \
                       ew   emacs, wait\n \
                       eg   emacs, do not wait\n \
                       g    emacs, do not wait\n \
                       v    codium, wait\n \
                       vg   codium, do not wait\n \
                       f    codium, do not wait\n \
                       fw   codium, wait\n\
                       \n \
                       Besides starting the program with the listed name, you can\n \
                       also set the `E_IS` env var to the desired program name\n \
                       and then start it as `e`.\n\
                       \n \
                       If `F_SUFFICES` is set, it matched as a regex against the file extension\n \
                       of the file arguments (the regex is implicitly anchored with ^ and $),\n \
                       files that match are passed to VSCodium.\n\
                       \n \
                       The two can be combined!\n\
                       ");
            return Ok(());
        }
        _ => (),
    }

    // Remove `*~` items, remove "-nw" like options, treat "--",
    // i.e. turn to actual file arguments (in which case
    // `args_is_all_files == true`; note that the items can still be
    // path:linenumber, path:linenumber:colnumber, or with garbage
    // appended) , or, if that can't be done, keep them as is.
    let (files_or_args, args_is_all_files, opt_nw): (Vec<CString>, bool, bool) =
        (|| -> Result<_> {
            let args = cstrings_from_osstrings(program_args)?;
            let mut opt_nw = false;
            let mut files: Vec<CString> = Vec::new();
            let mut iargs = args.clone().into_iter();
            for arg in &mut iargs {
                let a = arg.to_bytes();
                if a == b"--" {
                    // (Idea: mark files from after "--" as such, and
                    // don't do some magic then?)
                    files.extend(&mut iargs);
                    // Return args_is_all_files=`true` since otherwise we
                    // returned earlier already.
                    return Ok((files, true, opt_nw));
                } else if a == b"-nw" || a == b"-t" || a == b"--tty" {
                    opt_nw = true;
                } else if a.starts_with(b"-") && !is_hr(a) {
                    eprintln!("{program_name}: can't currently deal with options, falling \
                           back to single emacsclient call (not opening \
                           a separate frame per file)");
                    return Ok((args, false, opt_nw));
                } else if a.starts_with(b"+") {
                    // XX todo: now that we support "path:123" style
                    // positions, either remove this or implement it too.
                    eprintln!("{program_name}: can't currently deal with '+' style positions, falling \
                           back to single emacsclient call (not opening \
                           a separate frame per file); note that 'file:123' style positions \
                           are supported.");
                    return Ok((args, false, opt_nw));
                } else if a.ends_with(b"~") {
                    // Simply always ignore such arguments (for now? But
                    // I'm not sure I've ever opened backup files via `e`)
                } else {
                    files.push(arg);
                }
            }
            Ok((files, true, opt_nw))
        })()?;

    // Drop superfluous `e` arguments from accidentally running
    // e.g. `e e foo`, and file paths consisting of 3 or more `-`
    // characters (copy pastes from gitk).
    let files_or_args = if args_is_all_files {
        let mut e_exists = {
            // I had a Lazy something somewhere; not the one from
            // once_cell.
            let mut cache = None;
            move || -> bool {
                if let Some(val) = cache {
                    val
                } else {
                    let val = PathBuf::from(program_name).exists();
                    cache = Some(val);
                    val
                }
            }
        };
        files_or_args
            .into_iter()
            .filter(|a| {
                if a.as_bytes() == program_name.as_bytes() {
                    e_exists()
                } else if is_hr(a.as_bytes()) {
                    path_is_normal_file(a)
                } else {
                    true
                }
            })
            .collect()
    } else {
        files_or_args
    };

    let (is_running_in_terminal, add_nw_option) =
        if env::var_os("DISPLAY").is_none() {
            (true, false)
        } else if args_is_all_files {
            // We stripped the "-nw" or similar options if they were
            // present.
            (opt_nw, opt_nw)
        } else {
            // We did retain the "-nw" or similar options; only need
            // to add_nw_option if not already seen it.
            (opt_nw, !opt_nw)
        };

    verify_env()?;

    let logpath = {
        let mut logpath = env::var_os("HOME")
            .ok_or_else(|| anyhow!("missing HOME env var"))?;
        logpath.push(format!("/.{program_name}.log"));
        logpath
    };

    if None == env::var_os("ALTERNATE_EDITOR") {
        // Make sure emacsclient will not try to exec the file
        // argument (from PATH)! (Genuine bug?)
        env::set_var("ALTERNATE_EDITOR", "/usr/bin/false");
    }

    if files_or_args.len() > 8 {
        if !ask_yn(&format!(
            "{program_name}: got {} arguments, do you really want to open \
                              so many files?",
            files_or_args.len()
        ))? {
            eprintln!("{program_name}: cancelled.");
            return Ok(());
        }
    }

    // Check if emacs daemon is up, if not, start it. Then open each
    // file (args is just files here) with a separate emacsclient
    // call, so that each is opened in a separate frame.

    emacs_possibly_start_daemon(program_name, &logpath)?;

    let client_cmd_base_for_mode = |mode| match mode {
        ProgramMode::Emacs => {
            let mut cmd = vec![
                CString::new("emacsclient").unwrap(),
                CString::new("-c").unwrap(),
            ];
            if add_nw_option {
                cmd.push(CString::new("-nw").unwrap());
            }
            cmd
        }
        ProgramMode::VSCodium => {
            vec![
                CString::new("codium").unwrap(),
                CString::new("--wait").unwrap(),
            ]
        }
    };
    // VSCodium will never run in the terminal, this is hacky though.
    if args_is_all_files && !is_running_in_terminal {
        // Open each file separately, collecting the pids that
        // we then wait on.
        let mut pids: HashMap<Pid, Vec<CString>> = HashMap::new();
        for file in files_or_args {
            let path_or_more = PathOrMore::new(&file)?;
            let mode = if let Some(re) = f_suffices_re.as_ref() {
                if let Some(ext) = path_or_more.extension() {
                    if re.is_match(ext) {
                        ProgramMode::VSCodium
                    } else {
                        program_mode
                    }
                } else {
                    program_mode
                }
            } else {
                program_mode
            };

            let cmd = {
                let mut cmd = client_cmd_base_for_mode(mode);
                path_or_more.append_to_cmd_for_mode(mode, &mut cmd);
                cmd
            };
            let pid = fork_session_proc(program_name, || {
                if do_debug() {
                    eprintln!("{program_name}: child {} {:?}", getpid(), cmd)
                }
                run_cmd_with_log(program_name, &cmd, &logpath)?;
                Ok(0)
            })?;
            if let Some(oldcmd) = pids.insert(pid, cmd) {
                bail!("bug?: got same pid again, previously cmd {:?}", oldcmd)
            }
        }
        if do_wait {
            while pids.len() > 0 {
                let (pid, status) = wait_until_gone()?;
                if let Some(cmd) = pids.remove(&pid) {
                    xcheck_status(status, &cmd)?;
                } else {
                    eprintln!(
                        "{program_name}: bug?: ignoring unknown pid {}",
                        pid
                    );
                }
            }
        }
    } else {
        let mut cmd = client_cmd_base_for_mode(program_mode);
        if args_is_all_files {
            cmd.push(CString::new("--").unwrap());
        }
        cmd.append(&mut files_or_args.to_owned());

        if is_running_in_terminal {
            // Need to run direcly, can't redirect log.
            // VSCodium will never run here, just pretend (hacky).
            execvp(&cmd[0], &cmd)?;
        } else {
            xcheck_status(
                run_session_proc(program_name, || {
                    run_cmd_with_log(program_name, &cmd, &logpath)
                })?,
                &cmd,
            )?;
        }
    }
    Ok(())
}
