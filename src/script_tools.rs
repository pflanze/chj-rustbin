use std::{
    cell::Cell,
    ffi::OsStr,
    io::Read,
    marker::PhantomData,
    os::unix::process::ExitStatusExt,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

use anyhow::{anyhow, bail, Context, Result};

use bstr::BString;
use maybe_expand_tilde::{let_path_expand_tilde, MaybeExpandTilde};

// 0 = silent, 1+ = set -x style command tracing
thread_local! {
    static TRACE: Cell<u32> = 0.into();
}

fn tracing() -> bool {
    TRACE.get() > 0
}

/// Constructing enables tracing, dropping disables it (unless other `SetX`
/// instances still exist)
pub struct SetX(PhantomData<()>);

impl SetX {
    pub fn new() -> Self {
        TRACE.set(TRACE.get() + 1);
        SetX(PhantomData)
    }
}

impl Drop for SetX {
    fn drop(&mut self) {
        TRACE.set(TRACE.get() - 1);
    }
}

pub fn cmd_acceptable<A: AsRef<OsStr>>(
    prog: &str,
    args: impl IntoIterator<Item = A>,
    acceptable_exit_codes: &[i32],
) -> Result<i32> {
    let mut cmd = Command::new(prog);
    cmd.args(args);
    if tracing() {
        eprintln!("+ {cmd:?}");
    }
    let status = cmd
        .status()
        .with_context(|| anyhow!("running external command {prog:?}"))?;
    if status.success() {
        Ok(0)
    } else {
        if let Some(code) = status.code() {
            if acceptable_exit_codes.contains(&code) {
                Ok(code)
            } else {
                bail!("{prog:?} exited with code {code}")
            }
        } else {
            let signal = status.signal();
            bail!("{prog:?} exited via signal {signal:?}")
        }
    }
}

pub fn cmd<A: AsRef<OsStr>>(
    prog: &str,
    args: impl IntoIterator<Item = A>,
) -> Result<()> {
    cmd_acceptable(prog, args, &[])?;
    Ok(())
}

pub fn cmd01<A: AsRef<OsStr>>(
    prog: &str,
    args: impl IntoIterator<Item = A>,
) -> Result<i32> {
    cmd_acceptable(prog, args, &[1])
}

pub fn catfile<'t>(path: impl MaybeExpandTilde<'t>) -> Result<String> {
    let_path_expand_tilde!(path);
    std::fs::read_to_string(&path)
        .with_context(|| anyhow!("reading file {path:?}"))
}

pub fn cd<'t>(path: impl MaybeExpandTilde<'t>) -> Result<()> {
    let_path_expand_tilde!(path);
    if tracing() {
        eprintln!("+ cd {path:?}");
    }
    std::env::set_current_dir(&path).with_context(|| anyhow!("cd {path:?}"))
}

/// Run prog with args, capturing its stdout, but letting it inherit
/// stderr. Unlike shell's backtick (subcommand capture), does not
/// remove a trailing newline.
pub fn cmd_backtick<A: AsRef<OsStr>>(
    prog: &str,
    args: impl IntoIterator<Item = A>,
    acceptable_exit_codes: &[i32],
) -> Result<BString> {
    let mut cmd = Command::new(prog);
    cmd.args(args);
    cmd.stdout(Stdio::piped());
    if tracing() {
        eprintln!("+ `{cmd:?}`");
    }

    let mut child = cmd.spawn()?;
    let stdout = child.stdout.as_mut().expect("asked for it above");
    let mut output = Vec::new();
    stdout.read_to_end(&mut output)?;
    let status = child.wait()?;

    if status.success() {
        Ok(output.into())
    } else {
        if let Some(code) = status.code() {
            if acceptable_exit_codes.contains(&code) {
                Ok(output.into())
            } else {
                bail!("{prog:?} exited with code {code}")
            }
        } else {
            let signal = status.signal();
            bail!("{prog:?} exited via signal {signal:?}")
        }
    }
}

/// Run prog with args, capturing its stderr, but letting it inherit
/// stdout.
pub fn cmd_capturing_stderr<A: AsRef<OsStr>>(
    prog: &str,
    args: impl IntoIterator<Item = A>,
    acceptable_exit_codes: &[i32],
) -> Result<BString> {
    let mut cmd = Command::new(prog);
    cmd.args(args);
    cmd.stderr(Stdio::piped());
    if tracing() {
        eprintln!("+ `{cmd:?}`2");
    }

    let mut child = cmd.spawn()?;
    let out = child.stderr.as_mut().expect("asked for it above");
    let mut output = Vec::new();
    out.read_to_end(&mut output)?;
    let output = output.into();
    let status = child.wait()?;

    if status.success() {
        Ok(output)
    } else {
        if let Some(code) = status.code() {
            if acceptable_exit_codes.contains(&code) {
                Ok(output)
            } else {
                bail!("{prog:?} exited with code {code} after printing {output:?}")
            }
        } else {
            let signal = status.signal();
            bail!("{prog:?} exited via signal {signal:?} after printing {output:?}")
        }
    }
}

/// Run prog with args, capturing both its stdout and stderr,
/// returning them in that order.
pub fn cmd_capturing_both<A: AsRef<OsStr>>(
    prog: &str,
    args: impl IntoIterator<Item = A>,
    acceptable_exit_codes: &[i32],
) -> Result<(BString, BString)> {
    let mut cmd = Command::new(prog);
    cmd.args(args);
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());
    if tracing() {
        eprintln!("+ `{cmd:?} 2>&1`");
    }

    let mut child = cmd.spawn()?;

    let child_stdout = child.stdout.as_mut().expect("asked for it above");
    let child_stderr = child.stderr.as_mut().expect("asked for it above");
    let mut stdout = Vec::new();
    let mut stderr = Vec::new();

    let (r1, r2) = rayon::join(
        || child_stdout.read_to_end(&mut stdout),
        || child_stderr.read_to_end(&mut stderr),
    );
    r1?;
    r2?;

    let status = child.wait()?;

    let stdout = stdout.into();
    let stderr = stderr.into();

    if status.success() {
        Ok((stdout, stderr))
    } else {
        if let Some(code) = status.code() {
            if acceptable_exit_codes.contains(&code) {
                Ok((stdout, stderr))
            } else {
                bail!("{prog:?} exited with code {code} after printing {stderr:?}")
            }
        } else {
            let signal = status.signal();
            bail!("{prog:?} exited via signal {signal:?} after printing {stderr:?}")
        }
    }
}

/// Get a list of the paths to all items in `dir`, sorted by path
pub fn ls_paths(dir: &Path) -> Result<Vec<PathBuf>> {
    let mut items = dir
        .read_dir()
        .context("opening dir")?
        .map(|item| Ok(item.context("getting item")?.path()))
        .collect::<Result<Vec<PathBuf>>>()?;
    items.sort();
    Ok(items)
}
