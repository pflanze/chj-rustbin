use std::{
    cell::{Cell, RefCell},
    ffi::OsStr,
    fs::File,
    io::Read,
    os::unix::process::ExitStatusExt,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    sync::Arc,
};

use anyhow::{anyhow, bail, Context, Result};

use bstr::BString;
use maybe_expand_tilde::{let_path_expand_tilde, MaybeExpandTilde};

use crate::define_scoped_setting;

define_scoped_setting! {
    /// Enable or disable tracing of command invocations
    ///
    /// Example: `let _guard = SetX::new(true);`
    ///
    /// `SetX::new(true)` corresponds to `set -x`, `SetX::new(false)` to `set
    /// +x` in Bash. Unlike in Bash, the setting is removed when the `SetX`
    /// instance is dropped and the last previous active setting comes into
    /// effect.
    SetX => bool, false => SETX, SETX_ID
}

define_scoped_setting! {
    /// Directory from which to execute commands, until dropped.
    ///
    /// Example: `let _guard = CmdCwd::new(Some(arc_path));`
    ///
    /// This only registers the given path, which can be relative, for future
    /// `cmd_*` executions--it does not change the current directory of the
    /// current process (the parent process of the future commands), nor is the
    /// given path canonicalized at the time of registration. Dropping removes
    /// the registration again, meaning previous still active registrations will
    /// be used. Careful: the registration is resolved at the time of running a
    /// command, thus changing the actual current dir while relative
    /// registrations are active may not lead to the outcome that you want.
    CmdCwd => Option<Arc<Path>>, None => CMD_CWD, CMD_CWD_ID
}

pub fn cmd_acceptable<A: AsRef<OsStr>>(
    prog: &str,
    args: impl IntoIterator<Item = A>,
    acceptable_exit_codes: &[i32],
) -> Result<i32> {
    let mut cmd = Command::new(prog);
    cmd.args(args);
    if let Some(path) = CmdCwd::get() {
        cmd.current_dir(path);
    }
    if SetX::get() {
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
    if SetX::get() {
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
    if let Some(path) = CmdCwd::get() {
        cmd.current_dir(path);
    }
    if SetX::get() {
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
    if let Some(path) = CmdCwd::get() {
        cmd.current_dir(path);
    }
    if SetX::get() {
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
    if let Some(path) = CmdCwd::get() {
        cmd.current_dir(path);
    }
    if SetX::get() {
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

/// Create the file at `path` or update its modification time if it
/// exists.
///
/// Careful: gives an error for directories. Follows symlinks. Currently only
/// manually tested, only on Linux.
pub fn touch(path: &Path) -> Result<()> {
    File::create(path).with_context(|| anyhow!("touch {path:?}"))?;
    Ok(())
}

#[test]
fn t_touch() -> Result<()> {
    // touch("fun".as_ref())?;
    // touch("fun".as_ref())?;
    // touch("/bar".as_ref())?;
    // touch("/usr".as_ref())?;
    // touch("/lib".as_ref())?;
    // touch("/opt/chj/bin/concentrate".as_ref())?;
    Ok(())
}
