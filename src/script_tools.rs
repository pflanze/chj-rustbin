use std::{
    ffi::OsStr,
    io::Read,
    os::unix::process::ExitStatusExt,
    process::{Command, Stdio},
};

use anyhow::{anyhow, bail, Context, Result};

use bstr::BString;
use maybe_expand_tilde::{let_path_expand_tilde, MaybeExpandTilde};

pub fn cmd_acceptable<A: AsRef<OsStr>>(
    prog: &str,
    args: impl IntoIterator<Item = A>,
    acceptable_exit_codes: &[i32],
) -> Result<i32> {
    let mut cmd = Command::new(prog);
    cmd.args(args);
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
