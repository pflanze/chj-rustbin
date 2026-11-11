use std::{
    borrow::Cow,
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
use cj_path_util::path_util::AppendToPath;
use maybe_expand_tilde::{let_path_expand_tilde, MaybeExpandTilde};

use crate::utillib::into_arc_path::IntoArcPath;

#[derive(Debug, Clone, Default)]
pub struct ScriptContext {
    /// Whether to show a trace in the style of `set -x` in bash, and
    /// the number of `+` characters shown (which is incremented by
    /// `set_cwd`). Initialize with 1 if you set it.
    pub x: Option<u8>,
    /// The current directory to use when resolving paths, instead of
    /// the actual current directoy of the running process; if this is
    /// a relative path, it is relative to the current directory of
    /// the process at the time of use!
    pub cwd: Option<Arc<Path>>,
}

impl ScriptContext {
    fn pluses(&self) -> Option<&'static str> {
        const PLUSES: &str = "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++\
                              +++++++++++++++++++++++++++++++++++++++++++++++++++++++++\
                              +++++++++++++++++++++++++++++++++++++++++++++++++++++++++\
                              +++++++++++++++++++++++++++++++++++++++++++++++++++++++++\
                              +++++++++++++++++++++++++++";
        self.x.map(|n| &PLUSES[0..(n as usize)])
    }

    /// Base `path` on `self.cwd` if defined
    ///
    /// If `self.cwd` is set and `path` is relative, add it to the
    /// former, otherwise leave unchanged
    pub fn resolve_path<'path>(&self, path: &'path Path) -> Cow<'path, Path> {
        if path.is_absolute() {
            path.into()
        } else if let Some(cwd) = &self.cwd {
            cwd.append(path).into()
        } else {
            path.into()
        }
    }

    pub fn set_cwd<P: IntoArcPath>(&self, cwd: P) -> Self {
        let this = Self {
            x: self.x.map(|x| x.saturating_add(1)),
            cwd: Some(cwd.into_arc_path()),
        };
        if let Some(pluses) = this.pluses() {
            eprintln!(
                "{pluses} cd {:?}",
                this.cwd.as_ref().expect("set above")
            );
        }
        this
    }

    pub fn unset_cwd(&self) -> Self {
        Self {
            x: self.x,
            cwd: None,
        }
    }

    pub fn cmd_acceptable<A: AsRef<OsStr>>(
        &self,
        prog: &str,
        args: impl IntoIterator<Item = A>,
        acceptable_exit_codes: &[i32],
    ) -> Result<i32> {
        let mut cmd = Command::new(prog);
        cmd.args(args);
        if let Some(path) = &self.cwd {
            cmd.current_dir(path);
        }
        if let Some(pluses) = self.pluses() {
            eprintln!("{pluses} {cmd:?}");
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
        &self,
        prog: &str,
        args: impl IntoIterator<Item = A>,
    ) -> Result<()> {
        self.cmd_acceptable(prog, args, &[])?;
        Ok(())
    }

    pub fn cmd01<A: AsRef<OsStr>>(
        &self,
        prog: &str,
        args: impl IntoIterator<Item = A>,
    ) -> Result<i32> {
        self.cmd_acceptable(prog, args, &[1])
    }

    pub fn catfile<'t>(
        &self,
        path: impl MaybeExpandTilde<'t>,
    ) -> Result<String> {
        let_path_expand_tilde!(path);
        let path = self.resolve_path(&path);
        std::fs::read_to_string(&path)
            .with_context(|| anyhow!("reading file {path:?}"))
    }

    pub fn cd<'t>(&mut self, path: impl MaybeExpandTilde<'t>) {
        let_path_expand_tilde!(path);
        if let Some(pluses) = self.pluses() {
            eprintln!("{pluses} cd {path:?}");
        }
        self.cwd = Some(path.into());
    }

    /// Run prog with args, capturing its stdout, but letting it inherit
    /// stderr. Unlike shell's backtick (subcommand capture), does not
    /// remove a trailing newline.
    pub fn cmd_backtick<A: AsRef<OsStr>>(
        &self,
        prog: &str,
        args: impl IntoIterator<Item = A>,
        acceptable_exit_codes: &[i32],
    ) -> Result<BString> {
        let mut cmd = Command::new(prog);
        cmd.args(args);
        cmd.stdout(Stdio::piped());
        if let Some(path) = &self.cwd {
            cmd.current_dir(path);
        }
        if let Some(pluses) = self.pluses() {
            eprintln!("{pluses} `{cmd:?}`");
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
        &self,
        prog: &str,
        args: impl IntoIterator<Item = A>,
        acceptable_exit_codes: &[i32],
    ) -> Result<BString> {
        let mut cmd = Command::new(prog);
        cmd.args(args);
        cmd.stderr(Stdio::piped());
        if let Some(path) = &self.cwd {
            cmd.current_dir(path);
        }
        if let Some(pluses) = self.pluses() {
            eprintln!("{pluses} `{cmd:?}`2");
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
                    bail!("{prog:?} exited with code {code} after printing:\n{output}")
                }
            } else {
                let signal = status.signal();
                bail!("{prog:?} exited via signal {signal:?} after printing:\n{output}")
            }
        }
    }

    /// Run prog with args, capturing both its stdout and stderr,
    /// returning them in that order.
    pub fn cmd_capturing_both<A: AsRef<OsStr>>(
        &self,
        prog: &str,
        args: impl IntoIterator<Item = A>,
        acceptable_exit_codes: &[i32],
    ) -> Result<(BString, BString)> {
        let mut cmd = Command::new(prog);
        cmd.args(args);
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());
        if let Some(path) = &self.cwd {
            cmd.current_dir(path);
        }
        if let Some(pluses) = self.pluses() {
            eprintln!("{pluses} `{cmd:?} 2>&1`");
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

    /// Run prog with args, writing its stdout to `output`, but letting it
    /// inherit stderr.
    pub fn cmd_with_stdout_to<A: AsRef<OsStr>>(
        &self,
        prog: &str,
        args: impl IntoIterator<Item = A>,
        output: File,
        acceptable_exit_codes: &[i32],
    ) -> Result<()> {
        let mut cmd = Command::new(prog);
        cmd.args(args);
        cmd.stdout(output);
        if let Some(path) = &self.cwd {
            cmd.current_dir(path);
        }
        if let Some(pluses) = self.pluses() {
            eprintln!("{pluses} `{cmd:?}`2");
        }

        let mut child = cmd.spawn()?;
        let status = child.wait()?;

        if status.success() {
            Ok(())
        } else {
            if let Some(code) = status.code() {
                if acceptable_exit_codes.contains(&code) {
                    Ok(())
                } else {
                    bail!("{prog:?} exited with code {code}")
                }
            } else {
                let signal = status.signal();
                bail!("{prog:?} exited via signal {signal:?}")
            }
        }
    }

    pub fn cmd_with_stdin_from<A: AsRef<OsStr>>(
        &self,
        input: File,
        prog: &str,
        args: impl IntoIterator<Item = A>,
        acceptable_exit_codes: &[i32],
    ) -> Result<()> {
        let mut cmd = Command::new(prog);
        cmd.args(args);
        cmd.stdin(input);
        if let Some(path) = &self.cwd {
            cmd.current_dir(path);
        }
        if let Some(pluses) = self.pluses() {
            eprintln!("{pluses} `{cmd:?}`2");
        }

        let mut child = cmd.spawn()?;
        let status = child.wait()?;

        if status.success() {
            Ok(())
        } else {
            if let Some(code) = status.code() {
                if acceptable_exit_codes.contains(&code) {
                    Ok(())
                } else {
                    bail!("{prog:?} exited with code {code}")
                }
            } else {
                let signal = status.signal();
                bail!("{prog:?} exited via signal {signal:?}")
            }
        }
    }

    /// Get a list of the paths to all items in `dir`, relative to
    /// `self.cwd` if any, sorted by path
    pub fn ls_paths(&self, dir: &Path) -> Result<Vec<PathBuf>> {
        let mut items = self
            .resolve_path(dir)
            .read_dir()
            .context("opening dir")?
            .map(|item| Ok(item.context("getting item")?.path()))
            .collect::<Result<Vec<PathBuf>>>()?;
        items.sort();
        Ok(items)
    }

    /// Create the file at `path` (relative to `self.cwd` if any) or
    /// update its modification time if it exists.
    ///
    /// Careful: gives an error for directories. Follows symlinks. Currently only
    /// manually tested, only on Linux.
    pub fn touch(&self, path: &Path) -> Result<()> {
        let path = self.resolve_path(path);
        File::create(&path).with_context(|| anyhow!("touch {path:?}"))?;
        Ok(())
    }
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
