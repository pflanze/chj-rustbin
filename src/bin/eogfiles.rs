use std::{
    ffi::OsStr,
    io::{stdin, Read},
    os::unix::fs::symlink,
    path::PathBuf,
};

use anyhow::{anyhow, bail, Context, Result};
use bstr::{ByteSlice, ByteVec};
use chj_rustbin::script_tools::cmd;
use cj_path_util::path_util::AppendToPath;
use clap::Parser;
use tempfile::tempdir;

#[derive(clap::Parser, Debug)]
#[clap(next_line_help = true)]
/// Wrapper around the `eog` (Eyes Of Gnome?) image viewer to show a number of
/// files in the given order instead of sorting by path
struct Args {
    /// Read the list of paths from standard input
    #[clap(short, long)]
    stdin: bool,

    /// When reading the list of paths (`--stdin`), use the null-byte as entry
    /// terminator instead of the newline. Implies `--stdin` for now.
    #[clap(short, long)]
    z: bool,

    /// The paths to the files to show, in the order in which they should be
    /// shown. Conflicts with `--stdin`.
    files: Vec<PathBuf>,
}

fn main() -> Result<()> {
    let Args {
        stdin: use_stdin,
        z,
        files,
    } = Args::parse();

    let files = if use_stdin || z {
        if !files.is_empty() {
            bail!("both --stdin and file paths given")
        }
        let mut inp = Vec::new();
        {
            stdin().lock().read_to_end(&mut inp)?;
        }
        let eol = if z { b'\0' } else { b'\n' };
        inp.split(|b| *b == eol)
            .filter(|path| !path.is_empty())
            .map(|path| -> Result<PathBuf> {
                let path: &OsStr = path
                    .to_os_str()
                    .context("converting record from stdin to OsStr")?;
                Ok(PathBuf::from(path))
            })
            .collect::<Result<_>>()?
    } else {
        files
    };

    if !files.is_empty() {
        let temp = tempdir()?;
        let temp_path = temp.path();

        let mut eog_path_args = Vec::new();

        for (i, file) in files.iter().enumerate() {
            if !file.is_file() {
                continue;
            }

            let target = file
                .canonicalize()
                .with_context(|| anyhow!("canonicalizing path {file:?}"))?;

            let mut filename: Vec<u8> = format!("{i:04}-").into_bytes();
            filename.push_str(
                file.file_name()
                    .ok_or_else(|| anyhow!("path has no filename: {file:?}"))?
                    .as_encoded_bytes(),
            );
            let filename = filename.to_os_str()?;
            let source = temp_path.append(&filename);
            symlink(&target, &source).with_context(|| {
                anyhow!("creating symlink for {target:?} at {source:?}")
            })?;
            eog_path_args.push(source);
        }

        cmd("eog", &eog_path_args)?;
    }

    Ok(())
}
