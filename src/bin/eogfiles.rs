use std::{
    ffi::OsStr,
    io::{stdin, Read},
    os::unix::fs::symlink,
    path::PathBuf,
};

use anyhow::{anyhow, bail, Context, Result};
use bstr::{ByteSlice, ByteVec};
use chj_rustbin::script_tools::{cmd, SetX};
use cj_path_util::path_util::AppendToPath;
use clap::Parser;
use tempfile::tempdir;

#[derive(clap::Parser, Debug)]
#[clap(next_line_help = true)]
/// Wrapper around the `eog` (Eyes Of Gnome?) image viewer to show a number of
/// files in the given order instead of sorting by path
struct Args {
    /// Say some things about what is done
    #[clap(short, long)]
    verbose: bool,

    /// Read the list of paths from standard input
    #[clap(long)]
    stdin: bool,

    /// When reading the list of paths (`--stdin`), use the null-byte as entry
    /// terminator instead of the newline. Implies `--stdin` for now.
    #[clap(short, long)]
    z: bool,

    /// Passed to `eog` (Open in fullscreen mode)
    #[clap(short, long)]
    fullscreen: bool,

    /// Passed to `eog` (Disable image gallery)
    #[clap(short = 'g', long)]
    disable_gallery: bool,

    /// Passed to `eog` (Open in slideshow mode)
    #[clap(short, long)]
    slide_show: bool,

    /// Passed to `eog` (Start a new instance instead of reusing an existing one)
    #[clap(short, long)]
    new_instance: bool,

    /// Passed to `eog` (Open in a single window, if multiple windows are open the first one is used)
    #[clap(short = 'w', long)]
    single_window: bool,

    /// Passed to `eog` (X display to use)
    #[clap(long)]
    display: Option<String>,

    /// The paths to the files to show, in the order in which they should be
    /// shown. Conflicts with `--stdin`.
    files: Vec<PathBuf>,
}

fn main() -> Result<()> {
    let Args {
        verbose,
        stdin: use_stdin,
        z,
        files,
        fullscreen,
        disable_gallery,
        slide_show,
        new_instance,
        single_window,
        display,
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

    if verbose {
        dbg!(&files);
    }

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

    if !eog_path_args.is_empty() {
        let _x = if verbose { Some(SetX::new()) } else { None };

        let mut eog_args: Vec<&OsStr> = vec![];

        if fullscreen {
            eog_args.push("--fullscreen".as_ref())
        }
        if disable_gallery {
            eog_args.push("--disable-gallery".as_ref())
        }
        if slide_show {
            eog_args.push("--slide-show".as_ref())
        }
        if new_instance {
            eog_args.push("--new-instance".as_ref())
        }
        if single_window {
            eog_args.push("--single-window".as_ref())
        }
        if let Some(display) = &display {
            eog_args.push("--display".as_ref());
            eog_args.push(display.as_ref());
        }
        eog_args.extend(
            eog_path_args.iter().map(|path| -> &OsStr { path.as_ref() }),
        );

        cmd("eog", eog_args)?;
    }

    Ok(())
}
