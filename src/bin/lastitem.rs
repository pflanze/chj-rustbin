#[macro_use]
extern crate log; // `tracing` crate, huh?

use anyhow::{Result, bail, anyhow};
use std::fs;
use std::io;
use std::env;
use std::io::Write;
use std::time::SystemTime;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
/// Show the newest (with regards to mtime) item in a directory. If
/// called via a symlink as `lastfile`, shows the last file, if called
/// as `lastdir`, the last dir, if called as `lastitem`, either file
/// or dir. Alternatively, if the --dirs or --files option is given,
/// that takes precedence. Always ignores device, pipe and socket
/// files.
#[structopt(name = "lastitem from chj-rustbin")]
struct Opt {
    /// consider dirs
    #[structopt(short, long)]
    dirs: bool,
    /// consider files
    #[structopt(short, long)]
    files: bool,
    /// the directory to find the item in
    #[structopt(parse(from_os_str), default_value = ".")]
    directory_path: PathBuf,
}

struct Item {
    path: PathBuf,
    mtime: SystemTime,
}

fn main() -> Result<()> {
    let mut opt = Opt::from_args();

    if !opt.files && !opt.dirs {
        let exepath = env::current_exe()?;
        let exename = exepath.file_name().ok_or(
            anyhow!("can't extract file_name from executable path")
        )?;

        if exename == "lastitem" {
            opt.files = true;
            opt.dirs = true;
        } else if exename == "lastfile" {
            opt.files = true;
        } else if exename == "lastdir" {
            opt.dirs = true;
        } else {
            bail!("inacceptable executable name: {}",
                  exename.to_string_lossy());
        }
    }

    let mut newest_item : Option<Item> = None;
    for entry in fs::read_dir(opt.directory_path)? {
        let itempath = entry?.path();
        let md = fs::symlink_metadata(&itempath)?;
        let mtime = md.modified()?;

        let mut keep_if_newer = |itempath| -> () {
            match newest_item {
                Some(Item { path: _, mtime: oldmtime }) =>
                    if oldmtime < mtime {
                        newest_item = Some(
                            Item { path: itempath, mtime: mtime }
                        )
                    }
                None =>
                    newest_item = Some(
                        Item { path: itempath, mtime: mtime }
                    )
            }
        };
        
        if md.is_dir() && opt.dirs {
            keep_if_newer(itempath)
        } else if md.is_file() && opt.files {
            keep_if_newer(itempath)
        } else {
            trace!("ignoring path '{:?}' (type {:?})",
                   &itempath, md.file_type());
        }
    }

    match newest_item {
        Some(Item { path, mtime: _ }) => {
            write!(io::stdout(), "{}\n", path.display())
        }
        None =>
            bail!("No {} found in given directory",
                  if opt.dirs {
                      if opt.files {
                          "directory or file"
                      } else {
                          "directory"
                      }
                  } else if opt.files {
                      "file"
                  } else {
                      panic!("neither option is set")
                  })
    }?;

    Ok(())
}
