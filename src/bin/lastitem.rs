#[macro_use]
extern crate log; // `tracing` crate, huh?

#[path = "../naturallanguagejoin.rs" ]
mod naturallanguagejoin;
use naturallanguagejoin::NaturalLanguageJoin;

use anyhow::{Context, Result, bail, anyhow};
use std::fs;
use std::io;
use std::env;
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::ffi::OsString;
use std::io::Write;
use std::time::SystemTime;
use std::path::PathBuf;
use structopt::StructOpt;
use rayon::iter::ParallelIterator;
use rayon::iter::IntoParallelIterator;
use std::collections::hash_set::HashSet;
use std::convert::From;


struct Excludes {
    files: HashSet<OsString>,
    dirs: HashSet<OsString>
}

fn default_excludes() -> Excludes {
    Excludes {
        files: HashSet::from(["HEUTE", "CALENDAR"].map(From::from)),
        dirs: HashSet::from([".git", ".METADATA-v2"].map(From::from))
    }
}

fn empty_excludes() -> Excludes {
    Excludes {
        files: HashSet::new(),
        dirs: HashSet::new(),
    }
}

fn generic_ignore_filename(filename: &OsString) -> bool {
    let bs = filename.as_bytes();
    let len = bs.len();
    len >= 1 && (bs[0] == b'.') ||
    len >= 2 && (bs[len-1] == b'~')
}


#[derive(StructOpt, Debug)]
/// Show the newest (with regards to mtime) item in a directory. If
/// called via a symlink as `lastfile`, shows the last file, if called
/// as `lastdir`, the last dir, if called as `lastitem`, any kind of
/// filesystem entry. Alternatively, if the --dirs or --files option
/// is given, that takes precedence.
#[structopt(name = "lastitem from chj-rustbin")]
struct Opt {
    /// consider dirs
    #[structopt(long)]
    dirs: bool,

    /// consider files
    #[structopt(long)]
    files: bool,

    /// consider other items (symlinks, pipes, sockets, device files)
    #[structopt(long)]
    other: bool,

    /// do not ignore dot and Emacs backup (ending in '~') files
    #[structopt(short, long)]
    all: bool,

    /// do not ignore special file and dir names that are ignored by
    /// default, like .git
    #[structopt(long)]
    no_ignore: bool,

    /// if a directory has no files after filtering, continue
    /// without showing a result (the default is to stop with
    /// an error)
    #[structopt(long)]
    allow_empty: bool,

    /// show the full path instead of just the filename
    #[structopt(short, long)]
    fullpath: bool,

    /// the directory to find the item in
    #[structopt(parse(from_os_str), default_value = ".")]
    directory_path: PathBuf,
}

struct Item {
    filename: OsString,
    mtime: SystemTime,
}

fn item_merge(old_item: Option<Item>, new_item: Option<Item>)
              -> Option<Item> {
    match (&old_item, &new_item) {
        (&Some(Item { filename: ref old_filename, mtime: old_mtime }),
         &Some(Item { filename: ref new_filename, mtime: new_mtime })) =>
            if (old_mtime < new_mtime)
            || ((old_mtime == new_mtime) &&
                (*old_filename > *new_filename)) {
                new_item
            } else {
                old_item
            },
        (_, None) =>
            old_item,
        (None, _) =>
            new_item
    }
}

fn main() -> Result<()> {
    let mut opt = Opt::from_args();

    if !opt.files && !opt.dirs && !opt.other {
        let arg0 = env::args_os().next();
        let exepath = arg0.ok_or_else(
            || anyhow!("can't get executable path from args_os"))?;
        let exename = Path::new(&exepath).file_name().ok_or_else(
            || anyhow!("can't extract file_name from executable path"))?;

        if exename == "lastitem" {
            opt.files = true;
            opt.dirs = true;
            opt.other = true;
        } else if exename == "lastfile" {
            opt.files = true;
        } else if exename == "lastdir" {
            opt.dirs = true;
        } else {
            bail!("inacceptable executable name: {}",
                  exename.to_string_lossy());
        }
    }

    let excludes = if opt.no_ignore {
        empty_excludes()
    } else {
        default_excludes()
    };

    env::set_current_dir(&opt.directory_path).with_context(
        || "can't chdir to the base directory")?;

    let items: Vec<OsString> =
        fs::read_dir(".").with_context(
            || "can't open the base directory for reading")?
        .filter_map(
            |entry_result: Result<fs::DirEntry, std::io::Error>|
                                  -> Option<Result<OsString,
                                                   std::io::Error>> {
                match entry_result {
                    Ok(entry) => {
                        let ft = entry.file_type()
                            .expect("does this fail on OSes needing stat?");
                        let filename = entry.file_name();
                        if opt.all || ! generic_ignore_filename(&filename) {
                            let handle_as_dir = ft.is_dir()
                                && opt.dirs && !excludes.dirs.contains(&filename);
                            let handle_as_file = ft.is_file()
                                && opt.files && !excludes.files.contains(&filename);
                            let handle_as_other = opt.other &&
                                (!ft.is_dir() && !ft.is_file());
                            if handle_as_dir || handle_as_file || handle_as_other {
                                Some(Ok(filename))
                            } else {
                                trace!(
                                    "ignoring item '{filename:?}' (type {ft:?})");
                                None
                            }
                        } else {
                            None
                        }
                    },
                    Err(e) =>
                        Some(Err(e))
                }
            })
        .collect::<Result<_,_>>()?;

    let newest_item =
        items.into_par_iter().try_fold(
            || None,
            |newest_item: Option<Item>, filename: OsString|
                                 -> Result<Option<Item>, std::io::Error> {
                let md = fs::symlink_metadata(&filename)?;
                let mtime = md.modified()?;
                Ok(item_merge(
                    newest_item,
                    Some(Item { filename, mtime })))
            })
        .try_reduce(
            || None,
            |a, b| Ok(item_merge(a, b)))?;

    match newest_item {
        Some(Item { filename, mtime: _ }) => {
            io::stdout().write_all(
                if opt.fullpath {
                    opt.directory_path.join(filename)
                        .into_os_string()
                } else {
                    filename
                }.as_bytes())?;
            io::stdout().write_all(b"\n")
        }
        None =>
            if opt.allow_empty {
                Ok(())
            } else {
                bail!("No {} found in given directory",
                      if opt.dirs && opt.files && opt.other {
                          String::from("items")
                      } else {
                          let mut which = Vec::new();
                          if opt.files { which.push("files") }
                          if opt.dirs { which.push("dirs") }
                          if opt.other { which.push("non-file-or-dir items") }
                          if which.is_empty() {
                              panic!("no option is set")
                          }
                          which.natural_language_join()
                      })
            }
    }?;

    Ok(())
}
