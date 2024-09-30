
use anyhow::{Context, Result, bail, anyhow};
use chj_rustbin::excludes::Excludes;
use chj_rustbin::excludes::default_excludes;
use chj_rustbin::excludes::empty_excludes;
use chj_rustbin::excludes::generic_ignore_filename;
use clap::Parser;
use log::trace;
use rayon::iter::ParallelIterator;
use rayon::iter::IntoParallelIterator;
use std::fmt::Debug;
use std::fs;
use std::io;
use std::env;
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::ffi::OsString;
use std::io::Write;
use std::time::SystemTime;
use std::path::PathBuf;
use std::convert::From;

use chj_rustbin::naturallanguagejoin::NaturalLanguageJoin;


#[derive(clap::Parser, Debug)]
/// Show the newest (with regards to mtime) item in a directory. If
/// called via a symlink as `lastfile`, shows the last file, if called
/// as `lastdir`, the last dir, if called as `lastitem`, any kind of
/// filesystem entry. Alternatively, if the --dirs or --files option
/// is given, that takes precedence.
#[clap(name = "lastitem from chj-rustbin")]
struct Opt {
    /// consider dirs
    #[clap(long)]
    dirs: bool,

    /// consider files
    #[clap(long)]
    files: bool,

    /// consider other items (symlinks, pipes, sockets, device files)
    #[clap(long)]
    other: bool,

    /// do not ignore dot and Emacs backup (ending in '~') files
    #[clap(short, long)]
    all: bool,

    /// do not ignore special file and dir names that are ignored by
    /// default, like .git; you still need `--all` as well to lift its
    /// ignores, too, if you want to not ignore anything
    #[clap(long)]
    no_ignore: bool,

    /// ignore files with the given name(s); these are in addition to
    /// the default ignores, use --no-ignore to drop those.
    #[clap(long, multiple = true)]
    ignore_file: Vec<OsString>,

    /// ignore dirs with the given names(s); the same comments apply
    /// as for --ignore-files
    #[clap(long, multiple = true)]
    ignore_dir: Vec<OsString>,

    /// look for an item DEPTH levels deeper than the given directory
    /// (i.e. with DEPTH levels of directories inbetween), default: 0
    #[clap(long)]
    depth: Option<u8>,

    /// if a directory has no files after filtering, succeed without
    /// showing a result (the default is to report an error)
    #[clap(long)]
    allow_empty: bool,

    /// show the full path instead of just the filename
    #[clap(short, long)]
    fullpath: bool,

    /// the directory to find the item in
    #[clap(parse(from_os_str), default_value = ".")]
    directory_path: PathBuf,

    /// show some information about what's being done
    #[clap(short, long)]
    verbose: bool,
}


#[derive(Debug)]
struct NoPath;

#[derive(Debug)]
struct Item<P: Debug> {
    parentdir: P,
    filename: OsString,
    mtime: SystemTime,
}

impl Item<NoPath> {
    fn with_parent(self, parentdir: PathBuf) -> Item<PathBuf> {
        Item {
            parentdir,
            filename: self.filename,
            mtime: self.mtime
        }
    }
}

fn newer_item<P: Debug>(
    a: Option<Item<P>>, b: Option<Item<P>>
) -> Option<Item<P>> {
    match (a, b) {
        (Some(a), Some(b)) => {
            if (a.mtime < b.mtime) ||
                ((a.mtime == b.mtime) && (a.filename > b.filename))
            {
                Some(b)
            } else {
                Some(a)
            }
        },
        (a, None) => a,
        (None, b) => b
    }
}


#[derive(Debug)]
struct LastitemOpt {
    all: bool,
    dirs: bool,
    files: bool,
    other: bool,
}

impl From<&Opt> for LastitemOpt {
    fn from(o: &Opt) -> Self {
        LastitemOpt {
            all: o.all,
            dirs: o.dirs,
            files: o.files,
            other: o.other
        }
    }
}

fn items(dir_path: &Path, opt: &LastitemOpt, excludes: &Excludes) -> Result<Vec<OsString>> {
    // eprintln!("items({dir_path:?}, {opt:?})");
    fs::read_dir(dir_path).with_context(
        || anyhow!("opening directory {dir_path:?} for reading"))?
    .filter_map(
        |entry_result: Result<fs::DirEntry, std::io::Error>| -> Option<Result<OsString>> {
            match entry_result {
                Ok(entry) => {
                    let ft = entry.file_type()
                        .expect("does this fail on OSes needing stat?");
                    let filename = entry.file_name();
                    if opt.all || ! generic_ignore_filename(&filename) {
                        let handle_as_dir = ft.is_dir()
                            && opt.dirs
                            && !excludes.dirs.contains(&filename);
                        let handle_as_file = ft.is_file()
                            && opt.files
                            && !excludes.files.contains(&filename);
                        let handle_as_other = opt.other &&
                            (!ft.is_dir() && !ft.is_file());
                        if handle_as_dir || (
                            handle_as_file || handle_as_other) {
                            Some(Ok(filename))
                        } else {
                            trace!(
                                "ignoring item '{:?}' (type {:?})",
                                filename, ft);
                            None
                        }
                    } else {
                        None
                    }
                },
                Err(e) =>
                    Some(Err(e).with_context(|| anyhow!("read_dir on {dir_path:?}")))
            }
        })
    .collect::<Result<_,_>>()
}

fn lastitem(
    dir_path: PathBuf, opt: &LastitemOpt, excludes: &Excludes
) -> Result<Option<Item<PathBuf>>> {
    let items = items(&dir_path, opt, excludes)?;
    let newest_item =
        items.into_par_iter().try_fold(
            || None,
            |newest_item: Option<Item<NoPath>>, filename: OsString| -> Result<Option<Item<NoPath>>> {
                let path = dir_path.join(&filename);
                let md = fs::symlink_metadata(&path)
                    .with_context(|| anyhow!("symlink_metadata on {filename:?}"))?;
                let mtime = md.modified().with_context(|| anyhow!("modified on {filename:?}"))?;
                Ok(newer_item(
                    newest_item,
                    Some(Item { parentdir: NoPath, filename, mtime })))
            })
        .try_reduce(
            || None,
            |a, b| Ok(newer_item(a, b)))?;
    Ok(newest_item.map(|item| item.with_parent(dir_path)))
}

fn deeper_lastitem(
    dir_path: PathBuf, depth: u8, opt: &LastitemOpt, excludes: &Excludes
) -> Result<Option<Item<PathBuf>>> {
    if depth == 0 {
        lastitem(dir_path, opt, excludes)
    } else {
        let dir_items = items(
            &dir_path,
            &LastitemOpt { all: opt.all, dirs: true, files: false, other: false },
            excludes)?;
        dir_items.into_par_iter().map(|dir_item| {
            let path = dir_path.join(dir_item);
            deeper_lastitem(path, depth - 1, opt, excludes)
        }).try_fold(
            || None,
            |a, b_result| b_result.map(|b| newer_item(a, b))
        ).try_reduce(
            || None,
            |a, b| Ok(newer_item(a, b)))           
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

    let mut excludes =
        if opt.no_ignore {
            empty_excludes()
        } else {
            default_excludes()
        };

    for s in &opt.ignore_file {
        excludes.files.insert(s.clone());
    }

    for s in &opt.ignore_dir {
        excludes.dirs.insert(s.clone());
    }

    if opt.verbose {
        eprintln!("lastitem: {excludes:?}");
    }

    env::set_current_dir(&opt.directory_path).with_context(
        || format!("can't chdir to {:?}", opt.directory_path))?;

    let last = deeper_lastitem(PathBuf::from("."),
                               opt.depth.unwrap_or(0),
                               &LastitemOpt::from(&opt),
                               &excludes)?;

    match last {
        Some(Item { parentdir, filename, mtime: _ }) => {
            // todo: it is offering `join`, yet then we use the
            // archaic "./" stripping.
            let clean_parentdir: &Path = parentdir.strip_prefix("./").unwrap_or(&parentdir);
            let path = clean_parentdir.join(&filename);
            let full_path =
                if opt.fullpath {
                    opt.directory_path.join(path)
                } else {
                    path.into()
                };
            // (todo: is going via OsString for bytes the correct approach?)

            // unstable feature
            // io::stdout().write_all_vectored(&mut [
            //     IoSlice::new(full_path.into_os_string().as_bytes()),
            //     IoSlice::new(b"\n")])?;
            let mut lock = io::stdout().lock();
            lock.write_all(full_path.into_os_string().as_bytes())?;
            lock.write_all(b"\n")?;
            Ok(())
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
    }
}
