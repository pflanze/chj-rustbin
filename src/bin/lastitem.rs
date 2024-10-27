use std::convert::From;
use std::env;
use std::ffi::OsString;
use std::fmt::Debug;
use std::fs;
use std::io;
use std::io::Write;
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::path::PathBuf;
use std::time::SystemTime;

use anyhow::{anyhow, bail, Context, Result};
use chj_rustbin::region::Region;
use clap::Parser;
use rayon::iter::IntoParallelIterator;
use rayon::iter::ParallelIterator;

use chj_rustbin::impl_item_options_from;
use chj_rustbin::io::excludes::{default_excludes, empty_excludes, Excludes};
use chj_rustbin::io::file_path_type::{
    file_path_types_vec, FilePathType, ItemOptions,
};

use chj_rustbin::text::naturallanguagejoin::NaturalLanguageJoin;

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

impl_item_options_from!(Opt);

#[derive(Debug)]
pub struct NoPath;

#[derive(Debug)]
pub struct Item<P: Debug> {
    pub parentdir: P,
    pub filename: OsString,
    pub mtime: SystemTime,
}

impl Item<NoPath> {
    pub fn with_parent(self, parentdir: &PathBuf) -> Item<PathBuf> {
        Item {
            parentdir: parentdir.clone(),
            filename: self.filename,
            mtime: self.mtime,
        }
    }
}

pub fn newer_item<P: Debug>(
    a: Option<Item<P>>,
    b: Option<Item<P>>,
) -> Option<Item<P>> {
    match (a, b) {
        (Some(a), Some(b)) => {
            if (a.mtime < b.mtime)
                || ((a.mtime == b.mtime) && (a.filename > b.filename))
            {
                Some(b)
            } else {
                Some(a)
            }
        }
        (a, None) => a,
        (None, b) => b,
    }
}

fn lastitem(
    dir_path: &PathBuf,
    opt: ItemOptions,
    excludes: &Excludes,
) -> Result<Option<Item<PathBuf>>> {
    let region = Region::new();
    let dir_path_id = region.store(dir_path.clone());
    let items =
        file_path_types_vec(&region, dir_path_id, opt, excludes, false)?;
    let newest_item = items
        .into_par_iter()
        .try_fold(
            || None,
            |newest_item: Option<Item<NoPath>>,
             FilePathType { file_name, .. }|
             -> Result<Option<Item<NoPath>>> {
                let path = dir_path.join(&file_name);
                let md = fs::symlink_metadata(&path).with_context(|| {
                    anyhow!("symlink_metadata on {file_name:?}")
                })?;
                let mtime = md
                    .modified()
                    .with_context(|| anyhow!("modified on {file_name:?}"))?;
                Ok(newer_item(
                    newest_item,
                    Some(Item {
                        parentdir: NoPath,
                        filename: file_name,
                        mtime,
                    }),
                ))
            },
        )
        .try_reduce(|| None, |a, b| Ok(newer_item(a, b)))?;
    Ok(newest_item.map(|item| item.with_parent(dir_path)))
}

fn deeper_lastitem(
    dir_path: PathBuf,
    depth: u8,
    opt: ItemOptions,
    excludes: &Excludes,
) -> Result<Option<Item<PathBuf>>> {
    if depth == 0 {
        lastitem(&dir_path, opt, excludes)
    } else {
        let region = Region::new();
        let dir_path = region.store(dir_path);
        let dir_items = file_path_types_vec(
            &region,
            dir_path,
            ItemOptions {
                dirs: true,
                files: false,
                other: false,
            },
            excludes,
            false,
        )?;
        dir_items
            .into_par_iter()
            .map(|FilePathType { file_name, .. }| {
                let path = region.get(dir_path).join(file_name);
                deeper_lastitem(path, depth - 1, opt, excludes)
            })
            .try_fold(|| None, |a, b_result| b_result.map(|b| newer_item(a, b)))
            .try_reduce(|| None, |a, b| Ok(newer_item(a, b)))
    }
}

fn main() -> Result<()> {
    let mut opt = Opt::from_args();

    if !opt.files && !opt.dirs && !opt.other {
        let arg0 = env::args_os().next();
        let exepath = arg0
            .ok_or_else(|| anyhow!("can't get executable path from args_os"))?;
        let exename = Path::new(&exepath).file_name().ok_or_else(|| {
            anyhow!("can't extract file_name from executable path")
        })?;

        if exename == "lastitem" {
            opt.files = true;
            opt.dirs = true;
            opt.other = true;
        } else if exename == "lastfile" {
            opt.files = true;
        } else if exename == "lastdir" {
            opt.dirs = true;
        } else {
            bail!(
                "inacceptable executable name: {}",
                exename.to_string_lossy()
            );
        }
    }

    let mut excludes = if opt.no_ignore {
        empty_excludes(opt.all)
    } else {
        default_excludes(opt.all)
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

    env::set_current_dir(&opt.directory_path)
        .with_context(|| format!("can't chdir to {:?}", opt.directory_path))?;

    let last = deeper_lastitem(
        PathBuf::from("."),
        opt.depth.unwrap_or(0),
        ItemOptions::from(&opt),
        &excludes,
    )?;

    match last {
        Some(Item {
            parentdir,
            filename,
            mtime: _,
        }) => {
            // todo: it is offering `join`, yet then we use the
            // archaic "./" stripping.
            let clean_parentdir: &Path =
                parentdir.strip_prefix("./").unwrap_or(&parentdir);
            let path = clean_parentdir.join(&filename);
            let full_path = if opt.fullpath {
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
        None => {
            if opt.allow_empty {
                Ok(())
            } else {
                bail!(
                    "No {} found in given directory",
                    if opt.dirs && opt.files && opt.other {
                        String::from("items")
                    } else {
                        let mut which = Vec::new();
                        if opt.files {
                            which.push("files")
                        }
                        if opt.dirs {
                            which.push("dirs")
                        }
                        if opt.other {
                            which.push("non-file-or-dir items")
                        }
                        if which.is_empty() {
                            panic!("no option is set")
                        }
                        which.natural_language_join()
                    }
                )
            }
        }
    }
}
