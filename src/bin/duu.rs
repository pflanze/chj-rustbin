use std::{
    cmp::Ordering,
    collections::{hash_map::Entry, HashMap},
    ffi::OsString,
    io::{stdout, BufWriter, Write},
    os::{linux::fs::MetadataExt, unix::prelude::OsStrExt},
    path::PathBuf,
    process::exit,
    sync::Mutex,
};

use anyhow::{anyhow, bail, Context, Result};
use chj_rustbin::io::file_path_type::FileType;
use clap::Parser;

// Bytes as KB, rounded up
fn bytes_to_kb(bytes: u64) -> u64 {
    (bytes + 1023) / 1024
}

// `man 3type stat`: st_blocks: Number of 512 B blocks allocated
const BLOCKSIZE: u64 = 512;

struct ItemError {
    file_type: FileType,
    file_name: OsString,
    error: String,
}

/// Disk usage of the contents of a particular directory
struct DirDiskUsage {
    /// Path to this directory
    path: PathBuf,
    /// Files directly inside this directory, including subdirectories
    /// themselves (excluding their contents)
    file_bytes: u64, // sum of (block * blocksize) of files only
    /// Files with a link count > 1 are recorded here, and added after
    /// finishing the scan, when the actual share count is known, so
    /// that their disk usage can be split across the usage sites
    shared_files: Vec<InodeKey>,
    /// Reflecting the *contents* of subdirectories
    subdirs: Vec<Result<DirDiskUsage>>,
    /// Errors while processing the items directly inside this
    /// directory (errors processing subdirectories are kept in
    /// `subdirs` instead)
    errors: Vec<ItemError>,
}

impl DirDiskUsage {
    /// in bytes
    fn total_files(&self, shared_inodes: &HashMap<InodeKey, InodeData>) -> u64 {
        self.file_bytes
            + self
                .shared_files
                .iter()
                .map(|inode_key| {
                    shared_inodes.get(inode_key).expect(
                        "given correct shared_inodes table, entries are always present"
                    ).bytes_share_rounded()
                })
                .sum::<u64>()
    }

    /// in bytes
    fn total_subdirs(
        &self,
        shared_inodes: &HashMap<InodeKey, InodeData>,
    ) -> u64 {
        self.subdirs
            .iter()
            .map(|result| -> u64 {
                match result {
                    Ok(du) => du.total(shared_inodes),
                    Err(_) => 0,
                }
            })
            .sum()
    }

    /// total in bytes
    fn total(&self, shared_inodes: &HashMap<InodeKey, InodeData>) -> u64 {
        self.total_files(shared_inodes) + self.total_subdirs(shared_inodes)
    }

    /// total in KB, rounded up (OK?)
    fn total_kb(&self, shared_inodes: &HashMap<InodeKey, InodeData>) -> u64 {
        bytes_to_kb(self.total(shared_inodes))
    }

    /// files in KB, rounded up (OK?)
    fn files_kb(&self, shared_inodes: &HashMap<InodeKey, InodeData>) -> u64 {
        bytes_to_kb(self.total_files(shared_inodes))
    }

    /// subdirs in KB, rounded up (OK?)
    fn dirs_kb(&self, shared_inodes: &HashMap<InodeKey, InodeData>) -> u64 {
        bytes_to_kb(self.total_subdirs(shared_inodes))
    }

    /// Collect all errors (of all kinds) of this tree into `out`
    fn get_errors(&self, limit: usize, out: &mut Vec<String>) {
        for ItemError {
            file_type,
            file_name,
            error,
        } in &self.errors
        {
            if out.len() >= limit {
                return;
            }
            out.push(format!(
                "{file_type:?} item {file_name:?} in {:?}: {error:#}",
                self.path
            ));
        }
        for subdir in &self.subdirs {
            if out.len() >= limit {
                return;
            }
            match subdir {
                Ok(du) => du.get_errors(limit, out),
                Err(error) => {
                    out.push(format!("{error:#}",));
                }
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct InodeKey {
    dev: u64,
    inode: u64,
}

struct InodeData {
    /// The usual blocks * blocksize of the inode's storage (as per
    /// stat)
    bytes: u64,
    /// This is not the inode count (number of times an inode is used
    /// in the file system), but only the number of times this inode
    /// is seen in the file system tree we're looking at. This is used
    /// when `share_globally` is *not* true.
    share_count: u64,
}

impl InodeData {
    /// The number of bytes to consider for each usage site
    fn bytes_share_rounded(&self) -> u64 {
        (self.bytes + (self.share_count + 1) / 2) / self.share_count
    }
}

#[test]
fn t_bytes_share_rounded() {
    let t = |bytes, share_count| {
        InodeData { share_count, bytes }.bytes_share_rounded()
    };
    assert_eq!(t(5, 3), 2);
    assert_eq!(t(5, 4), 1);
    assert_eq!(t(6, 3), 2);
    assert_eq!(t(6, 2), 3);
    assert_eq!(t(6, 4), 2);
    assert_eq!(t(6, 5), 1);
}

struct GetDirDiskUsage {
    one_file_system: bool,
    share_globally: bool,
    shared_inodes: Mutex<HashMap<InodeKey, InodeData>>,
}

impl GetDirDiskUsage {
    fn dir_disk_usage(
        &self,
        path: PathBuf,
        current_dev: u64,
    ) -> Result<DirDiskUsage> {
        let items = std::fs::read_dir(&path)
            .with_context(|| anyhow!("opening directory {path:?}"))?;
        let mut file_bytes = 0;
        let mut errors = vec![];
        let mut shared_files = vec![];
        let subdirs = Mutex::new(vec![]);
        rayon::scope(|scope| -> Result<()> {
            for item in items {
                let item =
                    item.with_context(|| anyhow!("reading items in {path:?}"))?;
                let file_name = item.file_name();
                match item.metadata() {
                    Ok(metadata) => {
                        /* Number of 512 B blocks allocated */
                        let blocks = metadata.st_blocks();
                        let blocksize = BLOCKSIZE; // *not* s.st_blksize()!

                        let mut inc_file_bytes = || {
                            file_bytes += blocks * blocksize;
                        };

                        if metadata.is_dir() {
                            // Include counting the dirs too (the shells only):
                            inc_file_bytes();

                            let new_dev = metadata.st_dev();

                            if (!self.one_file_system) || new_dev == current_dev
                            {
                                // recurse
                                let mut path = path.clone();
                                path.push(&file_name);
                                let subdirs = &subdirs;
                                scope.spawn(move |_| {
                                    let result =
                                        self.dir_disk_usage(path, new_dev);
                                    subdirs
                                        .lock()
                                        .expect("no crash")
                                        .push(result);
                                });
                            }
                        } else {
                            let nlink = metadata.st_nlink();
                            if nlink > 1 && blocks > 0 {
                                if self.share_globally {
                                    file_bytes += (blocks * blocksize
                                        + (nlink + 1) / 2)
                                        / nlink;
                                } else {
                                    let key = InodeKey {
                                        dev: metadata.st_dev(),
                                        inode: metadata.st_ino(),
                                    };

                                    shared_files.push(key.clone());

                                    let mut shared = self
                                        .shared_inodes
                                        .lock()
                                        .expect("no crash");
                                    match shared.entry(key) {
                                        Entry::Occupied(mut o) => {
                                            let mut data = o.get_mut();
                                            data.share_count += 1;
                                        }
                                        Entry::Vacant(v) => {
                                            v.insert(InodeData {
                                                share_count: 1,
                                                bytes: blocks * blocksize,
                                            });
                                        }
                                    }
                                }
                            } else {
                                inc_file_bytes()
                            }
                        }
                    }
                    Err(e) => {
                        // This call should never fail on Linux?
                        let file_type: FileType = (&item.file_type()?).into();
                        errors.push(ItemError {
                            file_type,
                            file_name,
                            error: format!("{e:#}"),
                        });
                    }
                }
            }
            Ok(())
        })?;

        let subdirs = subdirs.into_inner().expect("no crash either");

        Ok(DirDiskUsage {
            path,
            file_bytes,
            shared_files,
            subdirs,
            errors,
        })
    }
}

#[derive(clap::Parser, Debug)]
/// Show the space use of the immediate items (subdirectories, files)
/// in a directory.
struct Opts {
    /// Ignore other file system mounts
    #[clap(short = 'x', long)]
    one_file_system: bool,

    /// Calculate the space use of hard-linked inodes as shared across
    /// the whole file system, not only the subtree. By default, the
    /// space used by inodes is split up amongst all places in the
    /// subtree pointed at by `dir_path`; i.e. an inode of `n` bytes
    /// is also added as a total of `n` bytes to the result (but
    /// reported in each subdir as per its usage, as `n /
    /// num_total_usage_sites` per usage site). With this option,
    /// instead `n / link_count` is added to each usage site (where
    /// `link_count` is the cound in the file system, as reported by
    /// stat), i.e. usage of the inode outside of the tree pointed to
    /// by `dir_path` makes for fewer than `n` bytes added to the
    /// total reported by duu.
    #[clap(short = 'g', long)]
    share_globally: bool,

    /// Show all errors
    #[clap(long)]
    all_errors: bool,

    /// Path to the directory to show.
    #[clap(default_value = ".")]
    dir_path: PathBuf,
}

fn main() -> Result<()> {
    let Opts {
        one_file_system,
        share_globally,
        all_errors,
        dir_path,
    } = Opts::from_args();
    let top_metadata = std::fs::symlink_metadata(&dir_path)
        .with_context(|| anyhow!("getting metadata of {dir_path:?}"))?;
    if !top_metadata.is_dir() {
        bail!("given path is not to a directory (add a slash if this is a symlink): {dir_path:?}")
    }

    let gdu = GetDirDiskUsage {
        one_file_system,
        share_globally,
        shared_inodes: Default::default(),
    };
    let du = gdu.dir_disk_usage(dir_path, top_metadata.st_dev())?;

    let shared_inodes = gdu.shared_inodes.lock().expect("no crash");
    let dirs_kb = du.dirs_kb(&*shared_inodes);
    let files_kb = du.files_kb(&*shared_inodes);
    let total_kb = du.total_kb(&*shared_inodes);

    const ERRORS_LIMIT: usize = 10;
    let mut errors = vec![];
    du.get_errors(usize::MAX, &mut errors);

    let mut subdirs: Vec<(u64, DirDiskUsage)> = du
        .subdirs
        .into_iter()
        .filter_map(|du| -> Option<_> {
            let du = du.ok()?;
            Some((du.total(&*shared_inodes), du))
        })
        .collect();

    subdirs.sort_by(|(total1, du1), (total2, du2)| -> Ordering {
        total1.cmp(total2).then_with(|| du1.path.cmp(&du2.path))
    });

    let mut out = BufWriter::new(stdout().lock());
    let write_line =
        // Takes filename as bytes to allow for 'correct'
        // representation of non-UTF8 filenames (although, no control
        // of newline characters is done!)
        |kb: u64, filename: &[u8], out: &mut BufWriter<_>| -> Result<()> {
            let indent = "            ";
            let number = kb.to_string();
            let indent = if let Some(indent_rest) =
                indent.len().checked_sub(number.len())
            {
                &indent[0..indent_rest]
            } else {
                ""
            };
            out.write_all(indent.as_bytes())?;
            out.write_all(number.as_bytes())?;
            out.write_all(" ".as_bytes())?;
            out.write_all(filename)?;
            out.write_all(b"\n")?;
            Ok(())
        };

    for (total, subdir) in &subdirs {
        let kb = bytes_to_kb(*total);
        let filename =
            subdir.path.file_name().expect("subdir does have filename");
        write_line(kb, filename.as_bytes(), &mut out)?;
    }

    out.write_all(
        "-----------------------------------------------------------------\n"
            .as_bytes(),
    )?;

    write_line(dirs_kb, "k folders".as_bytes(), &mut out)?;
    write_line(files_kb, "k files".as_bytes(), &mut out)?;
    write_line(total_kb, "k total".as_bytes(), &mut out)?;

    let exit_code;
    if errors.is_empty() {
        exit_code = 0;
    } else {
        errors.sort();
        let num_errors = errors.len();
        if !all_errors && num_errors > ERRORS_LIMIT {
            errors.truncate(ERRORS_LIMIT);
            errors.push("...".into());
        }
        let s = if num_errors == 1 { "" } else { "s" };
        writeln!(&mut out, "\n{num_errors} error{s}:")?;
        for error in &errors {
            writeln!(&mut out, "{error}")?;
        }
        exit_code = 1;
    }

    drop(out);
    exit(exit_code);
}
