use std::{
    collections::{hash_map::Entry, HashMap},
    ffi::OsString,
    os::unix::fs::MetadataExt,
    path::PathBuf,
    sync::Mutex,
};

use anyhow::{anyhow, Context, Result};

use crate::io::file_path_type::FileType;

/// In GiB fixed with 3 decimal places. Pads to 999.999 GiB.
pub fn bytes_to_gib_string(bytes: u64) -> String {
    let mb_times_1000 = (bytes / 1024 + 512) * 1000 / 1024;
    let gb_times_1000 = mb_times_1000 / 1024;
    let digits = gb_times_1000.to_string();
    let len = digits.len();
    if len <= 3 {
        format!("  0.{gb_times_1000:03}")
    } else {
        let hi = &digits[0..len - 3];
        let lo = &digits[len - 3..];
        format!("{hi:>3}.{lo}")
    }
}

#[test]
fn t_bytes_to_gib_string() {
    let t = bytes_to_gib_string;
    assert_eq!(t(0), "  0.000");
    assert_eq!(t(20000), "  0.000");
    assert_eq!(t(500000), "  0.000");
    assert_eq!(t(600000), "  0.001");
    assert_eq!(t(1024 * 1024 * 1024), "  1.000");
    assert_eq!(t(512 * 1024 * 1024), "  0.500");
    assert_eq!(t(900 * 1024 * 1024 * 1024), "900.000");
}

// Bytes as KB, rounded up
pub fn bytes_to_kb(bytes: u64) -> u64 {
    (bytes + 1023) / 1024
}

pub fn to_human_readable(
    si: bool,
    // bytes
    mut val: u64,
) -> (u64, &'static str) {
    let powers = if si { 1000 } else { 1024 };
    let mut n = 0;
    // What multiplier to use? Ah, `du` actually uses fractional
    // format below some value, like "7.6G"
    const MULTIPLIER: u64 = 10;
    loop {
        let val2 = (val + powers / 2) / powers;
        if val2 > MULTIPLIER {
            val = val2;
            n += 1;
        } else {
            break;
        }
    }
    let unit = match n {
        0 => "",
        1 => {
            if si {
                "k"
            } else {
                "K"
            }
        }
        2 => "M",
        3 => "G",
        4 => "T",
        5 => "P",
        6 => "E",
        // 7 => "Z",
        // 8 => "Y",
        // 9 => "R",
        // 10 => "Q",
        _ => unreachable!("doesn't fit into 64-bit"),
    };
    (val, unit)
}

#[test]
fn t_to_human_readable() {
    assert_eq!(to_human_readable(true, 1000), (1000, ""));
    assert_eq!(to_human_readable(true, 9000), (9000, ""));
    assert_eq!(to_human_readable(true, 10499), (10499, ""));
    assert_eq!(to_human_readable(true, 10500), (11, "k"));
    assert_eq!(to_human_readable(true, 1000000), (1000, "k"));
    assert_eq!(to_human_readable(true, 1900000), (1900, "k"));
    assert_eq!(to_human_readable(true, 10000000), (10000, "k"));
    assert_eq!(to_human_readable(true, 11000000), (11, "M"));

    assert_eq!(to_human_readable(false, 1000), (1000, ""));
    assert_eq!(to_human_readable(false, 9000), (9000, ""));
    assert_eq!(to_human_readable(false, 10751), (10751, ""));
    assert_eq!(to_human_readable(false, 10752), (11, "K"));
    assert_eq!(to_human_readable(false, 11264), (11, "K"));
    assert_eq!(to_human_readable(false, 1000000), (977, "K"));
    assert_eq!(to_human_readable(false, 10000000), (9766, "K"));
    assert_eq!(to_human_readable(false, 11000000), (10742, "K"));
    // odd?
    assert_eq!(to_human_readable(false, 11009535), (10751, "K"));
    assert_eq!(to_human_readable(false, 11009536), (11, "M"));
    assert_eq!(to_human_readable(false, 11010047), (11, "M"));
    // /odd
    assert_eq!(to_human_readable(false, 11010048), (11, "M"));

    assert_eq!(to_human_readable(true, u64::MAX / 2), (9223, "P"));
    // assert_eq!(to_human_readable(true, u64::MAX), (9223, "P"));
}

// `man 3type stat`: st_blocks: Number of 512 B blocks allocated
const BLOCKSIZE: u64 = 512;

pub struct ItemError {
    pub file_type: FileType,
    pub file_name: OsString,
    pub error: String,
}

/// Disk usage of the contents of a particular directory
pub struct DirDiskUsage {
    /// Path to this directory
    pub path: PathBuf,
    /// Files directly inside this directory, including subdirectories
    /// themselves (excluding their contents)
    pub file_bytes: u64,
    /// Files with a link count > 1 are recorded here, and added after
    /// finishing the scan, when the actual share count is known, so
    /// that their disk usage can be split across the usage sites
    pub shared_files: Vec<InodeKey>,
    /// Reflecting the *contents* of subdirectories
    pub subdirs: Vec<Result<DirDiskUsage>>,
    /// Errors while processing the items directly inside this
    /// directory (errors processing subdirectories are kept in
    /// `subdirs` instead)
    pub errors: Vec<ItemError>,
}

impl DirDiskUsage {
    /// in bytes
    pub fn total_files(
        &self,
        shared_inodes: &HashMap<InodeKey, InodeData>,
    ) -> u64 {
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
    pub fn total_subdirs(
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
    pub fn total(&self, shared_inodes: &HashMap<InodeKey, InodeData>) -> u64 {
        self.total_files(shared_inodes) + self.total_subdirs(shared_inodes)
    }

    /// Collect all errors (of all kinds) of this tree into `out`
    pub fn get_errors(&self, limit: usize, out: &mut Vec<String>) {
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
pub struct InodeKey {
    pub dev: u64,
    pub inode: u64,
}

pub struct InodeData {
    /// The usual blocks * blocksize of the inode's storage (as per
    /// stat)
    pub bytes: u64,
    /// This is not the inode count (number of times an inode is used
    /// in the file system), but only the number of times this inode
    /// is seen in the file system tree we're looking at. This is used
    /// when `share_globally` is *not* true.
    pub share_count: u64,
}

impl InodeData {
    /// The number of bytes to consider for each usage site
    pub fn bytes_share_rounded(&self) -> u64 {
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

pub struct GetDirDiskUsage {
    pub one_file_system: bool,
    pub share_globally: bool,
    pub shared_inodes: Mutex<HashMap<InodeKey, InodeData>>,
}

impl GetDirDiskUsage {
    pub fn dir_disk_usage(
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
                // Note: this is `metadata` on DirEntry, not Path; on
                // Unix this calls symlink_metadata internally (on
                // Windows it has the info already).
                match item.metadata() {
                    Ok(metadata) => {
                        /* Number of 512 B blocks allocated */
                        let blocks = metadata.blocks();
                        let blocksize = BLOCKSIZE; // *not* s.blksize()!

                        let mut inc_file_bytes = || {
                            file_bytes += blocks * blocksize;
                        };

                        if metadata.is_dir() {
                            let new_dev = metadata.dev();

                            if (!self.one_file_system) || new_dev == current_dev
                            {
                                // Include counting the dir too (the
                                // 'shell' only; and only if
                                // recursing, since otherwise the
                                // space use is on the other file
                                // system):
                                inc_file_bytes();

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
                            let nlink = metadata.nlink();
                            if nlink > 1 && blocks > 0 {
                                if self.share_globally {
                                    file_bytes += (blocks * blocksize
                                        + (nlink + 1) / 2)
                                        / nlink;
                                } else {
                                    let key = InodeKey {
                                        dev: metadata.dev(),
                                        inode: metadata.ino(),
                                    };

                                    shared_files.push(key.clone());

                                    let mut shared = self
                                        .shared_inodes
                                        .lock()
                                        .expect("no crash");
                                    match shared.entry(key) {
                                        Entry::Occupied(mut o) => {
                                            let data = o.get_mut();
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
