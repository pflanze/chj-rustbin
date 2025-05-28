use std::{
    ffi::OsString,
    io::{stdout, BufWriter, Write},
    os::{linux::fs::MetadataExt, unix::prelude::OsStrExt},
    path::PathBuf,
    sync::Mutex,
};

use anyhow::{anyhow, Context, Result};
use chj_rustbin::io::file_path_type::FileType;
use clap::Parser;

// Bytes as KB, rounded up
fn bytes_to_kb(bytes: u64) -> u64 {
    (bytes + 1023) / 1024
}

// man 3type stat: st_blocks: Number of 512 B blocks allocated
const BLOCKSIZE: u64 = 512;

struct ItemError {
    file_type: FileType,
    file_name: OsString,
    error: String,
}

struct DirDiskUsage {
    path: PathBuf,
    file_bytes: u64, // sum of (block * blocksize) of files only
    subdirs: Vec<(OsString, Result<DirDiskUsage>)>,
    errors: Vec<ItemError>,
}

impl DirDiskUsage {
    // total bytes
    fn total(&self) -> u64 {
        self.file_bytes
            + self
                .subdirs
                .iter()
                .map(|(_, result)| -> u64 {
                    match result {
                        Ok(du) => du.total(),
                        Err(_) => 0,
                    }
                })
                .sum::<u64>()
    }

    // total in KB, rounded up (OK?)
    fn total_kb(&self) -> u64 {
        bytes_to_kb(self.total())
    }

    // files in KB, rounded up (OK?)
    fn files_kb(&self) -> u64 {
        bytes_to_kb(self.file_bytes)
    }

    // subdirs in KB, rounded up (OK?)
    fn dirs_kb(&self) -> u64 {
        let dirs_bytes = self
            .subdirs
            .iter()
            .map(|(_, result)| match result {
                Ok(du) => du.total(),
                Err(_) => 0,
            })
            .sum::<u64>();
        bytes_to_kb(dirs_bytes)
    }

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
        for (file_name, subdir) in &self.subdirs {
            if out.len() >= limit {
                return;
            }
            match subdir {
                Ok(du) => du.get_errors(limit, out),
                Err(error) => {
                    out.push(format!(
                        "subdir item {file_name:?} in {:?}: {error:#}",
                        self.path
                    ));
                }
            }
        }
    }
}

fn dir_disk_usage(
    path: PathBuf,
    current_dev: Option<u64>,
    one_file_system: bool,
) -> Result<DirDiskUsage> {
    let items = std::fs::read_dir(&path)
        .with_context(|| anyhow!("opening directory {path:?}"))?;
    let mut file_bytes = 0;
    let mut errors = vec![];
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

                    // Include counting the dirs too (the shells only):
                    file_bytes += blocks * blocksize;

                    if metadata.is_dir() {
                        let new_dev = metadata.st_dev();

                        let do_recurse = if let Some(current_dev) = current_dev
                        {
                            (!one_file_system) || new_dev == current_dev
                        } else {
                            true
                        };

                        if do_recurse {
                            let mut path = path.clone();
                            path.push(&file_name);
                            let subdirs = &subdirs;
                            scope.spawn(move |_| {
                                let result = dir_disk_usage(
                                    path,
                                    Some(new_dev),
                                    one_file_system,
                                );
                                subdirs
                                    .lock()
                                    .expect("no crash")
                                    .push((file_name, result));
                            });
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
        subdirs,
        errors,
    })
}

#[derive(clap::Parser, Debug)]
/// Show the space use of the immediate items (subdirectories, files)
/// in a directory.
struct Opts {
    /// Ignore other file system mounts
    #[clap(short = 'x', long)]
    one_file_system: bool,

    /// Path to the directory to show.
    #[clap(default_value = ".")]
    dir_path: PathBuf,
}

fn main() -> Result<()> {
    let Opts {
        one_file_system,
        dir_path,
    } = Opts::from_args();
    let du = dir_disk_usage(dir_path, None, one_file_system)?;

    let dirs_kb = du.dirs_kb();
    let files_kb = du.files_kb();
    let total_kb = du.total_kb();

    const ERRORS_LIMIT: usize = 11;
    let mut errors = vec![];
    du.get_errors(ERRORS_LIMIT, &mut errors);

    let mut subdirs: Vec<(u64, DirDiskUsage)> = du
        .subdirs
        .into_iter()
        .filter_map(|(_, du)| -> Option<_> {
            let du = du.ok()?;
            Some((du.total(), du))
        })
        .collect();

    subdirs.sort_by_key(|(total, _)| *total);

    let mut out = BufWriter::new(stdout().lock());
    let write_line =
        |kb: u64, filename: &[u8], out: &mut BufWriter<_>| -> Result<()> {
            let indent = "            ";
            let number = kb.to_string();
            let spacing = if let Some(indent_rest) =
                indent.len().checked_sub(number.len())
            {
                &indent[0..indent_rest]
            } else {
                " "
            };
            out.write_all(number.as_bytes())?;
            out.write_all(spacing.as_bytes())?;
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

    write_line(
        total_kb,
        format!("=== TOTAL: folders {dirs_kb} k, files {files_kb} k ===")
            .as_bytes(),
        &mut out,
    )?;

    if !errors.is_empty() {
        if errors.len() == ERRORS_LIMIT {
            errors.pop();
            errors.push("...".into());
        }
        writeln!(&mut out, "--- Ignored errors: ---")?;
        for error in &errors {
            writeln!(&mut out, "{error}")?;
        }
    }

    Ok(())
}
