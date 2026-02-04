use std::{
    cmp::Ordering,
    io::{stdout, BufWriter, Write},
    os::{unix::fs::MetadataExt, unix::prelude::OsStrExt},
    path::PathBuf,
    process::exit,
};

use anyhow::{anyhow, bail, Context, Result};
use chj_rustbin::{
    duu::{bytes_to_kb, to_human_readable, DirDiskUsage, GetDirDiskUsage},
    get_terminal_width::get_terminal_width,
};
use clap::Parser;

#[derive(clap::Parser, Debug)]
/// Show the space use of the immediate items (subdirectories, files)
/// in a directory.
#[clap(name = "duu")]
#[clap(next_line_help = true)]
#[clap(set_term_width = get_terminal_width())]
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

    /// Show total only
    #[clap(long, short)]
    sum: bool,

    /// Print sizes in human-readable format like `du`
    #[clap(long, short)]
    human_readable: bool,

    /// Like -h, but use powers of 1000 not 1024
    #[clap(long)]
    si: bool,

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
        sum,
        human_readable,
        si,
    } = Opts::from_args();

    let powers = if si {
        Some(1000)
    } else if human_readable {
        Some(1024)
    } else {
        None
    };

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
    let du = gdu.dir_disk_usage(dir_path, top_metadata.dev())?;

    let shared_inodes = gdu.shared_inodes.lock().expect("no crash");

    let mut out = BufWriter::new(stdout().lock());

    const ERRORS_LIMIT: usize = 10;
    let mut errors = vec![];
    du.get_errors(usize::MAX, &mut errors);

    // possibly human-readable
    let ph = |val| {
        if let Some(powers) = &powers {
            let (val, unit) = to_human_readable(*powers, si, val);
            format!("{val}{unit}")
        } else {
            format!("{}", bytes_to_kb(val))
        }
    };

    let total_string = ph(du.total(&*shared_inodes));

    if sum {
        writeln!(&mut out, "{total_string}")?;
    } else {
        let dirs_string = ph(du.total_subdirs(&*shared_inodes));
        let files_string = ph(du.total_files(&*shared_inodes));

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

        let write_line =
        // Takes filename as bytes to allow for 'correct'
        // representation of non-UTF8 filenames (although, no control
        // of newline characters is done!)
        |number: String, filename: &[u8], out: &mut BufWriter<_>| -> Result<()> {
            let indent = "            ";
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
            let number = ph(*total);
            let filename =
                subdir.path.file_name().expect("subdir does have filename");
            write_line(number, filename.as_bytes(), &mut out)?;
        }

        out.write_all(
            "-----------------------------------------------------------------\n"
            .as_bytes(),
        )?;

        write_line(dirs_string, "folders".as_bytes(), &mut out)?;
        write_line(files_string, "files".as_bytes(), &mut out)?;
        write_line(total_string, "total".as_bytes(), &mut out)?;
    }

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
