use std::path::PathBuf;

use anyhow::Result;
use chj_rustbin::scripting::linux::config::Config;
use clap::Parser;
use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

#[derive(clap::Parser, Debug)]
#[clap(next_line_help = true)]
/// Wrapper around the `eog` (Eyes Of Gnome?) image viewer to show a number of
/// files in the given order instead of sorting by path
struct Args {
    /// Do not show entries that are new or disappeared when they were or are not set
    #[clap(short, long)]
    less: bool,

    /// Left-hand file path
    file1: PathBuf,

    /// Right-hand file path
    file2: PathBuf,
}

fn main() -> Result<()> {
    let Args { less, file1, file2 } = Args::parse();

    let config1 = Config::from_path(&file1)?;
    let config2 = Config::from_path(&file2)?;

    let differences = config1.diff(&config2);

    // dbg!(differences);

    for difference in differences {
        if !(less && difference.diff.is_not_set_new_or_disappeared()) {
            println!("{difference}");
        }
    }

    Ok(())
}
