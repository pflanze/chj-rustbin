use std::{path::PathBuf, convert::TryInto};

use anyhow::Result;
use clap::Parser;


#[derive(clap::Parser, Debug)]
/// Parse a folder with todo files with "OPEN.." markers.
struct Opts {
    // #[clap(long)]

    /// The base directory holding the todo files.
    directory: Option<PathBuf>
}



fn main() -> Result<()> {
    let opts: Opts = Opts::from_args();

    let directory = opts.directory.unwrap_or_else(|| ".".try_into().unwrap());
    // let directory = opts.directory.unwrap_or_default();

    let r = std::fs::read_dir(&directory);
    for path in r {
        dbg!(path);
    }
    
    
    Ok(())
}
