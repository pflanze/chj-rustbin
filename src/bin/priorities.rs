use std::{path::PathBuf, convert::TryInto, time::Instant, collections::HashSet};

use anyhow::Result;
use clap::Parser;


#[derive(clap::Parser, Debug)]
/// Parse a folder with todo files with "OPEN.." markers.
struct Opts {
    // #[clap(long)]

    /// The base directory holding the todo files.
    directory: Option<PathBuf>
}

struct Priority {
    
}

struct OpenInfo<'t> {
    path: PathBuf,
    mtime: Instant,
    priority: Priority,
    dependencies: Vec<&'t OpenInfo<'t>>,
}

fn parse_path(path: PathBuf) {
    
}


fn main() -> Result<()> {
    let opts: Opts = Opts::from_args();

    let directory = opts.directory.unwrap_or_else(|| ".".try_into().unwrap());
    // let directory = opts.directory.unwrap_or_default();

    for item in std::fs::read_dir(&directory)? {
        let item = item?;
        let ft = item.file_type()?;
        if ft.is_file() {
            let path = item.path();
            
            path.file_name()
            dbg!((path, ft));

        }
        // XX: if it's a symlink, check if it has different OPEN info?

    }
    
    
    Ok(())
}
