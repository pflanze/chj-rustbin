use std::{path::PathBuf, convert::TryInto, time::Instant, collections::HashSet, sync::Arc};

use anyhow::{Result, Context, anyhow};
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

struct OpenInfo {
    path: Arc<PathBuf>,
    mtime: Instant,
    priority: Priority,
    dependencies: Vec<Arc<PathBuf>>,
}


/// Look for the first occurrence of `needle` in `haystack`, return
/// the rest after it.
fn str_match<'t>(haystack: &'t str, needle: &str) -> Option<&'t str> {
    let pos = haystack.find(needle)?;
    Some(&haystack[pos + needle.len()..])
}

fn parse_path(path: PathBuf) -> Result<Option<OpenInfo>> {
    let file_name_os = path.file_name().expect(
        "filename always present in path since we iterate over the dir and filter files");
    let file_name = file_name_os.to_str().ok_or_else(
        || anyhow!("the file name in path {path:?} is not valid UTF-8"))?;
    
    if let Some(rest) = str_match(file_name, "OPEN") {
        dbg!((file_name, rest));
        Ok(None)
    } else {
        Ok(None)
    }
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
            parse_path(path);

        }
        // XX: if it's a symlink, check if it has different OPEN info?

    }
    
    
    Ok(())
}
