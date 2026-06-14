use std::{
    path::{Path, PathBuf},
    str::FromStr,
    time::{Duration, SystemTime},
};

use anyhow::{anyhow, Context, Result};
use clap::Parser;

use chj_rustbin::mystring::MyString;

fn path_mtime(path: &Path) -> Result<u64> {
    (|| -> Result<_> {
        let m: std::fs::Metadata = path.metadata()?;
        let mtime: SystemTime = m.modified()?;
        let unix: u64 = mtime.duration_since(SystemTime::UNIX_EPOCH)?.as_secs();
        Ok(unix)
    })()
    .with_context(|| anyhow!("getting modification time of path {path:?}"))
}

#[derive(clap::Parser, Debug)]
struct FilterOpts {
    /// Mails must carry a delivery time newer than this unix time
    /// stamp
    #[clap(long)]
    newer_than_unixtime: Option<u64>,

    /// Mails must carry a delivery time newer than the modification
    /// time of the file at this path
    #[clap(long)]
    newer_than_file_path: Option<PathBuf>,
}

impl FilterOpts {
    /// Process the given options into a single time point; if both
    /// options were given, returns the newer time. Returns None if no
    /// option was given. Returns an error if a given file's path
    /// couldn't be used.
    fn newer_than_unixtime(&self) -> Result<Option<u64>> {
        match self {
            FilterOpts {
                newer_than_unixtime: Some(t),
                newer_than_file_path: None,
            } => Ok(Some(*t)),
            FilterOpts {
                newer_than_unixtime: None,
                newer_than_file_path: Some(path),
            } => path_mtime(path).map(Some),
            FilterOpts {
                newer_than_unixtime: Some(t),
                newer_than_file_path: Some(path),
            } => {
                let t1 = path_mtime(path)?;
                Ok(Some((*t).max(t1)))
            }
            FilterOpts {
                newer_than_unixtime: None,
                newer_than_file_path: None,
            } => Ok(None),
        }
    }
}

#[derive(clap::Parser, Debug)]
/// Print files in a Maildir that have a newer delivery time than a
/// given time.
#[clap(name = "find-new-mail from chj-rustbin")]
struct Opt {
    /// Poll until there are files to be listed
    #[clap(long)]
    poll: bool,

    /// How long to sleep between polls in seconds (float)
    #[clap(long, default_value = "3")]
    sleep_time: f64,

    /// How long to poll in total in seconds (float) before exiting
    /// (default: no limit)
    #[clap(long)]
    poll_total_time: Option<f64>,

    #[clap(flatten)]
    filter_opts: FilterOpts,

    /// Path to the directory with the incoming email
    dir: PathBuf,
}

fn check(
    dir: &Path,
    filter_opts: &FilterOpts,
) -> Result<Vec<(u64, MyString<23>)>> {
    let newer_than_time = filter_opts.newer_than_unixtime()?;
    let dir = std::fs::read_dir(&dir)?;
    let mut items = Vec::new();
    for item in dir {
        let item = item?;
        let name = item.file_name();
        if let Ok(name) = name.into_string() {
            // 1622820231.16542.foo
            if let Some((timestr, _)) = name.split_once('.') {
                if let Ok(time) = u64::from_str(timestr) {
                    if let Some(min_time) = newer_than_time {
                        if time > min_time {
                            items.push((time, name.into()));
                        }
                    } else {
                        // Always print
                        items.push((time, name.into()));
                    }
                } else {
                    // warn?
                }
            } else {
                // warn?
            }
        } else {
            // warn?
        }
    }
    Ok(items)
}

fn main() -> Result<()> {
    let opt: Opt = Opt::parse();

    let mut found = if opt.poll {
        if let Some(found) = (|| -> Result<Option<Vec<_>>> {
            let start = SystemTime::now();
            loop {
                let found = check(&opt.dir, &opt.filter_opts)?;
                if !found.is_empty() {
                    return Ok(Some(found));
                }
                if let Some(poll_total_time) = opt.poll_total_time {
                    let dur = SystemTime::now().duration_since(start)?;
                    if dur.as_secs_f64() > poll_total_time {
                        return Ok(None);
                    }
                }
                std::thread::sleep(Duration::from_secs_f64(opt.sleep_time));
            }
        })()
        .transpose()
        {
            found
        } else {
            return Ok(());
        }
    } else {
        check(&opt.dir, &opt.filter_opts)
    }
    .with_context(|| anyhow!("reading directory {:?}", opt.dir))?;

    found.sort();

    for item in found {
        println!("{}", item.1);
    }

    Ok(())
}
