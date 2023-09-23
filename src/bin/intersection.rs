use anyhow::{Result, bail};
use structopt::StructOpt;
use std::fs::File;
use std::io::{BufReader, BufRead, stdout, Write};
use std::path::PathBuf;
use std::collections::{VecDeque, HashSet};

#[derive(StructOpt, Debug)]
/// Print the lines that occur in all input files. Currently, reads
/// the first file into an index in memory. By default, the order of
/// the output lines follows the last file, and if there are
/// repetitions in the last file, those are repeated, too.

/// (Todo: sorted intersection mode.)
#[structopt(name = "intersection from chj-rustbin")]
struct Opt {
    /// Show the set, not the filtered last file (i.e. there are no
    /// repetitions in the output, and the ordering is sorted)
    #[structopt(long)]
    set: bool,

    /// The paths to files to get the intersection of
    #[structopt(parse(from_os_str))]
    file_paths: Vec<PathBuf>,
}

fn println_stdout(mut line: String) -> Result<()> {
    line.push_str("\n");
    stdout().write_all(line.as_bytes())?;
    Ok(())
}

fn main() -> Result<()> {
    let opt : Opt = Opt::from_args();
    let mut paths: VecDeque<PathBuf> = opt.file_paths.into();
    if opt.set {
        if paths.len() < 1 {
            bail!("Need at least 1 input file in --set mode");
        }
    } else {
        if paths.len() < 2 {
            bail!("Need at least 2 input files in default mode");
        }
    }
    let mut set = HashSet::new();

    let first_path = paths.pop_front().unwrap();
    {
        let path = first_path;
        let inp = BufReader::new(File::open(path)?);
        for line in inp.lines() {
            set.insert(line?);
        }
    }

    let last_path = if opt.set { None } else { Some(paths.pop_back().unwrap()) };
    
    for path in paths {
        let inp = BufReader::new(File::open(path)?);
        let mut newset = HashSet::new();
        for line in inp.lines() {
            let line = line?;
            if set.contains(&line) {
                newset.insert(line);
            }
        }
        set = newset;
    }

    if opt.set {
        let mut v: Vec<String> = set.into_iter().collect();
        v.sort();
        for line in v {
            println_stdout(line)?;
        }
    } else {
        let path = last_path.unwrap();
        let inp = BufReader::new(File::open(path)?);
        for line in inp.lines() {
            let line = line?;
            if set.contains(&line) {
                println_stdout(line)?;
            }
        }
    }
    
    Ok(())
}
