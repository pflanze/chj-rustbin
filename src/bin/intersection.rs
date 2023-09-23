use anyhow::{Result, bail};
use structopt::StructOpt;
use std::fs::File;
use std::io::{BufReader, BufRead, stdout, Write};
use std::path::PathBuf;
use std::collections::{VecDeque, HashMap};

#[derive(StructOpt, Debug)]
/// Print the lines that occur in all input files. Currently, reads
/// the first file into an index in memory. The order of the output
/// lines follows the last file, and if there are repetitions in the
/// last file, those are repeated, too. (Todo: sorted intersection
/// mode.)
#[structopt(name = "intersection from chj-rustbin")]
struct Opt {
    /// The paths to files to get the intersection of
    #[structopt(parse(from_os_str))]
    file_paths: Vec<PathBuf>,
}

fn main() -> Result<()> {
    let opt : Opt = Opt::from_args();
    let mut paths: VecDeque<PathBuf> = opt.file_paths.into();
    if paths.len() < 2 {
        bail!("Need at least 2 input files");
    }
    let mut set = HashMap::new(); // line => bool, true == seen in other file too
    let mut to_delete = Vec::new(); // temporary for marking removals (sigh)

    let last_path = paths.pop_back().unwrap();
    let first_path = paths.pop_front().unwrap();

    {
        let path = first_path;
        let inp = BufReader::new(File::open(path)?);
        for line in inp.lines() {
            set.insert(line?, false);
        }
    }
    for path in paths {
        let inp = BufReader::new(File::open(path)?);
        for line in inp.lines() {
            let line = line?;
            if let Some(b) = set.get_mut(&line) {
                *b = true;
            }
        }
        for (key, seen) in set.iter_mut() {
            if !*seen {
                to_delete.push(key.clone());
            }
        }
        for key in to_delete.iter() {
            set.remove(key);
        }
        to_delete.clear();
    }

    {
        let path = last_path;
        let inp = BufReader::new(File::open(path)?);
        for line in inp.lines() {
            let mut line = line?;
            if set.contains_key(&line) {
                line.push_str("\n");
                stdout().write_all(line.as_bytes())?;
            }
        }
    }
    
    Ok(())
}
