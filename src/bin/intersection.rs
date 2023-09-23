use anyhow::{Result, bail};
use structopt::StructOpt;
use std::fs::File;
use std::io::{BufReader, BufRead, stdout, Write};
use std::path::PathBuf;
use std::collections::{VecDeque, HashSet};
use kstring::KString;

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

fn println_stdout(line: &mut String) -> Result<()> {
    line.push_str("\n");
    stdout().write_all(line.as_bytes())?;
    Ok(())
}

fn trim(line: &mut String) {
    if line.ends_with("\n") {
        line.pop().unwrap();
    }
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
    let mut set: HashSet<KString> = HashSet::new();
    let mut tmpline = String::new();

    let first_path = paths.pop_front().unwrap();
    {
        let path = first_path;
        let mut inp = BufReader::new(File::open(path)?);
        while inp.read_line(&mut tmpline)? != 0  {
            trim(&mut tmpline);
            set.insert(KString::from(&tmpline));
            tmpline.clear();
        }
    }

    let last_path = if opt.set { None } else { Some(paths.pop_back().unwrap()) };
    
    for path in paths {
        let mut inp = BufReader::new(File::open(path)?);
        let mut newset = HashSet::new();
        while inp.read_line(&mut tmpline)? != 0  {
            trim(&mut tmpline);
            let line = KString::from(&tmpline);
            if set.contains(&line) {
                newset.insert(line);
            }
            tmpline.clear();
        }
        set = newset;
    }

    if opt.set {
        let mut v: Vec<KString> = set.into_iter().collect();
        v.sort();
        for line in v {
            // (XX optim: could we fill tmpline from a KString?)
            println_stdout(&mut line.to_string())?;
        }
    } else {
        let path = last_path.unwrap();
        let mut inp = BufReader::new(File::open(path)?);
        while inp.read_line(&mut tmpline)? != 0  {
            trim(&mut tmpline);
            if set.contains(&KString::from(&tmpline)) {
                println_stdout(&mut tmpline)?;
            }
            tmpline.clear();
        }
    }
    
    Ok(())
}
