use anyhow::{Result, bail, Context, Error};
use structopt::StructOpt;
use thiserror::Error;
use std::cmp::Ordering;
use std::fs::File;
use std::io::{BufReader, BufRead, stdout, Write, BufWriter};
use std::os::unix::prelude::MetadataExt;
use std::path::{PathBuf, Path};
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
    /// Show the set, not the filtered last file (i.e. there will be
    /// no repetitions in the output, and it is lexically sorted)
    #[structopt(long)]
    set: bool,

    /// Assume that the input files are lexically sorted (uses a
    /// streaming implementation).
    #[structopt(long)]
    sorted: bool,

    /// The paths to files to get the intersection of
    #[structopt(parse(from_os_str))]
    file_paths: Vec<PathBuf>,
}

fn println(out: &mut impl Write, line: &mut String) -> Result<()> {
    line.push_str("\n");
    out.write_all(line.as_bytes())?;
    Ok(())
}

fn trim(line: &mut String) {
    if line.ends_with("\n") {
        line.pop().unwrap();
    }
}

fn open_file(path: &Path) -> Result<BufReader<File>> {
    Ok(BufReader::new(File::open(path).with_context(
        || format!("opening file {:?}", path))?))
}

// "Clean" read_line function: returns true if it did read a line,
// false on EOF. Does overwrite `line`, not append to it. Removes
// trailing '\n' if present.
fn easy_read_line(inp: &mut BufReader<File>, line: &mut String) -> Result<bool> {
    line.clear();
    if inp.read_line(line)? != 0 {
        trim(line);
        Ok(true)
    } else {
        Ok(false)
    }
}

#[derive(Error, Debug)]
enum Signal {
    #[error("Signal: Finished")]
    Finished,
    #[error("Signal: Error({0:?})")]
    Error(Error)
}

fn main() -> Result<()> {
    let opt : Opt = Opt::from_args();
    let mut paths: VecDeque<PathBuf> = opt.file_paths.into();
    if opt.set || opt.sorted {
        if paths.len() < 1 {
            bail!("Need at least 1 input file in --set and --sorted modes");
        }
    } else {
        if paths.len() < 2 {
            bail!("Need at least 2 input files in default mode");
        }
    }

    if opt.set && opt.sorted {
        bail!("Only one of --set or --sorted is valid");
    }

    if opt.sorted {
        match
            paths.iter().map(|p| {
                let mut inp = open_file(p).map_err(Signal::Error)?;
                let mut line = String::new();
                if easy_read_line(&mut inp, &mut line).map_err(Signal::Error)? {
                    Ok((inp, line))
                } else {
                    Err(Signal::Finished)
                }
            }).collect::<Result<Vec<(BufReader<File>, String)>, Signal>>()
        {
            Ok(mut inputs) => {
                let mut out = BufWriter::new(stdout());

                'full: loop {
                    // Get the largest value
                    inputs.sort_unstable_by(|a, b| b.1.cmp(&a.1));
                    let ((_, largest), rest) = inputs.split_first_mut().unwrap();
                    // Are the other ones the same, or can we update
                    // them to the same value?
                    let mut all_same = true;
                    for (input, line) in rest {
                        'this_input: loop {
                            match line.cmp(&largest) {
                                Ordering::Equal => {
                                    break 'this_input;
                                }
                                Ordering::Greater => {
                                    all_same = false;
                                    break 'this_input;
                                }
                                Ordering::Less => {
                                    if ! easy_read_line(input, line)? {
                                        break 'full;
                                    }
                                }
                            }
                        }
                    }
                    if all_same {
                        println(&mut out, largest)?;
                    }
                }

                out.flush()?;
            }
            Err(Signal::Finished) => {
                // 1+ of the files are empty, we're done
            }
            Err(Signal::Error(e)) => {
                Err(e)?;
            }
        }
        
    } else {
        let mut set: HashSet<KString> = HashSet::new();
        let mut tmpline = String::new();

        let last_path = if opt.set { None } else { Some(paths.pop_back().unwrap()) };

        let mut paths_meta: VecDeque<(PathBuf, u64)> =
            paths.into_iter().map(
                |path| {
                    let s = path.metadata().with_context(
                        || format!("stat on file {:?}", path))?.size();
                    Ok((path, s))}
            )
            .collect::<Result<_>>()?;
        paths_meta.make_contiguous().sort_by_key(|x| x.1);

        let first_path = paths_meta.pop_front().unwrap().0;
        {
            let path = first_path;
            let mut inp = open_file(&path)?;
            while easy_read_line(&mut inp, &mut tmpline)? {
                set.insert(KString::from(&tmpline));
            }
        }

        for (path, _) in paths_meta {
            if set.is_empty() {
                break
            }
            let mut inp = open_file(&path)?;
            let mut newset = HashSet::new();
            while easy_read_line(&mut inp, &mut tmpline)? {
                let line = KString::from(&tmpline);
                if set.contains(&line) {
                    newset.insert(line);
                }
            }
            set = newset;
        }

        let mut out = BufWriter::new(stdout());
        if opt.set {
            let mut v: Vec<KString> = set.into_iter().collect();
            v.sort();
            for line in v {
                tmpline.clear();
                tmpline.push_str(&line);
                println(&mut out, &mut tmpline)?;
            }
        } else {
            let path = last_path.unwrap();
            let mut inp = open_file(&path)?;
            while easy_read_line(&mut inp, &mut tmpline)? {
                if set.contains(&KString::from(&tmpline)) {
                    println(&mut out, &mut tmpline)?;
                }
            }
        }
        out.flush()?;
    }
    
    Ok(())
}
