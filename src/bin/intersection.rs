use anyhow::{Result, bail, Context};
use structopt::StructOpt;
use std::fs::File;
use std::io::{BufReader, BufRead, stdout, Write, BufWriter};
use std::os::unix::prelude::MetadataExt;
use std::path::{PathBuf, Path};
use std::collections::{VecDeque, BTreeSet};
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
    let mut set: BTreeSet<KString> = BTreeSet::new();
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
        while inp.read_line(&mut tmpline)? != 0  {
            trim(&mut tmpline);
            set.insert(KString::from(&tmpline));
            tmpline.clear();
        }
    }

    for (path, _) in paths_meta {
        if set.is_empty() {
            break
        }
        let mut inp = open_file(&path)?;
        let mut newset = BTreeSet::new();
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

    let mut out = BufWriter::new(stdout());
    if opt.set {
        let mut v: Vec<KString> = set.into_iter().collect();
        v.sort();
        for line in v {
            tmpline.push_str(&line);
            println(&mut out, &mut tmpline)?;
            tmpline.clear();
        }
    } else {
        let path = last_path.unwrap();
        let mut inp = open_file(&path)?;
        while inp.read_line(&mut tmpline)? != 0  {
            trim(&mut tmpline);
            if set.contains(&KString::from(&tmpline)) {
                println(&mut out, &mut tmpline)?;
            }
            tmpline.clear();
        }
    }
    out.flush()?;
    
    Ok(())
}
