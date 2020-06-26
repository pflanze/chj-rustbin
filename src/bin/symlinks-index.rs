// Same functionality as symlinks-index from
// https://github.com/pflanze/chj-bin/blob/master/symlinks-index

#[macro_use]
extern crate log;
extern crate stderrlog;
#[path = "../startswith.rs"]
mod startswith;

use anyhow::{Context, Result};
use startswith::StartsWith;
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::fs;
use std::io::{stdin, stdout, BufRead, BufWriter, Write};
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
/// On startup creates an in-memory index of the symlinks
/// contained in specified directories, then repeatedly
/// reads a target-path from stdin and writes the source-paths
/// that link to the target-path to stdout. The end of an answer
/// group is indicated by an empty string. Entries are
/// terminated by newline characters by default, but see -z
/// and -0.
///
/// See the `coproc` feature in Bash for how to integrate this
/// into scripts.
#[structopt(name = "symlinks-index from chj-rustbin")]
struct Opt {
    /// show debugging output
    #[structopt(short, long, parse(from_occurrences))]
    debug: u8,
    /// use the null byte as record terminator for writing
    #[structopt(short, long)]
    z: bool,
    /// use the null byte as record terminator for reading
    /// and writing
    #[structopt(long)]
    zz: bool,
    /// remove base from the target paths before putting
    /// them into the index (simply substring it (for now))
    #[structopt(long, parse(from_os_str))]
    remove_base: Option<PathBuf>,
    /// paths to one or more directories to be scanned for symlinks
    /// to index
    #[structopt(name = "DIR", parse(from_os_str), required(true))]
    directory_paths: Vec<PathBuf>,
}

fn cleanup_target(path: &PathBuf) -> PathBuf {
    // remove end slash (but only if it's not the '/' dir), and
    // slightly canonicalize
    path.components().as_path().to_path_buf()
}

fn dirs_index(
    dirs: &[PathBuf],
    remove_base: Option<&OsStr>,
) -> Result<HashMap<PathBuf, Vec<OsString>>> {
    let mut target_to_items: HashMap<PathBuf, Vec<OsString>> = HashMap::new();

    for path in dirs {
        for item in fs::read_dir(path)
            .with_context(|| format!("opening directory {:?}", path))?
        {
            let item =
                item.with_context(|| format!("reading directory {:?}", path))?;
            let s = item.path().symlink_metadata()?;
            if s.file_type().is_symlink() {
                let target: PathBuf = item.path().read_link()?;
                let target = match remove_base {
                    Some(ref base) => {
                        let targetos = target.as_os_str();
                        let mut ia = targetos.as_bytes().iter();
                        let mut ib = base.as_bytes().iter();
                        if let Some(pos) = ia.starts_with(&mut ib) {
                            let bs = targetos.as_bytes();
                            let nbs = Vec::from(&bs[pos..bs.len()]);
                            let os = OsString::from_vec(nbs);
                            PathBuf::from(os)
                        } else {
                            target
                        }
                    }
                    None => target,
                };
                let target = cleanup_target(&target);
                let fnam = item.file_name();
                trace!("target = {:?}, fnam = {:?}", &target, &fnam);
                if let Some(vec) = target_to_items.get_mut(&target) {
                    vec.push(fnam);
                } else {
                    target_to_items.insert(target, vec![fnam]);
                }
            }
        }
    }
    Ok(target_to_items)
}

fn serve(
    inl: &mut dyn BufRead,
    outl: &mut dyn Write,
    target_to_items: &HashMap<PathBuf, Vec<OsString>>,
    input_separator: u8,
    output_separator: u8,
) -> Result<()> {
    let sep = [output_separator];

    let mut item = Vec::new();
    while inl.read_until(input_separator, &mut item)? > 0 {
        if let Some(&c) = item.last() {
            if c == input_separator {
                item.pop();
            }
        }
        let osstr = OsStr::from_bytes(&item);
        let pb = PathBuf::from(osstr);
        let b = cleanup_target(&pb);
        let mut write = |v| -> Result<()> {
            outl.write_all(v).with_context(|| "writing to output")
        };
        if let Some(vec) = target_to_items.get(&b) {
            for item in vec {
                write(item.as_os_str().as_bytes())?;
                write(&sep)?;
            }
        }
        write(&sep)?;
        let mut flush = || -> Result<()> {
            outl.flush().with_context(|| "writing to output")
        };
        flush()?;
        item.clear();
    }

    Ok(())
}

fn main() -> Result<()> {
    let opt = Opt::from_args();

    let debug = opt.debug;
    let remove_base = opt.remove_base.map(OsString::from);
    let input_separator = if opt.zz { 0 } else { b'\n' };
    let output_separator = if opt.zz || opt.z { 0 } else { b'\n' };
    let dirpaths = opt.directory_paths;

    stderrlog::new()
        .module(module_path!())
        .verbosity(if debug == 0 { 0 } else { 5 })
        .init()?;

    let target_to_items = dirs_index(&dirpaths, remove_base.as_deref())
        .with_context(|| "indexing")?;

    trace!("target_to_items = {:?}", &target_to_items);

    let inp = stdin();
    let mut inl = inp.lock();
    let outp = stdout();
    let mut outl = BufWriter::new(outp.lock());

    serve(
        &mut inl,
        &mut outl,
        &target_to_items,
        input_separator,
        output_separator,
    )
    .with_context(|| "serving pipe")
}
