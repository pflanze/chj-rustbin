// Same functionality as symlinks-index from
// https://github.com/pflanze/chj-bin/blob/master/symlinks-index

#[macro_use]
extern crate log;
extern crate stderrlog;

use anyhow::{Context, Result};
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::fs;
use std::io::{stdin, stdout, BufRead, BufWriter, Write};
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::PathBuf;
use structopt::StructOpt;

fn cleanup_target(path: &PathBuf) -> PathBuf {
    // remove end slash (but only if it's not the '/' dir), and
    // slightly canonicalize
    path.components().as_path().to_path_buf()
}

fn dirs_index(
    dirs: &[PathBuf],
    remove_base: &Option<OsString>,
) -> Result<HashMap<PathBuf, Vec<OsString>>> {
    let mut items_from_target: HashMap<PathBuf, Vec<OsString>> = HashMap::new();

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
                    Some(ref remove_base) => {
                        let targetos = target.as_os_str();
                        if let Some(pos) =
                            targetos.starts_with_to_pos(remove_base)
                        {
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
                if let Some(vec) = items_from_target.get_mut(&target) {
                    vec.push(fnam);
                } else {
                    items_from_target.insert(target, vec![fnam]);
                }
            }
        }
    }
    Ok(items_from_target)
}

fn serve(
    inl: &mut dyn BufRead,
    mut outl: &mut dyn Write,
    items_from_target: &HashMap<PathBuf, Vec<OsString>>,
    input_separator: u8,
    output_separator: u8,
) -> Result<()> {
    let write = |outl: &mut dyn Write, v| -> Result<()> {
        outl.write_all(v).with_context(|| "writing to output")
    };
    let flush = |outl: &mut dyn Write| -> Result<()> {
        outl.flush().with_context(|| "writing to output")
    };
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
        if let Some(vec) = items_from_target.get(&b) {
            for item in vec {
                write(&mut outl, item.as_os_str().as_bytes())?;
                write(&mut outl, &sep)?;
            }
        }
        write(&mut outl, &sep)?;
        flush(&mut outl)?;
        item.clear();
    }

    Ok(())
}

trait StartsWith<T> {
    fn starts_with_to_pos(&self, start: T) -> Option<usize>;
    fn starts_with(&self, start: T) -> bool;
}

impl StartsWith<&OsString> for OsStr {
    fn starts_with_to_pos(&self, start: &OsString) -> Option<usize> {
        let mut ia = self.as_bytes().iter();
        let mut ib = start.as_bytes().iter();
        let mut len: usize = 0;
        loop {
            if let Some(b) = ib.next() {
                if let Some(a) = ia.next() {
                    if a != b {
                        return None;
                    } else {
                        len += 1;
                    }
                } else {
                    return None;
                }
            } else {
                return Some(len);
            }
        }
    }
    fn starts_with(&self, start: &OsString) -> bool {
        self.starts_with_to_pos(start).is_some()
    }
}

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

    let items_from_target =
        dirs_index(&dirpaths, &remove_base).with_context(|| "indexing")?;

    trace!("items_from_target = {:?}", &items_from_target);

    let inp = stdin();
    let mut inl = inp.lock();
    let outp = stdout();
    let mut outl = BufWriter::new(outp.lock());

    serve(
        &mut inl,
        &mut outl,
        &items_from_target,
        input_separator,
        output_separator,
    )
    .with_context(|| "serving pipe")
}
