// Same functionality as symlinks-index from
// https://github.com/pflanze/chj-bin/blob/master/symlinks-index

extern crate clap;
use anyhow::{Context, Result};
use clap::{App, Arg};
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::fs;
use std::io::{
    stdin, stdout, BufRead, BufWriter, Stdin, Stdout, StdoutLock, Write,
};
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::PathBuf;
use std::process::exit;

fn cleanup_target(path: PathBuf) -> PathBuf {
    // remove end slash (but only if it's not the '/' dir), and
    // slightly canonicalize
    path.components().as_path().to_path_buf()
}

fn dirs_index(
    debug: bool,
    dirs: &mut dyn Iterator<Item = &str>,
    remove_base: Option<OsString>,
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
                let target = cleanup_target(target);
                let fnam = item.file_name();
                if debug {
                    eprintln!("target = {:?}, fnam = {:?}", &target, &fnam)
                };
                match items_from_target.get_mut(&target) {
                    Some(vec) => vec.push(fnam),
                    None => {
                        items_from_target.insert(target, vec![fnam]);
                        ()
                    }
                }
            }
        }
    }
    Ok(items_from_target)
}

fn serve(
    _debug: bool,
    inp: Stdin,   // XX how to generalize?
    outp: Stdout, // XX
    items_from_target: HashMap<PathBuf, Vec<OsString>>,
    input_separator: u8,
    output_separator: u8,
) -> Result<()> {
    let mut inl = inp.lock();
    let mut outl = BufWriter::new(outp.lock());

    let write = |outl: &mut BufWriter<StdoutLock>, v| -> Result<()> {
        outl.write_all(v).with_context(|| "writing to output")
    };
    let flush = |outl: &mut BufWriter<StdoutLock>| -> Result<()> {
        outl.flush().with_context(|| "writing to output")
    };
    let sep = [output_separator];

    let mut item = Vec::new();
    loop {
        if inl.read_until(input_separator, &mut item)? == 0 {
            break;
        }
        match item.last() {
            Some(&c) => {
                if c == input_separator {
                    item.pop();
                    ()
                }
            }
            None => (),
        }
        let osstr = OsStr::from_bytes(&item);
        let pb = PathBuf::from(osstr);
        let b = cleanup_target(pb);
        match items_from_target.get(&b) {
            Some(vec) => {
                for item in vec {
                    write(&mut outl, item.as_os_str().as_bytes())?;
                    write(&mut outl, &sep)?;
                }
            }
            None => (),
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
                        len = len + 1;
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

fn main() {
    let app= App::new ("symlinks-index")
        .author("Christian Jaeger")
        .about("On startup creates an in-memory index of the symlinks \
                contained in $dirpath, then reads target-paths from stdin \
                and writes the source-paths that link to the target-path \
                to stdout. The end of an answer group is indicated by an \
                empty string written to stdout.  By default, entries are \
                terminated by newline characters.")
        .arg (Arg::with_name ("debug")
              .short ("d")
              .long ("debug")
              .multiple(true)
              .help ("show debugging output"))
        .arg (Arg::with_name ("null as write terminator")
              .short ("z")
              .help ("use the null byte as record terminator for writing"))
        .arg (Arg::with_name ("null as read+write terminator")
              .short ("0")
              .help ("use the null byte as record terminator for reading \
                      and writing"))
        .arg (Arg::with_name ("remove base")
              .long ("remove-base")
              .help ("remove base from the target paths before putting \
                      them into the index (simply substring it (for now))")
              .value_name("PATH"))
        .arg (Arg::with_name("directory-paths")
              .multiple(true));

    let args = app.get_matches();

    let debug = args.is_present("debug");
    let remove_base = match args.value_of("remove base") {
        Some(s) => Some(OsString::from(s)),
        None => None,
    };
    let opt_z = args.is_present("null as write terminator");
    let opt_0 = args.is_present("null as read+write terminator");
    let input_separator = if opt_0 { 0 } else { b'\n' };
    let output_separator = if opt_0 || opt_z { 0 } else { b'\n' };

    if let Some(mut dirpaths) = args.values_of("directory-paths") {
        match dirs_index(debug, &mut dirpaths, remove_base) {
            Ok(items_from_target) => {
                if debug {
                    eprintln!("items_from_target = {:?}", &items_from_target)
                };
                match serve(
                    debug,
                    stdin(),
                    stdout(),
                    items_from_target,
                    input_separator,
                    output_separator,
                ) {
                    Ok(()) => (),
                    Err(err) => {
                        eprintln!("Error serving pipe: {:#}", err);
                        exit(1);
                    }
                }
            }
            Err(err) => {
                eprintln!("Error indexing: {:#}", err);
                exit(1);
            }
        }
    } else {
        eprintln!("Missing directory-paths. Run with --help for help.");
        exit(1);
    }
}
