use std::{
    ffi::OsStr,
    io::{stdin, stdout, BufWriter, Read, Write},
    os::unix::prelude::OsStrExt,
    path::Path,
    time::SystemTime,
};

use anyhow::{bail, Context, Result};
use clap::Parser;

#[derive(clap::Parser, Debug)]
/// Read paths from stdin, print them sorted by time.
#[clap(name = "lrt from chj-rustbin")]
struct Opt {
    /// Expect inputs trailed by zero-bytes
    #[clap(short, long)]
    z: bool,

    /// Print output records separated by zero-bytes
    #[clap(long)]
    zo: bool,
}

fn chomp(v: &mut Vec<u8>, record_separator: u8) {
    if let Some(last) = v.last() {
        if *last == record_separator {
            v.pop();
        }
    }
}

struct Item<'t> {
    path: &'t Path,
    mtime: SystemTime,
}

fn main() -> Result<()> {
    let Opt { z, zo } = Opt::from_args();
    let input_record_separator = if z { 0 } else { b'\n' };
    let output_record_separator = if zo { 0 } else { b'\n' };

    let mut all_entries = Vec::new();
    stdin()
        .lock()
        .read_to_end(&mut all_entries)
        .context("reading from stdin")?;
    chomp(&mut all_entries, input_record_separator);
    let mut items: Vec<Item> = all_entries
        .split(|c| *c == input_record_separator)
        .map(|path| -> Result<Option<Item>> {
            let path: &OsStr = OsStr::from_bytes(path);
            let path: &Path = path.as_ref();
            match path.symlink_metadata() {
                Ok(s) => {
                    let mtime = s.modified()?;
                    Ok(Some(Item { path, mtime }))
                }
                Err(e) => match e.kind() {
                    std::io::ErrorKind::NotFound => Ok(None),
                    _ => bail!("getting metadata for {path:?}: {e:#}"),
                },
            }
        })
        .filter_map(|r| r.transpose())
        .collect::<Result<Vec<Item>>>()?;
    items.sort_by(|a, b| a.mtime.cmp(&b.mtime));

    let mut outp = BufWriter::new(stdout().lock());
    (|| -> Result<()> {
        for item in items {
            outp.write_all(item.path.as_os_str().as_bytes())?;
            outp.write_all(&[output_record_separator])?;
        }
        outp.flush()?;
        Ok(())
    })()
    .context("writing to stdout")?;

    Ok(())
}
