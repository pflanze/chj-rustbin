use std::{
    ffi::OsStr,
    io::{stdin, stdout, BufWriter, Read, Write},
    os::unix::prelude::OsStrExt,
    path::Path,
    time::SystemTime,
};

use anyhow::{bail, Context, Result};
use clap::Parser;
use rayon::prelude::{ParallelBridge, ParallelIterator};

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

    /// Sort in the reverse, show newest items first
    #[clap(long)]
    reverse: bool,

    /// Skip up to n items from the top
    #[clap(long)]
    skip: Option<usize>,

    /// Print only up to the n bottom items (after skip)
    #[clap(long)]
    head: Option<usize>,

    /// Print only up to the n bottom items (after head)
    #[clap(long)]
    tail: Option<usize>,

    /// Reverse after processing skip, head and tail
    #[clap(long)]
    reverse_after: bool,
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
    let Opt {
        z,
        zo,
        reverse,
        skip,
        head,
        tail,
        reverse_after,
    } = Opt::from_args();
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
        .par_bridge()
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

    if reverse {
        items.sort_by(|b, a| {
            a.mtime.cmp(&b.mtime).then_with(|| a.path.cmp(&b.path))
        });
    } else {
        items.sort_by(|a, b| {
            a.mtime.cmp(&b.mtime).then_with(|| a.path.cmp(&b.path))
        });
    }

    let mut selected_items = &mut *items;
    if let Some(skip) = skip {
        let n = selected_items.len().min(skip);
        selected_items = &mut selected_items[n..]
    }
    if let Some(head) = head {
        let n = selected_items.len().min(head);
        selected_items = &mut selected_items[..n]
    }
    if let Some(tail) = tail {
        let n = selected_items.len().saturating_sub(tail);
        selected_items = &mut selected_items[n..]
    }

    #[allow(unused)]
    let items = ();
    if reverse_after {
        selected_items.reverse()
    }

    let mut outp = BufWriter::new(stdout().lock());
    (|| -> Result<()> {
        for item in selected_items {
            outp.write_all(item.path.as_os_str().as_bytes())?;
            outp.write_all(&[output_record_separator])?;
        }
        outp.flush()?;
        Ok(())
    })()
    .context("writing to stdout")?;

    Ok(())
}
