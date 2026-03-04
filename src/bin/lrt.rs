use std::{
    ffi::OsStr,
    io::{stdin, stdout, BufWriter, Read, Write},
    os::unix::prelude::OsStrExt,
    path::Path,
    str::FromStr,
    time::SystemTime,
};

use anyhow::{anyhow, bail, Context, Result};
use clap::Parser;
use rayon::prelude::{ParallelBridge, ParallelIterator};

#[derive(clap::Parser, Debug)]
/// Read paths from stdin, print them sorted by time.
#[clap(name = "lrt from chj-rustbin")]
struct Opt {
    /// Say what is done
    #[clap(short, long)]
    verbose: bool,

    /// Expect inputs trailed by zero-bytes
    #[clap(short, long)]
    z: bool,

    /// Print output records separated by zero-bytes
    #[clap(long)]
    zo: bool,

    /// Sort in the reverse, show newest items first. This is more
    /// efficient than using default sort order and then the `reverse`
    /// processing command.
    #[clap(long)]
    reverse: bool,

    /// Disable the optimizer for processing commands (in case there
    /// are bugs in it!)
    #[clap(long)]
    no_optimize: bool,

    /// Any number of post-processing commands after initial sorting:
    /// `skip n` skips up to n items from the top, `head n` takes up
    /// to the n top items, `tail n` takes up to the n bottom items,
    /// `reverse` reverses the items in the selection, `filter-days
    /// n|..n|n..|from..to` filters mtime age in rounded days.
    processing_commands: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum IntRange {
    At(u16),
    RangeFrom(u16),
    RangeTo(u16),
    RangeInclusive(u16, u16),
}

impl FromStr for IntRange {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some((from, to)) = s.split_once("..") {
            if from.is_empty() {
                Ok(IntRange::RangeTo(to.parse()?))
            } else if to.is_empty() {
                Ok(IntRange::RangeFrom(from.parse()?))
            } else {
                Ok(IntRange::RangeInclusive(from.parse()?, to.parse()?))
            }
        } else {
            Ok(IntRange::At(s.parse()?))
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum ProcessingCommand {
    Skip(usize),
    Head(usize),
    Tail(usize),
    FilterDays(IntRange),
    Reverse,
}

fn parse_processing_commands(
    processing_commands: &[String],
) -> Result<Vec<ProcessingCommand>> {
    let mut cmds = Vec::new();
    let mut i = 0;
    while i < processing_commands.len() {
        let cmd_str: &str = processing_commands[i].as_ref();
        i += 1;
        let parse_usize = |i: &mut usize| -> Result<usize> {
            if *i < processing_commands.len() {
                let arg_str: &str = processing_commands[*i].as_ref();
                *i += 1;
                Ok(arg_str.parse().with_context(|| anyhow!(
                    "expecting number argument after processing command {cmd_str:?}, \
                     got {arg_str:?}"))?)
            } else {
                bail!("missing number argument after processing command {cmd_str:?}")
            }
        };
        let parse_range = |i: &mut usize| -> Result<IntRange> {
            if *i < processing_commands.len() {
                let arg_str: &str = processing_commands[*i].as_ref();
                *i += 1;
                Ok(arg_str.parse().with_context(|| anyhow!(
                    "expecting number/range argument after processing command {cmd_str:?}, \
                     got {arg_str:?}"))?)
            } else {
                bail!("missing number argument after processing command {cmd_str:?}")
            }
        };
        let cmd = match cmd_str {
            "skip" => ProcessingCommand::Skip(parse_usize(&mut i)?),
            "head" => ProcessingCommand::Head(parse_usize(&mut i)?),
            "tail" => ProcessingCommand::Tail(parse_usize(&mut i)?),
            "reverse" => ProcessingCommand::Reverse,
            "filter-days" => {
                ProcessingCommand::FilterDays(parse_range(&mut i)?)
            }
            _ => bail!(
                "unknown processing command {cmd_str:?} -- \
                 valid are skip, head, tail, reverse, filter-days"
            ),
        };
        cmds.push(cmd);
    }
    Ok(cmds)
}

fn optimize_processing_commands(
    cmds: &[ProcessingCommand],
) -> Vec<ProcessingCommand> {
    let mut out = Vec::new();
    for cmd in cmds {
        match cmd {
            ProcessingCommand::Skip(_) => (),
            ProcessingCommand::Head(_) => (),
            ProcessingCommand::Tail(_) => (),
            ProcessingCommand::FilterDays(_) => (),
            ProcessingCommand::Reverse => {
                if let Some(last) = out.last() {
                    if *last == ProcessingCommand::Reverse {
                        out.pop();
                        continue;
                    }
                }
            }
        }
        out.push(cmd.clone());
    }
    out
}

fn chomp(v: &mut Vec<u8>, record_separator: u8) {
    if let Some(last) = v.last() {
        if *last == record_separator {
            v.pop();
        }
    }
}

#[derive(Clone)]
struct Item<'t> {
    path: &'t Path,
    mtime: SystemTime,
}

impl<'t> Item<'t> {
    fn age_secs(&self, now: SystemTime) -> Result<u64> {
        let age = now.duration_since(self.mtime)?;
        Ok(age.as_secs())
    }

    fn age_days(&self, now: SystemTime) -> Result<u64> {
        Ok((self.age_secs(now)? + 12 * 3600) / (24 * 3600))
    }
}

fn main() -> Result<()> {
    let Opt {
        verbose,
        z,
        zo,
        reverse,
        no_optimize,
        processing_commands,
    } = Opt::from_args();

    let orig_cmds = parse_processing_commands(&processing_commands)?;
    let cmds = if no_optimize {
        orig_cmds
    } else {
        let cmds = optimize_processing_commands(&orig_cmds);
        if verbose {
            eprintln!("original:\n{orig_cmds:?}\noptimized:\n{cmds:?}");
        }
        cmds
    };

    let input_record_separator = if z { 0 } else { b'\n' };
    let output_record_separator = if zo { 0 } else { b'\n' };

    let now = SystemTime::now();

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
    let mut items: Vec<Item>;

    for cmd in cmds {
        match cmd {
            ProcessingCommand::Skip(n) => {
                let n = selected_items.len().min(n);
                selected_items = &mut selected_items[n..]
            }
            ProcessingCommand::Head(n) => {
                let n = selected_items.len().min(n);
                selected_items = &mut selected_items[..n]
            }
            ProcessingCommand::Tail(n) => {
                let n = selected_items.len().saturating_sub(n);
                selected_items = &mut selected_items[n..]
            }
            ProcessingCommand::Reverse => selected_items.reverse(),
            ProcessingCommand::FilterDays(range) => {
                items = selected_items
                    .into_iter()
                    .map(|item| item.clone())
                    .filter(|item| {
                        let age_days = item.age_days(now).expect("age is ok");
                        match range {
                            IntRange::At(n) => u64::from(n) == age_days,
                            IntRange::RangeFrom(n) => age_days >= u64::from(n),
                            IntRange::RangeTo(n) => age_days <= u64::from(n),
                            IntRange::RangeInclusive(from, to) => {
                                age_days >= u64::from(from)
                                    && age_days <= u64::from(to)
                            }
                        }
                    })
                    .collect();
                selected_items = &mut *items;
            }
        }
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
