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
use rand::{rngs::ThreadRng, Rng};
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
pub enum IntRange {
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

// XX should impl one of the random value generation traits instead
impl IntRange {
    pub fn random(rng: &mut ThreadRng) -> Self {
        // Reminder to update it
        match IntRange::RangeTo(0) {
            IntRange::At(_) => (),
            IntRange::RangeFrom(_) => (),
            IntRange::RangeTo(_) => (),
            IntRange::RangeInclusive(_, _) => (),
        }
        match rng.gen_range::<u8, _>(0..4) {
            0 => IntRange::At(rng.gen_range(0..10)),
            1 => IntRange::RangeFrom(rng.gen_range(0..12)),
            2 => IntRange::RangeTo(rng.gen_range(0..20)),
            3 => {
                let from = rng.gen_range(0..11);
                IntRange::RangeInclusive(from, from + rng.gen_range(0..8))
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum ProcessingCommand {
    Skip(usize),
    SkipTail(usize),
    Head(usize),
    Tail(usize),
    FilterDays(IntRange),
    Reverse,
}

impl ProcessingCommand {
    /// Return the command to run on a reversed sequence to get the
    /// same results as running the original then reversing the result
    fn reversed(&self) -> Self {
        match self {
            ProcessingCommand::Skip(n) => ProcessingCommand::SkipTail(*n),
            ProcessingCommand::SkipTail(n) => ProcessingCommand::Skip(*n),
            ProcessingCommand::Head(n) => ProcessingCommand::Tail(*n),
            ProcessingCommand::Tail(n) => ProcessingCommand::Head(*n),
            // No change
            ProcessingCommand::FilterDays(v) => {
                ProcessingCommand::FilterDays(v.clone())
            }
            // Weird one, should not be encountered because we already
            // take spans without Reverse
            ProcessingCommand::Reverse => ProcessingCommand::Reverse,
        }
    }
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
            "skip-tail" => ProcessingCommand::SkipTail(parse_usize(&mut i)?),
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

fn remove_two<T>(vec: &mut Vec<T>, i1: usize, i2: usize) {
    assert!(i1 < i2);
    // Could optimize further to move the part after i2 by 2
    // positions, but don't want to do unsafe right now, OK?
    vec.remove(i2);
    vec.remove(i1);
}

fn optimize_processing_commands(
    cmds: &[ProcessingCommand],
) -> Vec<ProcessingCommand> {
    let mut out: Vec<ProcessingCommand> =
        cmds.iter().map(|v| v.clone()).collect();

    loop {
        // Eliminate duplicate Reverse commands
        if let Some((i, _v)) = out
            .iter()
            .enumerate()
            .find(|(_i, v)| **v == ProcessingCommand::Reverse)
        {
            let rest = &mut out[i + 1..];
            if let Some((i2, _v)) = rest
                .iter()
                .enumerate()
                .find(|(_i, v)| **v == ProcessingCommand::Reverse)
            {
                let inbetween = &mut rest[..i2];
                for item in inbetween {
                    *item = item.reversed();
                }
                remove_two(&mut out, i, i + 1 + i2);
                continue;
            }
        }

        break;
    }

    // Can only do these after the above is finished!

    // Move the single remaining `Reverse` to the end, if present
    if let Some((i, _v)) = out
        .iter()
        .enumerate()
        .find(|(_i, v)| **v == ProcessingCommand::Reverse)
    {
        out.remove(i);
        for item in &mut out[i..] {
            *item = item.reversed();
        }
        out.push(ProcessingCommand::Reverse);
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::{thread_rng, Rng};
    use std::time::Duration;

    #[test]
    fn randomt_optimize_processing_commands() {
        let mut rng = thread_rng();

        const APPROX_NUM_PATHS: u16 = 1000;
        // Make some random 'paths', each ~20 B long, matching the
        // frequency of 0-bytes we insert
        let backing: Vec<u8> = (0..u32::from(APPROX_NUM_PATHS) * 20)
            .map(|_| {
                if rng.gen_range::<u8, _>(0..20) == 0 {
                    0
                } else {
                    rng.gen_range(65..91)
                }
            })
            .collect();

        // Make items from it
        let now = SystemTime::now();
        let mut items: Vec<Item> = backing
            .split(|c| *c == 0)
            .map(|path| {
                let path: &OsStr = OsStr::from_bytes(path);
                let path: &Path = path.as_ref();
                let ago_seconds: u64 =
                    // 10 days, so that the values for day patterns
                    // actually matter (and the random ranges are
                    // around and a bit over 10 days to match this)
                    rng.gen_range(0..10*24*3600);
                let mtime = now
                    .checked_sub(Duration::from_secs(ago_seconds))
                    .expect("always OK");
                Item { path, mtime }
            })
            .collect();
        sort_items(&mut items, rng.gen_bool(0.5));
        let items = items;

        // Search for invalid optimizations
        for i in 0..10000 {
            let max_len: usize = match rng.gen_range::<u8, _>(0..5) {
                0 | 1 => 5,
                2 => 10,
                3 => 20,
                4 => 100,
                _ => unreachable!(),
            };
            let cmds_len: usize = rng.gen_range(1..max_len);
            let cmds_original: Vec<ProcessingCommand> = (0..cmds_len)
                .map(|_| {
                    // reminder to add cases when it changes:
                    match ProcessingCommand::Reverse {
                        ProcessingCommand::Skip(_) => (),
                        ProcessingCommand::SkipTail(_) => (),
                        ProcessingCommand::Head(_) => (),
                        ProcessingCommand::Tail(_) => (),
                        ProcessingCommand::FilterDays(_) => (),
                        ProcessingCommand::Reverse => (),
                    }
                    match rng.gen_range::<u8, _>(0..6) {
                        0 => ProcessingCommand::Skip(
                            rng.gen_range(0..APPROX_NUM_PATHS.into()),
                        ),
                        1 => ProcessingCommand::SkipTail(
                            rng.gen_range(0..APPROX_NUM_PATHS.into()),
                        ),
                        2 => ProcessingCommand::Head(
                            rng.gen_range(0..APPROX_NUM_PATHS.into()),
                        ),
                        3 => ProcessingCommand::Tail(
                            rng.gen_range(0..APPROX_NUM_PATHS.into()),
                        ),
                        4 => ProcessingCommand::FilterDays(IntRange::random(
                            &mut rng,
                        )),
                        5 => ProcessingCommand::Reverse,
                        _ => unreachable!(),
                    }
                })
                .collect();
            let cmds_optimized = optimize_processing_commands(&cmds_original);

            let mut items1 = items.clone();
            let results_original =
                run_processing_commands(&mut items1, &cmds_original, now);
            let mut items2 = items.clone();
            let results_optimized =
                run_processing_commands(&mut items2, &cmds_optimized, now);
            if results_original != results_optimized {
                panic!(
                    "optimizer failure (i={i}):\n\
                     items={items:#?}\n\
                     results_original={results_original:#?}\n\
                     results_optimized={results_optimized:#?}\n\
                     cmds_original={cmds_original:#?}\n\
                     cmds_optimized={cmds_optimized:#?}\n\
                     "
                )
            }
        }
    }
}

fn sort_items<'t: 'v, 'v>(items: &'v mut Vec<Item<'t>>, reverse: bool) {
    if reverse {
        items.sort_by(|b, a| {
            a.mtime.cmp(&b.mtime).then_with(|| a.path.cmp(&b.path))
        });
    } else {
        items.sort_by(|a, b| {
            a.mtime.cmp(&b.mtime).then_with(|| a.path.cmp(&b.path))
        });
    }
}

// XX see if newer compilers can compile the code below without this!
// This is needed for rustc 1.63.0.
unsafe fn hack_static<'a, 'v, T: ?Sized>(rf: &'a mut T) -> &'v mut T {
    let ptr: *mut T = rf;
    &mut *ptr
}

fn run_processing_commands<'t: 'v, 'v>(
    items: &'v mut Vec<Item<'t>>,
    cmds: &[ProcessingCommand],
    now: SystemTime,
) -> &'v mut [Item<'t>] {
    let mut selected_items = unsafe { hack_static(&mut **items) };
    for cmd in cmds {
        match cmd {
            ProcessingCommand::Skip(n) => {
                let n = selected_items.len().min(*n);
                selected_items = &mut selected_items[n..]
            }
            ProcessingCommand::SkipTail(n) => {
                let n = selected_items.len() - selected_items.len().min(*n);
                selected_items = &mut selected_items[..n]
            }
            ProcessingCommand::Head(n) => {
                let n = selected_items.len().min(*n);
                selected_items = &mut selected_items[..n]
            }
            ProcessingCommand::Tail(n) => {
                let n = selected_items.len().saturating_sub(*n);
                selected_items = &mut selected_items[n..]
            }
            ProcessingCommand::Reverse => selected_items.reverse(),
            ProcessingCommand::FilterDays(range) => {
                let new_items: Vec<_> = selected_items
                    .into_iter()
                    .map(|item| item.clone())
                    .filter(|item| {
                        let age_days = item.age_days(now).expect("age is ok");
                        match range {
                            IntRange::At(n) => u64::from(*n) == age_days,
                            IntRange::RangeFrom(n) => age_days >= u64::from(*n),
                            IntRange::RangeTo(n) => age_days <= u64::from(*n),
                            IntRange::RangeInclusive(from, to) => {
                                age_days >= u64::from(*from)
                                    && age_days <= u64::from(*to)
                            }
                        }
                    })
                    .collect();
                drop(selected_items);
                *items = new_items;
                selected_items = unsafe { hack_static(&mut **items) };
            }
        }
    }
    selected_items
}

fn chomp(v: &mut Vec<u8>, record_separator: u8) {
    if let Some(last) = v.last() {
        if *last == record_separator {
            v.pop();
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

    sort_items(&mut items, reverse);

    let selected_items = run_processing_commands(&mut items, &cmds, now);

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
