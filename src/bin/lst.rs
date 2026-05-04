use std::{
    borrow::Borrow,
    cmp::Ordering,
    env::set_current_dir,
    ffi::OsString,
    io::{stdin, stdout, BufWriter, IoSlice},
    ops::BitXor,
    os::unix::prelude::OsStrExt,
    path::PathBuf,
    str::FromStr,
    sync::Mutex,
    time::SystemTime,
};

use anstyle::{AnsiColor, Color, Style};
use anyhow::{anyhow, bail, Context, Result};
use chj_rustbin::{
    bag::Bag,
    cpu_probe,
    efficient_regex::EfficientRegex,
    file_location,
    hack_static::hack_static,
    io::{
        unix::unix_file_type::UnixFileTypeMask, unix_gr::GrInfoCache,
        unix_pw::PwInfoCache,
    },
    io_utils::{
        read_buf::{ParReadBufStream, ReadBufStream},
        read_dir_bufs::ReadDirBufStream,
        read_find_bufs::FindBufStream,
    },
    is_a_terminal::is_a_terminal,
    lst::{
        get_items::{GetItems, Item, UnixFileType},
        possibly_segmented_path::PossiblySegmentedPath,
        segmented_path::{tmp_path_buffer, SegmentedPath},
    },
    merge_trait::Merge,
    probe,
    shared_regions::SharedRegions,
    text::yattable::{Widths, YatTable},
    time::age_at::AgeAt,
};
use chrono::{DateTime, Datelike, Local, Timelike};
use clap::Parser;
use clap_with_warnings::clap_with_warnings;
use log::info;
use mimalloc::MiMalloc;
use rand::{rngs::ThreadRng, Rng};
use rayon::{
    iter::{IntoParallelIterator, ParallelBridge},
    prelude::ParallelIterator,
    slice::{ParallelSlice, ParallelSliceMut},
};
use regex::Regex;

#[derive(clap::ValueEnum, Clone, Debug)]
enum ColorMode {
    Always,
    Never,
    Auto,
}

impl FromStr for ColorMode {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "always" => Ok(Self::Always),
            "never" => Ok(Self::Never),
            "auto" => Ok(Self::Auto),
            _=> bail!("invalid string {s:?} for color mode, expecting always|never|auto")
        }
    }
}

#[clap_with_warnings]
#[derive(clap::Parser, Debug)]
/// Partial `ls` replacement that takes the paths to (sort and) show
/// from stdin
///
/// Formatting should behave exactly like `ls -l`, except for the
/// following known bugs: (1) ACLs are not checked (shown by ls as `+`
/// after permissions); (2) major and minor device numbers are not
/// aligned in columns like ls does.
///
/// Color mode should behave exactly like `ls` on a 16-color terminal.
///
/// There is a difference in behaviour when showing symlinks on
/// world-writable sticky folders (tmp folders), probably because the
/// kernel inhibits following the link for non-symlink `stat`, and
/// `ls` does something different.
///
/// Sorting should behave like `ls` when `--like-ls` is given.
#[clap(name = "lst from chj-rustbin")]
struct Opt {
    /// Use &Path to store paths; default: segmented paths.
    #[clap(long)]
    dev_old_path: bool,

    /// Say what is done
    #[clap(short, long)]
    verbose: bool,

    /// Say what is done
    #[clap(short, long)]
    debug: bool,

    /// Get the path listing internally instead of reading it from
    /// stdin: list the file names in the given directory (1 level
    /// deep)
    #[clap(long)]
    ls: Option<PathBuf>,

    /// Get the path listing internally instead of reading it from
    /// stdin: list the file tree in the given directory (all depths)
    #[clap(long)]
    find: Option<PathBuf>,

    /// Ignore paths matching the given regex (not glob pattern!)
    #[clap(long)]
    ignore_path_regex: Vec<Regex>,

    /// Ignore items matching the given file name (not a regex or glob
    /// pattern, and must not contain slashes)
    #[clap(long)]
    ignore_item_name: Vec<OsString>,

    /// Ignore items for which the file name matches the given regex
    /// (not glob pattern!)
    #[clap(long)]
    ignore_item_regex: Vec<Regex>,

    /// Print entries as "long" listing, like `ls -l`
    #[clap(short, long)]
    long: bool,

    /// In `--long` mode, whether to show color
    #[clap(long, default_value = "auto")]
    color: ColorMode,

    /// Expect inputs trailed by zero-bytes
    #[clap(short, long)]
    z: bool,

    /// Print output records separated by zero-bytes
    #[clap(long)]
    zo: bool,

    /// Sort by modification time
    #[clap(short, long)]
    time: bool,

    /// Sort in the reverse. (This is more efficient than using
    /// default sort order and then the `reverse` processing command.)
    #[clap(short, long)]
    reverse: bool,

    /// When `--time` is given, change the sort order to be with the
    /// newest items at the bottom by default. The fall-back sorting
    /// happens *forward* alphabetically (a file with path `a` is
    /// shown above a file with path `b` if both have the same
    /// time). This is unlike `ls` which requires the `--reverse`
    /// option to get this sort order, and then has the alpha fallback
    /// on its head. `--reverse` reverses both time and alpha fallback
    /// order (meaning alpha is then on its head, as expected).
    #[clap(long)]
    time_reversed: bool,

    /// When using `filter-days`, whether to let through items that
    /// are from the future; by default, only let them through if less
    /// than half a day in the future and 0 days is included in the
    /// filter range
    #[clap(long)]
    show_files_from_future: bool,

    /// Disable the optimizer for processing commands (in case there
    /// are bugs in it?)
    #[clap(long)]
    no_optimize: bool,

    /// Any number of post-processing commands after initial sorting:
    /// `skip n` skips up to n items from the top, `skip-tail n` skips
    /// up to n items from the bottom, `head n` takes up to the n top
    /// items, `tail n` takes up to the n bottom items, `reverse`
    /// reverses the items in the selection, `filter-days
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
    Filter {
        invert: bool,
        file_types: UnixFileTypeMask,
    },
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
            ProcessingCommand::FilterDays(_)
            | ProcessingCommand::Filter {
                invert: _,
                file_types: _,
            } => self.clone(),
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
        let parse_filter = |i: &mut usize| -> Result<ProcessingCommand> {
            let help_allowed = "f|d|l|s|c|b|p (or '-' as alias for 'f'), \
                                multiple allowed (with optional '|'), optionally prefixed with '!'";
            let help = || {
                format!(
                    "file type filtering argument ({help_allowed}) \
                     after processing command"
                )
            };
            if *i < processing_commands.len() {
                let arg_str: &str = processing_commands[*i].as_ref();
                *i += 1;
                let all_chars = arg_str.trim().chars();
                let mut chars = all_chars.clone();
                let mut invert = false;
                if chars.next() == Some('!') {
                    invert = true;
                } else {
                    chars = all_chars;
                }
                let mut file_types = 0;
                for c in chars {
                    match c {
                        '|' | ' ' => (),
                        _ => {
                            if let Some(ft) = UnixFileType::from_type_char(c) {
                                file_types |= ft.as_mask();
                            } else {
                                bail!(
                                    "invalid file type character {c:?} in argument {arg_str:?} \
                                     of processing command {cmd_str:?}, allowed: {help_allowed}"
                                )
                            }
                        }
                    }
                }
                Ok(ProcessingCommand::Filter { invert, file_types })
            } else {
                bail!("missing {} {cmd_str:?}", help())
            }
        };

        let cmd = match cmd_str {
            "skip" => ProcessingCommand::Skip(parse_usize(&mut i)?),
            "skip-tail" => ProcessingCommand::SkipTail(parse_usize(&mut i)?),
            "head" => ProcessingCommand::Head(parse_usize(&mut i)?),
            "tail" => ProcessingCommand::Tail(parse_usize(&mut i)?),
            "reverse" => ProcessingCommand::Reverse,
            "filter" => parse_filter(&mut i)?,
            "filter-days" => {
                ProcessingCommand::FilterDays(parse_range(&mut i)?)
            }
            _ => bail!(
                "unknown processing command {cmd_str:?} -- \
                 valid are skip, head, tail, reverse, filter, filter-days"
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
    let mut out: Vec<ProcessingCommand> = cmds.to_vec();

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
    use chj_rustbin::lst::get_items::EssentialMetadata;
    use rand::{thread_rng, Rng};
    use rayon::{iter::IntoParallelIterator, slice::ParallelSliceMut};
    use std::{ffi::OsStr, path::Path, time::Duration};

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
        let items: Vec<Item<_, _>> = backing
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
                Item {
                    path,
                    metadata: EssentialMetadata {
                        mtime,
                        size: 0,
                        mode: 0.into(),
                        uid: 0.into(),
                        gid: 0.into(),
                        nlink: 0,
                        device: None,
                        file_kind: None,
                    },
                    link_target: None,
                    _phantom: std::marker::PhantomData,
                }
            })
            .collect();
        let mut mini_items: Vec<_> = items.iter().map(MiniItem::from).collect();
        let cmp = cmp_function(
            rng.gen_bool(0.5),
            rng.gen_bool(0.5),
            rng.gen_bool(0.5),
        );
        mini_items.par_sort_by(|a, b| cmp(a, b));

        // Search for invalid optimizations
        let num_runs = match std::env::var_os("LST_NUM_TEST_RUNS") {
            Some(s) => u32::from_str(&s.to_string_lossy())
                .expect("LST_NUM_TEST_RUNS needs a u32 number"),
            None => 4,
        };
        (0..num_runs).into_par_iter().for_each(|thread_i| {
            let mut rng = thread_rng();

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
                            ProcessingCommand::Filter {
                                invert: _,
                                file_types: _,
                            } => (),
                        }
                        match rng.gen_range::<u8, _>(0..7) {
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
                            4 => ProcessingCommand::FilterDays(
                                IntRange::random(&mut rng),
                            ),
                            5 => ProcessingCommand::Reverse,
                            6 => ProcessingCommand::Filter {
                                invert: rng.gen_bool(0.5),
                                file_types: rng.gen(),
                            },
                            _ => unreachable!(),
                        }
                    })
                    .collect();
                let cmds_optimized =
                    optimize_processing_commands(&cmds_original);

                let mut items1 = mini_items.clone();
                let results_original = run_processing_commands(
                    &mut items1,
                    &cmds_original,
                    now,
                    false,
                );
                let mut items2 = mini_items.clone();
                let results_optimized = run_processing_commands(
                    &mut items2,
                    &cmds_optimized,
                    now,
                    false,
                );
                if results_original != results_optimized {
                    panic!(
                        "optimizer failure (thread/i={thread_i}/{i}):\n\
                         items={mini_items:#?}\n\
                         results_original={results_original:#?}\n\
                         results_optimized={results_optimized:#?}\n\
                         cmds_original={cmds_original:#?}\n\
                         cmds_optimized={cmds_optimized:#?}\n\
                     "
                    )
                }
            }
        });
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct InlineLst;

/// If `time_reversed`, then time sorting is with newest items at the
/// bottom by default; this also changes to forward alphanumeric
/// fallback for that sorting.
fn cmp_function<
    'region,
    P: PossiblySegmentedPath<'region, InlineLst> + Copy + Sync + Send + 'region,
>(
    reverse: bool,
    time: bool,
    time_reversed: bool,
) -> for<'i> fn(
    a: &MiniItem<'i, 'region, P>,
    b: &MiniItem<'i, 'region, P>,
) -> Ordering {
    if !time {
        if reverse {
            |b, a| a.path.ci_cmp(b.path)
        } else {
            |a, b| a.path.ci_cmp(b.path)
        }
    } else {
        if time_reversed {
            if !reverse {
                |a, b| a.mtime.cmp(&b.mtime).then_with(|| a.path.ci_cmp(b.path))
            } else {
                |b, a| a.mtime.cmp(&b.mtime).then_with(|| a.path.ci_cmp(b.path))
            }
        } else {
            if reverse {
                |a, b| a.mtime.cmp(&b.mtime).then_with(|| b.path.ci_cmp(a.path))
            } else {
                |b, a| a.mtime.cmp(&b.mtime).then_with(|| b.path.ci_cmp(a.path))
            }
        }
    }
}

/// Mutates `items`. Requires a mutable sequence because sorting
/// in-place needs to move the slots around. And it needs to be a Vec
/// since filtering changes the size of it.
fn run_processing_commands<
    'region: 'v + 'i,
    'v,
    'i,
    P: PossiblySegmentedPath<'region, InlineLst> + Copy + Sync + Send + 'region,
>(
    items: &'v mut Vec<MiniItem<'i, 'region, P>>,
    cmds: &[ProcessingCommand],
    now: SystemTime,
    show_files_from_future: bool,
) -> &'v [MiniItem<'i, 'region, P>] {
    probe!("run_processing_commands");
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
                let new_items: Vec<MiniItem<_>> = (&*selected_items)
                    .into_iter()
                    .filter(|item| {
                        let f = |age_days| match range {
                            IntRange::At(n) => u64::from(*n) == age_days,
                            IntRange::RangeFrom(n) => age_days >= u64::from(*n),
                            IntRange::RangeTo(n) => age_days <= u64::from(*n),
                            IntRange::RangeInclusive(from, to) => {
                                age_days >= u64::from(*from)
                                    && age_days <= u64::from(*to)
                            }
                        };
                        match item.item.age_days_at(now) {
                            Ok(age_days) => f(age_days),
                            Err(_e) => {
                                if show_files_from_future {
                                    true
                                } else {
                                    match now.age_days_at(item.mtime) {
                                        Ok(0) => f(0),
                                        _ => false,
                                    }
                                }
                            }
                        }
                    })
                    .map(|r| *r)
                    .collect();
                *items = new_items;
                selected_items = unsafe { hack_static(&mut **items) };
            }
            ProcessingCommand::Filter { invert, file_types } => {
                let new_items: Vec<MiniItem<_>> = (&*selected_items)
                    .into_iter()
                    .filter(|mini_item| {
                        invert.bitxor(
                            (mini_item.file_type_mask & file_types) != 0,
                        )
                    })
                    .map(|r| *r)
                    .collect();
                *items = new_items;
                selected_items = unsafe { hack_static(&mut **items) };
            }
        }
    }
    selected_items
}

struct TableFromItems {
    use_color: bool,
    pw_info_cache: PwInfoCache,
    gr_info_cache: GrInfoCache,
}

impl TableFromItems {
    fn run<
        'region: 'i,
        'i,
        P: PossiblySegmentedPath<'region, InlineLst>
            + Copy
            + Sync
            + Send
            + 'region,
    >(
        &self,
        mini_items: &[impl Borrow<MiniItem<'i, 'region, P>>],
    ) -> YatTable<7> {
        let Self {
            use_color,
            pw_info_cache,
            gr_info_cache,
        } = self;
        let mut table = YatTable::new();
        let mut tmp = tmp_path_buffer();
        for mini_item in mini_items {
            let mini_item: &MiniItem<'i, 'region, P> = mini_item.borrow();
            let metadata = &mini_item.item.metadata;
            let mode = metadata.mode;
            let nlink = metadata.nlink;
            let size = metadata.size;
            let uid = metadata.uid;
            let gid = metadata.gid;

            let mut row = table.new_row();
            row.add_cell_fmt(format_args!("{mode}"));
            row.add_cell_fmt(format_args!("{nlink}"));
            {
                let username = pw_info_cache
                    .get_by_uid(uid)
                    .and_then(|u| u.username())
                    .unwrap_or("<unknown>");
                row.add_cell_fmt(format_args!("{username}"));
            }
            {
                let groupname = gr_info_cache
                    .get_by_gid(gid)
                    .and_then(|g| g.groupname())
                    .unwrap_or("<unknown>");
                row.add_cell_fmt(format_args!("{groupname}"));
            }
            if let Some(rdev) = metadata.device {
                row.add_cell_fmt(format_args!(
                    "{}, {}",
                    rdev.major(),
                    rdev.minor()
                ));
            } else {
                row.add_cell_fmt(format_args!("{size}"));
            }
            let t: DateTime<Local> = metadata.mtime.into();
            let (year, month, day, hour, minute) =
                (t.year(), t.month(), t.day(), t.hour(), t.minute());
            row.add_cell_fmt(format_args!(
                "{year:04}-{month:02}-{day:02} {hour:02}:{minute:02}"
            ));

            let is_broken_link = *use_color
                && metadata.mode.file_type().is_link()
                && !mini_item
                    .item
                    .link_target
                    .as_ref()
                    .map(|(_, m)| m.is_some())
                    .unwrap_or(false);
            let style = if *use_color {
                // Special color if it's a broken link
                if is_broken_link {
                    Some(
                        Style::new()
                            .bold()
                            .fg_color(Some(Color::Ansi(AnsiColor::Red)))
                            .bg_color(Some(Color::Ansi(AnsiColor::Black))),
                    )
                } else {
                    // Otherwise (not a link, or not broken) what
                    // the metadata dictates
                    metadata.style()
                }
            } else {
                None
            };

            let path_bytes =
                mini_item.path.psp_to_path(&mut tmp).as_os_str().as_bytes();
            if let Some(style) = style {
                row.add_cell_fmt(format_args!("{style}"));
                row.amend_cell_bytes(path_bytes);
            } else {
                row.add_cell_bytes(path_bytes);
            }
            if let Some(style) = style {
                row.amend_cell_fmt(format_args!("{style:#}"));
            }

            if let Some((target_path, target_metadata)) =
                &mini_item.item.link_target
            {
                row.amend_cell_bytes(b" -> ");

                let target_style = if *use_color {
                    // If it's a broken link, re-use the same style as the main item
                    if is_broken_link {
                        style
                    } else {
                        target_metadata.as_ref().and_then(|m| m.style())
                    }
                } else {
                    None
                };

                if let Some(target_style) = target_style {
                    row.amend_cell_fmt(format_args!("{target_style}"));
                }
                row.amend_cell_bytes(target_path.as_os_str().as_bytes());
                if let Some(target_style) = target_style {
                    row.amend_cell_fmt(format_args!("{target_style:#}"));
                }
            }
        }
        table
    }
}

#[cfg(not(miri))]
#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn main() -> Result<()> {
    cpu_probe::init()?;

    let opt = Opt::parse();

    if opt.ls.is_some() && opt.find.is_some() {
        bail!("please only give one of the --ls-dir or --find-dir options")
    }

    if opt.verbose {
        std::env::set_var("RUST_LOG", "info");
    }
    if opt.debug {
        std::env::set_var("RUST_LOG", "trace");
    }
    env_logger::init();

    let orig_cmds = parse_processing_commands(&opt.processing_commands)?;
    let cmds = if opt.no_optimize {
        orig_cmds
    } else {
        let cmds = optimize_processing_commands(&orig_cmds);
        info!("original:\n{orig_cmds:?}\noptimized:\n{cmds:?}");
        cmds
    };

    let input_record_separator = if opt.z { 0 } else { b'\n' };
    let output_record_separator = if opt.zo { 0 } else { b'\n' };

    let use_color = match opt.color {
        ColorMode::Always => true,
        ColorMode::Never => false,
        ColorMode::Auto => is_a_terminal(1),
    };

    let now = SystemTime::now();

    let desired_number_of_paths_per_chunk = 1000;
    let buf_size = desired_number_of_paths_per_chunk * 100;

    let ignore_path_regex = if opt.ignore_path_regex.is_empty() {
        None
    } else {
        Some(EfficientRegex::new_either_from(&opt.ignore_path_regex))
    };

    let ignore_item_regex = if opt.ignore_item_regex.is_empty() {
        None
    } else {
        Some(EfficientRegex::new_either_from(&opt.ignore_item_regex))
    };

    let get_items = GetItems {
        ignore_path_regex,
        ignore_item_names: opt.ignore_item_name.clone(),
        ignore_item_regex,
        long: opt.long,
        use_color,
        _phantom: std::marker::PhantomData,
    };

    let shared_regions = SharedRegions::new(
        8 * 1048576,
        file_location!(),
        opt.verbose || opt.debug,
    );

    // Read the paths as blocks (as `Vec<u8>`) of some number of
    // null-terminated paths each, in either mode; pass to the
    // continuation in either of the supported path implementations

    macro_rules! cont_values {
        {} => {
            MainContVals {
                opt,
                cmds,
                now,
                output_record_separator,
                use_color,
            }
        }
    }

    if let Some(ref basepath) = opt.ls {
        let res = {
            probe!("get items");
            set_current_dir(basepath).with_context(|| {
                anyhow!("changing to directory {basepath:?}")
            })?;
            let dir_items = std::fs::read_dir(".")
                .with_context(|| anyhow!("reading directory {basepath:?}"))?;
            get_items.get_from_read_buf_stream(
                ReadDirBufStream::new(dir_items, buf_size),
                0,
            )
        };
        main_cont(cont_values!(), res)
    } else if let Some(ref basepath) = opt.find {
        if false {
            let res = {
                probe!("get items");
                let basepath = basepath.clone();
                get_items.get_from_read_buf_stream(
                    FindBufStream::new(buf_size, basepath, true)?.par_bridge(),
                    0,
                )
            };
            main_cont(cont_values!(), res)
        } else {
            if opt.dev_old_path {
                let res = {
                    probe!("get items");
                    let basepath = {
                        let mut region = shared_regions.get_region();
                        region.allocate_path(basepath)
                    };
                    get_items.find(basepath, true, &shared_regions)?
                };
                main_cont(cont_values!(), res)
            } else {
                let res = {
                    probe!("get items");
                    let basepath = {
                        let mut region = shared_regions.get_region();
                        SegmentedPath::new_from_path(&basepath, &mut region)
                            .ok_or_else(|| {
                                anyhow!("path {basepath:?} is not valid")
                            })?
                    };
                    get_items.find(basepath, true, &shared_regions)?
                };
                main_cont(cont_values!(), res)
            }
        }
    } else {
        let res = {
            probe!("get items");
            get_items.get_from_read_buf_stream(
                ParReadBufStream::from(ReadBufStream::new(
                    stdin().lock(),
                    buf_size,
                    input_record_separator,
                ))
                .par_bridge(),
                input_record_separator,
            )
        };
        main_cont(cont_values!(), res)
    }
}

struct MainContVals {
    opt: Opt,
    cmds: Vec<ProcessingCommand>,
    now: SystemTime,
    output_record_separator: u8,
    use_color: bool,
}

/// Flattened Item for more performant sorting
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct MiniItem<
    'i,
    'region: 'i,
    P: PossiblySegmentedPath<'region, InlineLst> + Copy + Sync + Send + 'region,
> {
    mtime: SystemTime,
    file_type_mask: UnixFileTypeMask,
    path: P,
    item: &'i Item<'region, P, InlineLst>,
}

impl<
        'i,
        'region: 'i,
        P: PossiblySegmentedPath<'region, InlineLst>
            + Copy
            + Sync
            + Send
            + 'region,
    > From<&'i Item<'region, P, InlineLst>> for MiniItem<'i, 'region, P>
{
    fn from(item: &'i Item<'region, P, InlineLst>) -> Self {
        MiniItem {
            mtime: item.mtime(),
            file_type_mask: item.metadata.file_type().as_mask(),
            path: item.path,
            item,
        }
    }
}

#[test]
fn t_sizeof_mini_item() {
    assert_eq!(size_of::<MiniItem<&SegmentedPath>>(), 40);
    assert_eq!(size_of::<[MiniItem<&SegmentedPath>; 10]>(), 400);
}

fn main_cont<
    'region,
    P: PossiblySegmentedPath<'region, InlineLst> + Copy + Sync + Send + 'region,
>(
    MainContVals {
        opt,
        cmds,
        now,
        output_record_separator,
        use_color,
    }: MainContVals,
    (items_bag, errors): (Bag<Item<'region, P, InlineLst>>, Vec<anyhow::Error>),
) -> Result<()> {
    let mut mini_items: Vec<MiniItem<'_, 'region, P>> = {
        probe!("map_flatten_refs");
        items_bag.map_flatten_refs(MiniItem::from)
    };
    let cmp = cmp_function::<P>(opt.reverse, opt.time, opt.time_reversed);
    {
        probe!("sort_items");
        mini_items.par_sort_by(|a, b| cmp(a, b));
    }

    let filtered_items = run_processing_commands(
        &mut mini_items,
        &cmds,
        now,
        opt.show_files_from_future,
    );

    probe!("writing to stdout");
    (|| -> Result<()> {
        if !opt.long {
            print_paths(filtered_items, output_record_separator)?
        } else {
            print_listing(filtered_items, output_record_separator, use_color)?
        }
        Ok(())
    })()
    .context("writing to stdout")?;

    if !errors.is_empty() {
        let mut msgs: Vec<u8> = Vec::new();
        for error in errors {
            use std::io::Write;
            _ = writeln!(&mut msgs, "{error:#}");
        }
        let msgs: &str = std::str::from_utf8(&msgs).expect("already utf8");
        bail!("while generating the above list:\n{msgs}");
    }

    Ok(())
}

fn print_paths<
    'region: 'i,
    'i,
    P: PossiblySegmentedPath<'region, InlineLst> + Copy + Sync + Send + 'region,
>(
    selected_items: &[impl Borrow<MiniItem<'i, 'region, P>>],
    output_record_separator: u8,
) -> Result<()> {
    use std::io::Write;
    let mut outp = BufWriter::new(stdout().lock());
    let mut tmp = tmp_path_buffer();
    for item in selected_items {
        let path = item.borrow().path.psp_to_path(&mut tmp);
        outp.write_all(path.as_os_str().as_bytes())?;
        outp.write_all(&[output_record_separator])?;
    }
    outp.flush()?;
    Ok(())
}

fn get_pw_gr_info<
    'region: 'i,
    'i,
    P: PossiblySegmentedPath<'region, InlineLst> + Copy + Sync + Send + 'region,
>(
    items: &[impl Borrow<MiniItem<'i, 'region, P>> + Sync],
) -> (PwInfoCache, GrInfoCache) {
    let mut pw_info_cache = PwInfoCache::new();
    let mut gr_info_cache = GrInfoCache::new();
    if let Some(item) = items.first() {
        let item = item.borrow();
        let (uid, gid) = (item.item.metadata.uid, item.item.metadata.gid);
        pw_info_cache.lookup_by_uid(uid);
        gr_info_cache.lookup_by_gid(gid);
        let (mut last_uid, mut last_gid) = (uid, gid);
        for item in items {
            let item: &MiniItem<_> = item.borrow();
            let (uid, gid) = (item.item.metadata.uid, item.item.metadata.gid);
            if uid != last_uid {
                pw_info_cache.lookup_by_uid(uid);
            }
            if gid != last_gid {
                gr_info_cache.lookup_by_gid(gid);
            }
            (last_uid, last_gid) = (uid, gid)
        }
    }
    (pw_info_cache, gr_info_cache)
}

fn print_listing<
    'region: 'i,
    'i,
    P: PossiblySegmentedPath<'region, InlineLst> + Copy + Sync + Send + 'region,
>(
    selected_items: &[impl Borrow<MiniItem<'i, 'region, P>> + Sync],
    output_record_separator: u8,
    use_color: bool,
) -> Result<()> {
    use std::io::Write;

    let table_from_items = {
        probe!("resolve pw+gr info");

        let chunk_size = 100000;
        let num_tasks = selected_items.len().div_ceil(chunk_size);
        let mut results: Vec<Mutex<Option<(PwInfoCache, GrInfoCache)>>> =
            Vec::new();
        results.resize_with(num_tasks, Default::default);
        rayon::scope(|scope| {
            let mut remainder = selected_items;
            let mut i = 0;
            while !remainder.is_empty() {
                let head;
                (head, remainder) =
                    remainder.split_at(chunk_size.min(remainder.len()));
                let result = &results[i];
                scope.spawn(move |_| {
                    let r = get_pw_gr_info(head);
                    *result.lock().expect("not locked before") = Some(r);
                });
                i += 1;
            }
        });
        let mut pw_info_cache = PwInfoCache::new();
        let mut gr_info_cache = GrInfoCache::new();
        for result in results {
            let (pw, gr) =
                result.into_inner().expect("no panic").expect("was set");
            pw_info_cache.mut_merge(pw);
            gr_info_cache.mut_merge(gr);
        }

        TableFromItems {
            use_color,
            pw_info_cache,
            gr_info_cache,
        }
    };

    let subtables: Vec<YatTable<7>> = {
        probe!("subtables");
        selected_items
            .par_chunks(2000)
            .map(|items| table_from_items.run(items))
            .collect()
    };

    let max_widths = {
        probe!("max_widths");
        let mut max_widths = Widths::default();
        for table in &subtables {
            max_widths.update_max(table.max_widths());
        }
        max_widths
    };

    let alignments = &[false, true, false, false, true, false, false];

    let output_chunks: Vec<Vec<u8>> = {
        probe!("output_chunks");
        subtables
            .into_par_iter()
            .map(|mut table| {
                table.set_max_widths(max_widths.clone());
                let mut outp = Vec::new();
                table
                    .write_out(alignments, output_record_separator, &mut outp)
                    .expect("writing to mem doesn't fail");
                outp
            })
            .collect()
    };

    {
        probe!("write out");
        let mut outp = stdout().lock();
        let mut output_ioslices = Vec::new();
        let mut tot_size = 0;
        for v in &output_chunks {
            output_ioslices.push(IoSlice::new(v));
            tot_size += v.len();
        }
        let written = outp.write_vectored(&output_ioslices)?;
        if written != tot_size {
            bail!(
                "could only write {written} out of {tot_size} bytes; \
                             todo: change to `write_all_vectored` once stable"
            )
        }
        outp.flush()?;
    }
    Ok(())
}
