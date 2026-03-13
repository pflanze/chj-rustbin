use std::{
    cmp::Ordering,
    env::set_current_dir,
    ffi::OsStr,
    fmt::Display,
    fs::Metadata,
    io::{stdin, stdout, BufWriter, IoSlice},
    marker::PhantomData,
    os::unix::prelude::{MetadataExt, OsStrExt},
    path::{Path, PathBuf},
    str::{Chars, FromStr},
    sync::Mutex,
    time::SystemTime,
};

use anstyle::{AnsiColor, Color, Style};
use anyhow::{anyhow, bail, Context, Result};
use chj_rustbin::{
    bag::Bag,
    cpu_probe,
    io::{
        unix_gr::{Gid, GrInfoCache},
        unix_pw::{PwInfoCache, Uid},
    },
    io_utils::{
        read_buf::{ParReadBufStream, ReadBufStream, ReadBufStreamError},
        read_dir_bufs::ReadDirBufStream,
        read_find_bufs::FindBufStream,
    },
    is_a_terminal::is_a_terminal,
    path_file_kind::{FileKind, ToFileKind},
    probe,
    text::yattable::{Widths, YatTable},
    time::age_at::AgeAt,
};
use chrono::{DateTime, Datelike, Local, Timelike};
use clap::Parser;
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
    /// Say what is done
    #[clap(short, long)]
    verbose: bool,

    /// Get the path listing internally instead of reading it from
    /// stdin: list the file names in the given directory (1 level
    /// deep)
    #[clap(long)]
    ls_dir: Option<PathBuf>,

    /// Get the path listing internally instead of reading it from
    /// stdin: list the file tree in the given directory (all depths)
    #[clap(long)]
    find_dir: Option<PathBuf>,

    /// Ignore paths matching the given regex (not glob pattern!)
    #[clap(short, long)]
    ignore: Vec<Regex>,

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
    use rayon::{iter::IntoParallelIterator, slice::ParallelSliceMut};
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
        let items: Vec<Item> = backing
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
                }
            })
            .collect();
        let sortfn = sort_function(
            rng.gen_bool(0.5),
            rng.gen_bool(0.5),
            rng.gen_bool(0.5),
        );
        let items = items;
        let mut itemrefs: Vec<_> = items.iter().collect();
        itemrefs.par_sort_by(|a, b| sortfn(a, b));
        let itemrefs = itemrefs;
        #[allow(unused)]
        let items = ();

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
                            4 => ProcessingCommand::FilterDays(
                                IntRange::random(&mut rng),
                            ),
                            5 => ProcessingCommand::Reverse,
                            _ => unreachable!(),
                        }
                    })
                    .collect();
                let cmds_optimized =
                    optimize_processing_commands(&cmds_original);

                let mut items1 = itemrefs.clone();
                let results_original = run_processing_commands(
                    &mut items1,
                    &cmds_original,
                    now,
                    false,
                );
                let mut items2 = itemrefs.clone();
                let results_optimized = run_processing_commands(
                    &mut items2,
                    &cmds_optimized,
                    now,
                    false,
                );
                if results_original != results_optimized {
                    panic!(
                        "optimizer failure (thread/i={thread_i}/{i}):\n\
                         items={items:#?}\n\
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

fn ci_cmp(a: &Path, b: &Path) -> Ordering {
    if let Some(a) = a.to_str() {
        if let Some(b) = b.to_str() {
            fn filter<'t>(it: Chars<'t>) -> impl Iterator<Item = char> + 't {
                it.filter(|c| c.is_alphanumeric())
            }
            let mut achars = filter(a.chars());
            let mut bchars = filter(b.chars());
            // What the ordering is case sensitively while still
            // ignoring non-alphanumeric characters
            let mut stricter_ordering = Ordering::Equal;
            loop {
                if let Some(ac) = achars.next() {
                    if let Some(bc) = bchars.next() {
                        match ac.to_lowercase().cmp(bc.to_lowercase()) {
                            Ordering::Equal => {
                                if stricter_ordering == Ordering::Equal {
                                    // ac == bc in lower-case; now
                                    // order lower-case versions
                                    // *before* upper-case ones--the
                                    // whole point of the
                                    // stricter_ordering variable over
                                    // just doing `a.cmp(b)` in the
                                    // end
                                    stricter_ordering = bc.cmp(&ac);
                                }
                                ()
                            }
                            v => return v,
                        }
                    } else {
                        return Ordering::Greater;
                    }
                } else {
                    if let Some(_) = bchars.next() {
                        return Ordering::Less;
                    } else {
                        // Don't return Equal, still make it
                        // deterministic:
                        return stricter_ordering.then_with(|| a.cmp(b));
                    }
                }
            }
        }
    }
    a.cmp(b)
}

/// If `time_reversed`, then time sorting is with newest items at the
/// bottom by default; this also changes to forward alphanumeric
/// fallback for that sorting.
fn sort_function<'t: 'v, 'v>(
    reverse: bool,
    time: bool,
    time_reversed: bool,
) -> for<'a, 'b, 'i> fn(a: &'a Item<'i>, b: &'b Item<'i>) -> Ordering {
    if !time {
        if reverse {
            |b, a| ci_cmp(&a.path, &b.path)
        } else {
            |a, b| ci_cmp(&a.path, &b.path)
        }
    } else {
        if time_reversed {
            if !reverse {
                |a, b| {
                    a.mtime()
                        .cmp(&b.mtime())
                        .then_with(|| ci_cmp(&a.path, &b.path))
                }
            } else {
                |b, a| {
                    a.mtime()
                        .cmp(&b.mtime())
                        .then_with(|| ci_cmp(&a.path, &b.path))
                }
            }
        } else {
            if reverse {
                |a, b| {
                    a.mtime()
                        .cmp(&b.mtime())
                        .then_with(|| ci_cmp(&b.path, &a.path))
                }
            } else {
                |b, a| {
                    a.mtime()
                        .cmp(&b.mtime())
                        .then_with(|| ci_cmp(&b.path, &a.path))
                }
            }
        }
    }
}

// XX see if newer compilers can compile the code below without this!
// This is needed for rustc 1.63.0.
unsafe fn hack_static<'a, 'v, T: ?Sized>(rf: &'a mut T) -> &'v mut T {
    let ptr: *mut T = rf;
    &mut *ptr
}

fn run_processing_commands<'t: 'u, 'u: 'v, 'v>(
    items: &'v mut Vec<&'u Item<'t>>,
    cmds: &[ProcessingCommand],
    now: SystemTime,
    show_files_from_future: bool,
) -> &'v [&'u Item<'t>] {
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
                let new_items: Vec<&'u Item<'t>> = (&*selected_items)
                    .into_iter()
                    .copied()
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
                        match item.age_days_at(now) {
                            Ok(age_days) => f(age_days),
                            Err(_e) => {
                                if show_files_from_future {
                                    true
                                } else {
                                    match now.age_days_at(item.mtime()) {
                                        Ok(0) => f(0),
                                        _ => false,
                                    }
                                }
                            }
                        }
                    })
                    .collect();
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnixFileType {
    File = 8,
    Dir = 4,
    Link = 10,
    Socket = 12,
    CharDevice = 2,
    BlockDevice = 6,
    Pipe = 1,
    // Linux uses this value when getting `metadata` for
    // /proc/$pid/task/$tid/fd/$n symlink
    None = 0,
}

impl UnixFileType {
    pub fn is_dir(self) -> bool {
        self == UnixFileType::Dir
    }
    pub fn is_file(self) -> bool {
        self == UnixFileType::File
    }
    pub fn is_link(self) -> bool {
        self == UnixFileType::Link
    }
    /// Char as used by the `ls` command with `-l`
    pub fn type_char(self) -> char {
        match self {
            UnixFileType::File => '-',
            UnixFileType::Dir => 'd',
            UnixFileType::Link => 'l',
            UnixFileType::Socket => 's',
            UnixFileType::CharDevice => 'c',
            UnixFileType::BlockDevice => 'b',
            UnixFileType::Pipe => 'p',
            UnixFileType::None => 'N',
        }
    }
    pub fn has_device_info(self) -> bool {
        match self {
            UnixFileType::File
            | UnixFileType::Dir
            | UnixFileType::Link
            | UnixFileType::Socket
            | UnixFileType::Pipe
            | UnixFileType::None => false,
            UnixFileType::CharDevice | UnixFileType::BlockDevice => true,
        }
    }
}

impl TryFrom<u8> for UnixFileType {
    type Error = anyhow::Error;
    fn try_from(m: u8) -> Result<Self> {
        match m {
            8 => Ok(UnixFileType::File),
            4 => Ok(UnixFileType::Dir),
            10 => Ok(UnixFileType::Link),
            12 => Ok(UnixFileType::Socket),
            2 => Ok(UnixFileType::CharDevice),
            6 => Ok(UnixFileType::BlockDevice),
            1 => Ok(UnixFileType::Pipe),
            0 => Ok(UnixFileType::None),
            _ => bail!("invalid file type number {m}"),
        }
    }
}

pub trait RwxPosition {
    const S_CHAR_SET: char;
    const S_CHAR_UNSET: char;
}

pub struct RwxUser;
impl RwxPosition for RwxUser {
    const S_CHAR_SET: char = 's';
    const S_CHAR_UNSET: char = 'S';
}

pub struct RwxGroup;
impl RwxPosition for RwxGroup {
    const S_CHAR_SET: char = 's';
    const S_CHAR_UNSET: char = 'S';
}

pub struct RwxOther;
impl RwxPosition for RwxOther {
    const S_CHAR_SET: char = 't';
    const S_CHAR_UNSET: char = 'T';
}

/// Contains the r, w, x flags for one of user/group/other (P
/// determines which of those), as well as the setuid/setgid/sticky
/// flag (again depending on P)
#[derive(Debug, PartialEq, Eq)]
pub struct Rwx<P: RwxPosition>(u8, PhantomData<fn() -> P>);

impl<P: RwxPosition> Clone for Rwx<P> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}
impl<P: RwxPosition> Copy for Rwx<P> {}

impl<P: RwxPosition> Rwx<P> {
    pub fn s_or_t(self) -> bool {
        ((self.0 >> 3) & 1) > 0
    }
    pub fn r(self) -> bool {
        ((self.0 >> 2) & 1) > 0
    }
    pub fn w(self) -> bool {
        ((self.0 >> 1) & 1) > 0
    }
    pub fn x(self) -> bool {
        ((self.0 >> 0) & 1) > 0
    }
}

// unused?
impl<P: RwxPosition> TryFrom<u8> for Rwx<P> {
    type Error = anyhow::Error;
    fn try_from(m: u8) -> Result<Self> {
        if m < 16 {
            Ok(Self(m, PhantomData))
        } else {
            bail!("Rwx number must be 4 bits, i.e. 0..15")
        }
    }
}

impl<P: RwxPosition> Display for Rwx<P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write;
        f.write_char(if self.r() { 'r' } else { '-' })?;
        f.write_char(if self.w() { 'w' } else { '-' })?;
        let x_char = if self.x() {
            if self.s_or_t() {
                P::S_CHAR_SET
            } else {
                'x'
            }
        } else {
            if self.s_or_t() {
                P::S_CHAR_UNSET
            } else {
                '-'
            }
        };
        f.write_char(x_char)?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Mode(u32);

impl Mode {
    pub fn u(self) -> Rwx<RwxUser> {
        let flags = ((self.0 & 0o7000) >> 9) as u8;
        let flag = ((flags >> 2) & 1) << 3;
        Rwx(((self.0 & 0o0700) >> 6) as u8 | flag, PhantomData)
    }
    pub fn g(self) -> Rwx<RwxGroup> {
        let flags = ((self.0 & 0o7000) >> 9) as u8;
        let flag = ((flags >> 1) & 1) << 3;
        Rwx(((self.0 & 0o0070) >> 3) as u8 | flag, PhantomData)
    }
    pub fn o(self) -> Rwx<RwxOther> {
        let flags = ((self.0 & 0o7000) >> 9) as u8;
        let flag = ((flags >> 0) & 1) << 3;
        Rwx((self.0 & 0o0007) as u8 | flag, PhantomData)
    }
    pub fn s_bits(self) -> u32 {
        (self.0 & 0o7000) >> 9
    }
    pub fn setuid(self) -> bool {
        (self.0 & 0o4000) > 0
    }
    pub fn setgid(self) -> bool {
        (self.0 & 0o2000) > 0
    }
    pub fn sticky(self) -> bool {
        (self.0 & 0o1000) > 0
    }
    pub fn any_x(self) -> bool {
        self.u().x() || self.g().x() || self.o().x()
    }
    pub fn all_w(self) -> bool {
        self.u().w() && self.g().w() && self.o().w()
    }
    pub fn filetype(self) -> UnixFileType {
        (((self.0 & 0o170000) >> 12) as u8)
            .try_into()
            .expect("OS gives valid values")
    }
}

impl From<u32> for Mode {
    fn from(m: u32) -> Self {
        Self(m)
    }
}

impl Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let t = self.filetype();
        let d = t.type_char();
        write!(f, "{d}{}{}{}", self.u(), self.g(), self.o())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RDev(u64);

impl From<u64> for RDev {
    fn from(v: u64) -> Self {
        Self(v)
    }
}

impl RDev {
    /// No idea if it uses anything more than 8 bits? But the original
    /// combined value *is* 64-bit.
    pub fn major(self) -> u64 {
        (self.0 >> 8) as u64
    }
    pub fn minor(self) -> u8 {
        (self.0 & ((1 << 8) - 1)) as u8
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct EssentialMetadata {
    mtime: SystemTime,
    size: u64,
    mode: Mode,
    uid: Uid,
    gid: Gid,
    nlink: u64,
    device: Option<RDev>,
    file_kind: Option<FileKind>,
}

impl EssentialMetadata {
    pub fn from_symlink_metadata(
        s: &Metadata,
        file_kind: Option<FileKind>,
    ) -> Result<Self> {
        let mode: Mode = s.mode().into();
        let device = if mode.filetype().has_device_info() {
            Some(s.rdev().into())
        } else {
            None
        };
        Ok(EssentialMetadata {
            mtime: s.modified()?,
            size: s.size(),
            mode,
            uid: s.uid().into(),
            gid: s.gid().into(),
            nlink: s.nlink(),
            device,
            file_kind,
        })
    }

    pub fn style(&self) -> Option<Style> {
        let mode = self.mode;
        match mode.filetype() {
            UnixFileType::None => None,
            UnixFileType::File => {
                if mode.u().s_or_t() {
                    Some(
                        Style::new()
                            .fg_color(Some(Color::Ansi(AnsiColor::White)))
                            .bg_color(Some(Color::Ansi(AnsiColor::Red))),
                    )
                } else if mode.g().s_or_t() {
                    Some(
                        Style::new()
                            .fg_color(Some(Color::Ansi(AnsiColor::Black)))
                            .bg_color(Some(Color::Ansi(AnsiColor::Yellow))),
                    )
                } else if mode.any_x() {
                    Some(
                        Style::new().bold().fg_color(Some(Color::Ansi(
                            AnsiColor::BrightGreen,
                        ))),
                    )
                } else {
                    match self.file_kind {
                        Some(k) => Some(match k {
                            FileKind::EmacsBackupFile => Style::new().fg_color(
                                Some(Color::Ansi(AnsiColor::BrightBlack)),
                            ),
                            FileKind::VisualMedia => {
                                Style::new().bold().fg_color(Some(Color::Ansi(
                                    AnsiColor::BrightMagenta,
                                )))
                            }
                            FileKind::Audio => Style::new()
                                .fg_color(Some(Color::Ansi(AnsiColor::Cyan))),
                            FileKind::Archive => Style::new()
                                .bold()
                                .fg_color(Some(Color::Ansi(AnsiColor::Red))),
                        }),
                        None => None,
                    }
                }
            }
            UnixFileType::Dir => Some(if mode.o().w() {
                Style::new()
                    .fg_color(Some(Color::Ansi(AnsiColor::Black)))
                    .bg_color(Some(Color::Ansi(AnsiColor::Green)))
            } else if mode.sticky() {
                Style::new()
                    .fg_color(Some(Color::Ansi(AnsiColor::White)))
                    .bg_color(Some(Color::Ansi(AnsiColor::Blue)))
            } else {
                Style::new()
                    .bold()
                    .fg_color(Some(Color::Ansi(AnsiColor::Blue)))
            }),
            UnixFileType::Link => Some(
                Style::new()
                    .bold()
                    .fg_color(Some(Color::Ansi(AnsiColor::Cyan))),
            ),
            UnixFileType::Socket => Some(
                Style::new()
                    .bold()
                    .fg_color(Some(Color::Ansi(AnsiColor::BrightMagenta))),
            ),
            UnixFileType::CharDevice | UnixFileType::BlockDevice => Some(
                Style::new()
                    .bold()
                    .fg_color(Some(Color::Ansi(AnsiColor::Yellow)))
                    .bg_color(Some(Color::Ansi(AnsiColor::Black))),
            ),
            UnixFileType::Pipe => Some(
                Style::new()
                    .fg_color(Some(Color::Ansi(AnsiColor::Yellow)))
                    .bg_color(Some(Color::Ansi(AnsiColor::Black))),
            ),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Item<'t> {
    path: &'t Path,
    metadata: EssentialMetadata,
    /// Metadata for the path if there was no error getting it
    link_target: Option<(Box<Path>, Option<EssentialMetadata>)>,
}

impl<'t> Item<'t> {
    pub fn from_path_and_metadata(
        path: &'t Path,
        read_link: bool,
        stat_link_target: bool,
        metadata: Metadata,
    ) -> Result<Option<Self>> {
        let metadata = EssentialMetadata::from_symlink_metadata(
            &metadata,
            path.to_file_kind(),
        )?;
        let link_target = if read_link && metadata.mode.filetype().is_link() {
            match path.read_link() {
                Ok(t) => {
                    let metadata2 = if stat_link_target {
                        path.metadata()
                            .ok()
                            .map(|m| {
                                EssentialMetadata::from_symlink_metadata(
                                    &m,
                                    t.to_file_kind(),
                                )
                                .ok()
                            })
                            .flatten()
                    } else {
                        None
                    };
                    Some((t.into(), metadata2))
                }
                Err(_) => None,
            }
        } else {
            None
        };
        Ok(Some(Item {
            path,
            metadata,
            link_target,
        }))
    }

    /// Returns None if the path is not found. `read_link` is true,
    /// retrieve the link target path; if `stat_link_target` is
    /// additionally true, try to get the metadata for the target, too
    pub fn from_path(
        path: &'t Path,
        read_link: bool,
        stat_link_target: bool,
    ) -> Result<Option<Self>> {
        match path.symlink_metadata() {
            Ok(metadata) => Self::from_path_and_metadata(
                path,
                read_link,
                stat_link_target,
                metadata,
            ),
            Err(e) => match e.kind() {
                std::io::ErrorKind::NotFound => Ok(None),
                _ => bail!("getting metadata for {path:?}: {e:#}"),
            },
        }
    }

    fn mtime(&self) -> SystemTime {
        self.metadata.mtime
    }

    // fn age_secs_at(&self, now: SystemTime) -> Result<u64> {
    //     self.mtime().age_secs_at(now)
    // }

    fn age_days_at(&self, now: SystemTime) -> Result<u64> {
        self.mtime().age_days_at(now)
    }
}

struct TableFromItems {
    use_color: bool,
    pw_info_cache: PwInfoCache,
    gr_info_cache: GrInfoCache,
}

impl TableFromItems {
    fn run<'u: 't, 't>(&self, items: &[&'u Item<'t>]) -> YatTable<7> {
        let Self {
            use_color,
            pw_info_cache,
            gr_info_cache,
        } = self;
        let mut table = YatTable::new();
        for item in items {
            let mode = item.metadata.mode;
            let nlink = item.metadata.nlink;
            let size = item.metadata.size;
            let uid = item.metadata.uid;
            let gid = item.metadata.gid;

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
            if let Some(rdev) = item.metadata.device {
                row.add_cell_fmt(format_args!(
                    "{}, {}",
                    rdev.major(),
                    rdev.minor()
                ));
            } else {
                row.add_cell_fmt(format_args!("{size}"));
            }
            let t: DateTime<Local> = item.metadata.mtime.into();
            let (year, month, day, hour, minute) =
                (t.year(), t.month(), t.day(), t.hour(), t.minute());
            row.add_cell_fmt(format_args!(
                "{year:04}-{month:02}-{day:02} {hour:02}:{minute:02}"
            ));

            let is_broken_link = *use_color
                && item.metadata.mode.filetype().is_link()
                && !item
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
                    item.metadata.style()
                }
            } else {
                None
            };

            if let Some(style) = style {
                row.add_cell_fmt(format_args!("{style}"));
                row.amend_cell_bytes(item.path.as_os_str().as_bytes());
            } else {
                row.add_cell_bytes(item.path.as_os_str().as_bytes());
            }
            if let Some(style) = style {
                row.amend_cell_fmt(format_args!("{style:#}"));
            }

            if let Some((target_path, target_metadata)) = &item.link_target {
                row.amend_cell_bytes(b" -> ");

                let target_style = if *use_color {
                    // If it's a broken link, re-use the same style as the main item
                    if is_broken_link {
                        style
                    } else {
                        target_metadata.as_ref().map(|m| m.style()).flatten()
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

struct GetItems<'t> {
    ignore: &'t [Regex],
    long: bool,
    use_color: bool,
}

impl<'t> GetItems<'t> {
    fn ignore_path(&self, path: &Path) -> bool {
        // Would it be better security wise to
        // ignore non-UTF8 paths, or error out?
        let path_str = path.to_string_lossy();
        for pat in self.ignore {
            if pat.is_match(&path_str) {
                return true;
            }
        }
        false
    }

    /// Leaks the backing memory for Item for now for simplicity
    fn from_read_buf_stream(
        &self,
        input: impl ParallelIterator<Item = Result<Vec<u8>, ReadBufStreamError>>,
        input_record_separator: u8,
    ) -> (Vec<Item<'static>>, Vec<anyhow::Error>) {
        // To avoid appending individual items multiple times (in
        // multiple reduce layers), collect the original vectors then
        // flatten them in one go at the end. Also, collect errors
        // from all chunks together, too, don't stop early except per
        // chunk.
        let (itemss, errors): (Vec<Vec<Item>>, Vec<anyhow::Error>) = input
            .map(|chunk| -> (Vec<Vec<Item>>, Vec<anyhow::Error>) {
                match (|| -> Result<Vec<Item>> {
                    let mut chunk = chunk?;
                    chomp(&mut chunk, input_record_separator);
                    // XX hack for now, OK for single-shot program
                    let chunk = chunk.leak();
                    let paths = chunk.split(|c| *c == input_record_separator);

                    Ok(paths
                        .map(|path| {
                            let path: &OsStr = OsStr::from_bytes(path);
                            let path: &Path = path.as_ref();
                            path
                        })
                        .filter(|path: &&Path| -> bool {
                            !self.ignore_path(path)
                        })
                        .map(|path| -> Result<Option<Item>> {
                            // Only stat linked files if used for
                            // coloring, and only in long format anyway.
                            Item::from_path(path, self.long, self.use_color)
                        })
                        .filter_map(|r| r.transpose())
                        .collect::<Result<Vec<_>>>()?)
                })() {
                    Ok(v) => (vec![v], vec![]),
                    Err(e) => (vec![], vec![e]),
                }
            })
            .reduce(
                || (Vec::new(), Vec::new()),
                |(mut itemss_a, mut errors_a), (mut itemss_b, mut errors_b)| {
                    itemss_a.append(&mut itemss_b);
                    errors_a.append(&mut errors_b);
                    (itemss_a, errors_a)
                },
            );
        // `into_par_iter` would be a large slow down here!
        (itemss.into_iter().flatten().collect(), errors)
    }

    /// `(Vec<Item<'static>>, Vec<anyhow::Error>)` is what one dir
    /// level yields. Vec of that since subdirs, too.
    fn _find(
        &self,
        dir: PathBuf,
        include_dir: bool,
        metadata: Metadata,
    ) -> (Bag<Item<'static>>, Bag<anyhow::Error>) {
        let Self {
            ignore: _,
            long,
            use_color,
        } = self;
        match (|| -> Result<_> {
            let mut items: Vec<Item<'static>> = Vec::new();
            let dir: &Path = dir.leak();
            if include_dir {
                if let Some(item) = Item::from_path_and_metadata(
                    dir, *long, *use_color, metadata,
                )? {
                    items.push(item);
                }
                // else will run into open error anyway--XX hmm
                // actually should accept not found then, there?
            }

            let input = std::fs::read_dir(dir)
                .with_context(|| anyhow!("directory {dir:?}"))?;
            let subdir_items: Mutex<(Bag<Item<'static>>, Bag<anyhow::Error>)> =
                Mutex::new((Bag::new(), Bag::new()));
            let subdir_items_rf = &subdir_items;
            rayon::scope(|scope| -> Result<()> {
                for entry in input {
                    let entry = entry?;
                    let metadata = entry.metadata()?;
                    let path = entry.path();
                    if self.ignore_path(&path) {
                        continue;
                    }
                    if metadata.is_dir() {
                        scope.spawn(move |_| {
                            let (items, errors) =
                                self._find(path, true, metadata);
                            let mut lock =
                                subdir_items_rf.lock().expect("no panics");
                            lock.0.push_bag(items);
                            lock.1.push_bag(errors);
                        });
                    } else {
                        // XX hmm wish i could unleak if below gives None?
                        let path = path.leak();
                        if let Some(item) = Item::from_path_and_metadata(
                            path, *long, *use_color, metadata,
                        )? {
                            items.push(item);
                        }
                    }
                }
                Ok(())
            })
            .with_context(|| anyhow!("reading entries in {dir:?}"))?;
            let mut subdir_items =
                subdir_items.into_inner().expect("no panics");
            subdir_items.0.push_bag(items.into());
            Ok(subdir_items)
        })() {
            Ok(items) => items,
            Err(e) => (Bag::Empty, Bag::Leaf(e)),
        }
    }

    /// Integrated "find", with `read_dir`, path filtering and
    /// `Item::from_path` intertwined for efficiency. Leaks the
    /// backing memory for Item for now for simplicity
    fn find(
        &self,
        dir: PathBuf,
        include_top: bool,
    ) -> Result<(Vec<Item<'static>>, Vec<anyhow::Error>)> {
        // XX .metadata() ?
        let metadata = dir
            .symlink_metadata()
            .with_context(|| anyhow!("directory {dir:?}"))?;
        let (items, errors) = self._find(dir, include_top, metadata);
        probe!("flattening");
        let items = items.flatten();
        let errors = errors.flatten();
        Ok((items, errors))
    }
}

fn main() -> Result<()> {
    cpu_probe::init()?;

    let Opt {
        verbose,
        ls_dir,
        find_dir,
        ignore,
        color,
        long,
        z,
        zo,
        time,
        time_reversed,
        show_files_from_future,
        reverse,
        no_optimize,
        processing_commands,
    } = Opt::from_args();

    if ls_dir.is_some() && find_dir.is_some() {
        bail!("please only give one of the --ls-dir or --find-dir options")
    }

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

    let use_color = match color {
        ColorMode::Always => true,
        ColorMode::Never => false,
        ColorMode::Auto => is_a_terminal(1),
    };

    let now = SystemTime::now();

    let desired_number_of_paths_per_chunk = 1000;
    let buf_size = desired_number_of_paths_per_chunk * 100;

    let get_items = GetItems {
        ignore: &ignore,
        long,
        use_color,
    };

    // Read the paths as blocks (as `Vec<u8>`) of some number of
    // null-terminated paths each, in either mode
    let (items, errors): (Vec<Item>, Vec<anyhow::Error>) = {
        probe!("get items");
        if let Some(basepath) = ls_dir {
            set_current_dir(&basepath).with_context(|| {
                anyhow!("changing to directory {basepath:?}")
            })?;
            get_items.from_read_buf_stream(
                ReadDirBufStream::new(
                    std::fs::read_dir(".").with_context(|| {
                        anyhow!("reading directory {basepath:?}")
                    })?,
                    buf_size,
                ),
                0,
            )
        } else if let Some(basepath) = find_dir {
            if false {
                get_items.from_read_buf_stream(
                    FindBufStream::new(buf_size, basepath, true)?.par_bridge(),
                    0,
                )
            } else {
                get_items.find(basepath, true)?
            }
        } else {
            get_items.from_read_buf_stream(
                ParReadBufStream::from(ReadBufStream::new(
                    stdin().lock(),
                    buf_size,
                    input_record_separator,
                ))
                .par_bridge(),
                input_record_separator,
            )
        }
    };

    let mut itemrefs: Vec<_> = items.iter().collect();
    #[allow(unused)]
    let items = ();

    let sortfn = sort_function(reverse, time, time_reversed);
    {
        probe!("sort_items");
        itemrefs.par_sort_by(|a, b| sortfn(a, b));
    }

    let selected_items = run_processing_commands(
        &mut itemrefs,
        &cmds,
        now,
        show_files_from_future,
    );

    probe!("writing to stdout");
    (|| -> Result<()> {
        use std::io::Write;
        if !long {
            let mut outp = BufWriter::new(stdout().lock());
            for item in selected_items {
                outp.write_all(item.path.as_os_str().as_bytes())?;
                outp.write_all(&[output_record_separator])?;
            }
            outp.flush()?;
        } else {
            let table_from_items = {
                probe!("resolve pw+gr info");
                let mut pw_info_cache = PwInfoCache::new();
                let mut gr_info_cache = GrInfoCache::new();
                if let Some(item) = selected_items.first() {
                    let (uid, gid) = (item.metadata.uid, item.metadata.gid);
                    pw_info_cache.lookup_by_uid(uid);
                    gr_info_cache.lookup_by_gid(gid);
                    let (mut last_uid, mut last_gid) = (uid, gid);
                    for item in selected_items {
                        let (uid, gid) = (item.metadata.uid, item.metadata.gid);
                        if uid != last_uid {
                            pw_info_cache.lookup_by_uid(uid);
                        }
                        if gid != last_gid {
                            gr_info_cache.lookup_by_gid(gid);
                        }
                        (last_uid, last_gid) = (uid, gid)
                    }
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
                            .write_out(
                                alignments,
                                output_record_separator,
                                &mut outp,
                            )
                            .expect("writing to mem doesn't fail");
                        outp
                    })
                    .collect()
            };

            {
                probe!("write out");
                let mut outp = stdout().lock();
                let output_ioslices: Vec<_> =
                    output_chunks.iter().map(|v| IoSlice::new(v)).collect();
                outp.write_vectored(&*output_ioslices)?;
                outp.flush()?;
            }
        }
        Ok(())
    })()
    .context("writing to stdout")?;

    if !errors.is_empty() {
        let mut msgs: Vec<u8> = Vec::new();
        for error in errors {
            use std::io::Write;
            _ = write!(&mut msgs, "{error}\n");
        }
        let msgs: &str = std::str::from_utf8(&msgs).expect("already utf8");
        bail!("while generating the above list:\n{msgs}");
    }

    Ok(())
}
