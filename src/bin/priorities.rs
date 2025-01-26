use std::{path::{PathBuf, Path},
          convert::TryFrom,
          time::SystemTime,
          str::FromStr, collections::{BTreeSet, BTreeMap},
          rc::Rc, cell::Cell, ops::{Deref, Range}, process::exit,
          io::{stdout, Write},
          os::unix::prelude::OsStrExt, cmp::Ordering, fmt::Debug};

use anyhow::{Result, anyhow, Context, bail};
use chrono::{NaiveDateTime, NaiveTime, Weekday, Datelike, Duration,
             Utc, DateTime, Timelike};
use clap::Parser;
use kstring::KString;

use chj_rustbin::{parse::{parsers::{ParseableStr, FromParseableStr, Separator},
                          parse_error::{ParseError, ParseContext, StringParseContext,
                                        IntoOwningBacking, Backing}},
                  impl_item_options_from,
                  io::file_path_type::{ItemOptions, FilePathType,
                                       recursive_file_path_types_iter},
                  io::excludes::default_excludes,
                  region::Region,
                  conslist::{cons, List},
                  fp::compose,
                  time::naive_date_time_without_year::NaiveDateTimeWithoutYear,
                  };
use chj_rustbin::{parse_error, T};
use once_cell::sync::OnceCell;


const MONTH_SECONDS: i64 = 3600 * 24 * 30;

/// How much time before and after the modification time of a file a
/// due date without year may lie (this is only relevant if the file
/// does not have an issue timestamp).
const MAX_SECONDS_AROUND_MTIME: Range<i64> =
    (-4 * MONTH_SECONDS) .. (4 * MONTH_SECONDS);


fn _warning(s: String) {
    eprintln!("WARNING: {s}");
}
macro_rules! warning {
    { $($fmt:tt)* } => {
        _warning(format!($($fmt)*))
    }
}


#[derive(clap::Parser, Debug)]
/// Parse a folder with todo files with "OPEN.." or "TODO.." markers.
struct Opts {
    /// consider dirs
    #[clap(long)]
    dirs: bool,

    /// consider files
    #[clap(long)]
    files: bool,

    /// consider other items (symlinks, pipes, sockets, device files)
    #[clap(long)]
    other: bool,

    /// do not ignore dot and Emacs backup (ending in '~') files
    #[clap(short, long)]
    all: bool,

    /// show what the program is doing
    #[clap(short, long)]
    verbose: bool,

    /// do not show calculated priority left of each path, separated by a '\t'
    #[clap(short, long)]
    no_priority: bool,

    /// calculate priorities for this date/time instead of for now
    // Don't use NaiveDateTime, its parser is bad
    #[clap(short, long)]
    time: Option<String>,

    /// The base directories holding the todo files. If none given,
    /// uses `.`.
    directories: Vec<PathBuf>,
}

impl_item_options_from! {Opts}


#[derive(Debug, Clone, Copy)]
pub enum TaskSize {
    Minutes,
    Hours,
    Days,
    Weeks,
    Months,
    Unknown,
}

impl TaskSize {
    pub fn in_weeks(self) -> f32 {
        match self {
            TaskSize::Minutes => 1. / 48., // 1./12./4., 30 minutes = 1/12 day programming capacity
            TaskSize::Hours => 1. / 8., // half a day, 4 work days a week
            TaskSize::Days => 0.5, // 2 days, 4 work days a week
            TaskSize::Weeks => 2.,
            TaskSize::Months => 8.,
            // XX warn if it leads to large changes?
            TaskSize::Unknown => 0.2 // assume it's small if I don't specify
        }
    }
}

impl Default for TaskSize {
    fn default() -> Self {
        Self::Unknown
    }
}

impl<'s, B: Backing + 's> FromParseableStr<'s, B> for TaskSize
    where &'s B: Backing
{
    fn from_parseable_str(s: &ParseableStr<'s, B>)
                          -> Result<Self, ParseError<StringParseContext<&'s B>>>
    {
        let s = s.trim();
        match s.s {
            "h" | "hour" | "hours" => Ok(TaskSize::Hours),
            "d" | "day" | "days" => Ok(TaskSize::Days),
            "w" | "week" | "weeks" => Ok(TaskSize::Weeks),
            "mo" | "mon" | "month" | "months" => Ok(TaskSize::Months),
            "mi" | "min" | "minute" | "minutes" => Ok(TaskSize::Minutes),
            _ => Err(parse_error! {
                message: format!("unknown task size {:?} (must be \
                                  h|d|w|mo|month|months|mi|min|minuteminutes",
                                 s.s),
                context: s.into()
            })
        }
    }
}

pub type ImportanceLevel = u8; // 1..10 but not currently restricted

#[derive(Debug, Clone)]
pub struct Importance {
    level: ImportanceLevel
}

impl<'s, B: Backing + 's> FromParseableStr<'s, B> for Importance
where &'s B: Backing
{
    fn from_parseable_str(s: &ParseableStr<'s, B>)
                          -> Result<Self, ParseError<StringParseContext<&'s B>>>
    {
        let s = s.trim();
        let ss = s.s;
        let level = ImportanceLevel::from_str(ss).map_err(|e| {
            parse_error! {
                message: format!("{e}"),
                context: s.into()
            }
        })?;
        Ok(Importance { level })
    }
}

const DEFAULT_IMPORTANCE_LEVEL: ImportanceLevel = 5;

impl Default for Importance {
    fn default() -> Self {
        Self { level: DEFAULT_IMPORTANCE_LEVEL }
    }
}



pub type ManualPriorityLevel = u8; // 1..10 but not currently restricted

#[derive(Debug, Clone)]
pub enum Priority<C: ParseContext> {
    /// Due on that date
    Date(NaiveDateTimeWithOrWithoutYear<C>),
    /// Relative due date
    DaysFromToday(u8),
    /// Today at noon
    Today,
    /// Today at 6 PM
    Tonight,
    /// No particular due date or priority
    Ongoing,
    /// None given
    Unknown,
    /// 1 is highest priority (manually specified)
    Level(ManualPriorityLevel),
}

impl<'s, B: Backing> IntoOwningBacking<B::Owned>
    for Priority<StringParseContext<&'s B>>
where &'s B: Backing,
      B::Owned: Backing
{
    type Owning = Priority<StringParseContext<B::Owned>>;

    fn into_owning_backing(self) -> Self::Owning {
        use Priority::*;
        match self {
            Date(ndt_noyear) => Date(ndt_noyear.into_owning_backing()),
            DaysFromToday(x) => DaysFromToday(x),
            Today => Today,
            Tonight => Tonight,
            Ongoing => Ongoing,
            Unknown => Unknown,
            Level(v) => Level(v),
        }
    }
}


// Calculate a priority purely from how much time is left and how much
// time the task will need.
fn priority_level_for(time_weeks: f32, task_size_weeks: f32) -> f32 {
    let relation = time_weeks / task_size_weeks;
    relation.log2() * 2. + 1.
}

#[cfg(test)]
#[test]
fn t_priority_level() {
    assert_eq!(priority_level_for(1., 1.), 1.);
    assert_eq!(priority_level_for(2., 1.), 3.);
    assert_eq!(priority_level_for(4., 1.), 5.);
    assert_eq!(priority_level_for(8., 1.), 7.);
    assert_eq!(priority_level_for(16., 2.), 7.);
    assert_eq!(priority_level_for(1., 2.), -1.);
}

// Rough, hack. UTC. good enough when distance in days are the main thing I want.
fn naive_from_systemtime(st: SystemTime) -> NaiveDateTime {
    let dt: DateTime<Utc> = DateTime::from(st);
    //dt.naive_local() // XX hmmm, local based on what zone?
    dt.naive_utc()
}

/// Complete `ndt_noyear` with a year so that the resulting date is
/// within max_seconds_around_mtime of the given mtime. Returns an
/// error if that's not possible.
fn add_year_from_mtime<C: ParseContext + Clone>(
    ndt_noyear: &NaiveDateTimeWithoutYear<C>,
    mtime_ndt: NaiveDateTime,
    max_seconds_around_mtime: &Range<i64>
) -> Result<NaiveDateTime, ParseError<C>>
{
    let seconds_since_mtime = |ndt: NaiveDateTime| {
        ndt.signed_duration_since(mtime_ndt).num_seconds()
    };

    let mtime_year = mtime_ndt.year();
    let candidate_dates: Vec<NaiveDateTime> = {
        [
            mtime_year,
            mtime_year.checked_add(1)
                .expect("year is far within i32 range by OS limitation?"),
            mtime_year.checked_sub(1)
                .expect("year is far within i32 range by OS limitation?"),
        ]
            .iter()
            .filter_map(|year| ndt_noyear.clone().with_year(*year).ok())
            .collect()
    };

    if candidate_dates.is_empty() {
        return Err(parse_error! {
            message: format!("date `{ndt_noyear}` has no valid representation \
                              for year {mtime_year} +- 1, day of month \
                              or month must be invalid"),
            context: ndt_noyear.context.clone()
        })
    }

    for date in &candidate_dates {
        if max_seconds_around_mtime.contains(&seconds_since_mtime(*date)) {
            return Ok(*date)
        }
    }
    let s = candidate_dates.into_iter().map(|d| d.to_string()).collect::<Vec<_>>()
        .join(", ");
    Err(parse_error! {
        message: format!("date `{ndt_noyear}` candidates {s} are \
                          all too far removed from mtime `{mtime_ndt}`"),
        context: ndt_noyear.context.clone()
    })
}

#[cfg(test)]
mod test {
    use chj_rustbin::{time::naive_date_time_without_year::NaiveDateTimeWithoutYear,
                      parse::parse_error::NoContext};
    use chrono::{NaiveDateTime, NaiveDate};

    pub fn ndt(year: i32, month: u32, day: u32) -> NaiveDateTime {
        NaiveDate::from_ymd_opt(year, month, day).unwrap()
            .and_hms_opt(0,0,0).unwrap()
    }

    pub fn noyear(month: u8, day: u8) -> NaiveDateTimeWithoutYear<NoContext> {
        NaiveDateTimeWithoutYear {
            context: NoContext, month, day, hour: 0, minute: 0, second: 0
        }
    }
}

#[test]
fn t_add_year_from_mtime() {
    use test::{noyear, ndt};
    let t = add_year_from_mtime;
    let ok = |year, month, day| Ok(ndt(year, month, day));
    let range1 = -1 * MONTH_SECONDS .. 3 * MONTH_SECONDS;
    assert_eq!(t(&noyear(1, 30), ndt(2024, 2, 1), &range1),
               ok(2024, 1, 30));
    assert_eq!(t(&noyear(3, 30), ndt(2024, 2, 1), &range1),
               ok(2024, 3, 30));
    assert_eq!(t(&noyear(1, 30), ndt(2023, 12, 1), &range1),
               ok(2024, 1, 30));
    assert_eq!(t(&noyear(2, 28), ndt(2023, 12, 20), &range1),
               ok(2024, 2, 28));
    assert_eq!(t(&noyear(2, 28), ndt(2023, 12, 1), &range1),
               ok(2024, 2, 28));
    assert_eq!(t(&noyear(3, 1), ndt(2023, 12, 1), &range1).unwrap_err().message,
               "date `03-01 00:00:00` candidates 2023-03-01 00:00:00, \
                2024-03-01 00:00:00, 2022-03-01 00:00:00 are all too far removed \
                from mtime `2023-12-01 00:00:00`");
    assert_eq!(t(&noyear(12, 28), ndt(2024, 1, 15), &range1),
               ok(2023, 12, 28));
    assert_eq!(t(&noyear(12, 16), ndt(2024, 1, 15), &range1),
               ok(2023, 12, 16));
    assert_eq!(t(&noyear(12, 15), ndt(2024, 1, 15), &range1).unwrap_err().message,
               "date `12-15 00:00:00` candidates 2024-12-15 00:00:00, \
                2025-12-15 00:00:00, 2023-12-15 00:00:00 are all too far removed \
                from mtime `2024-01-15 00:00:00`");
    assert_eq!(t(&noyear(2, 30), ndt(2024, 2, 15), &range1).unwrap_err().message,
               "date `02-30 00:00:00` has no valid representation for year \
                2024 +- 1, day of month or month must be invalid");
}

/// Complete `noyear` with the year that makes it come after (or be
/// equal to) `base`, but if no year works so that it is max. 1 year
/// after it, returns an error (invalid month/day suspected, thus
/// won't even try the year after).
fn add_year_from_basetime<C: ParseContext + Clone>(
    noyear: &NaiveDateTimeWithoutYear<C>,
    base: NaiveDateTime,
) -> Result<NaiveDateTime, ParseError<C>> {
    let base_noyear = NaiveDateTimeWithoutYear::from(&base);
    match base_noyear.compare(&noyear) {
        Ordering::Less => noyear.clone().with_year(
            base.year()),
        Ordering::Equal => Ok(base),
        Ordering::Greater => noyear.clone().with_year(
            base.year().checked_add(1).expect("OS-limited")),
    }
}

#[test]
fn t_add_year_from_basetime() {
    use test::{noyear, ndt};
    let t = add_year_from_basetime;
    let ok = |year, month, day| Ok(ndt(year, month, day));
    
    assert_eq!(t(&noyear(3, 1), ndt(2024, 3, 1)), ok(2024, 3, 1));
    assert_eq!(t(&noyear(3, 7), ndt(2024, 3, 1)), ok(2024, 3, 7));
    assert_eq!(t(&noyear(12, 7), ndt(2024, 3, 1)), ok(2024, 12, 7));
    assert_eq!(t(&noyear(2, 7), ndt(2024, 3, 1)), ok(2025, 2, 7));
    assert_eq!(t(&noyear(2, 29), ndt(2024, 3, 1)).unwrap_err().message,
               "invalid month/day in year 2025");
}

/// A number that can be compared, to directly order jobs to choose
/// which one should be worked on.
pub fn priority_level<C: ParseContext + Clone>(
    priority: &Priority<C>,
    tasksize: TaskSize,
    key: &Option<DependencyKey>,
    mtime: SystemTime,
    now: NaiveDateTime,
) -> Result<f32, ParseError<C>> {
    let priority_for_time_left = |time_to_target: Duration| {
        let time_secs = time_to_target.num_seconds().max(0);
        let programming_secs_per_week = 7. * 60. * 60. * 24. * 4.;
        let time_weeks: f32 = (time_secs as f32) / programming_secs_per_week * 5.; // XXXXHACK

        let task_size_weeks = tasksize.in_weeks();

        priority_level_for(time_weeks, task_size_weeks)
    };
    let modify_priority_with_mtime = |prio| {
        // the older the file, the lower the priority? (XX Except
        // after a month old or so, spike the priority. Always? Will
        // probably be spammy.)
        let mtime_ndt = naive_from_systemtime(mtime);
        let age = now.signed_duration_since(mtime_ndt);
        let age_sec = age.num_seconds() as f32;
        // prio * f32::powf(0.9999, age_sec)
        // but, prio itself is also in logarithmic space? So: for
        // every week, priority sinks by 1 point.
        prio + age_sec * (1. / (60. * 60. * 24. * 7.))
    };
    match priority {
        Priority::Date(ndtwowy) => {
            let target_date =
                match ndtwowy {
                    NaiveDateTimeWithOrWithoutYear::NaiveDateTime(ndt) => *ndt,
                    NaiveDateTimeWithOrWithoutYear::NaiveDateTimeWithoutYear(ndt_noyear) => {
                        let from_mtime = || add_year_from_mtime(ndt_noyear,
                                                                naive_from_systemtime(mtime),
                                                                &MAX_SECONDS_AROUND_MTIME);
                        if let Some(key) = key {
                            match key {
                                DependencyKey::String(_) => T!(from_mtime())?,
                                DependencyKey::NaiveDateTime(ndt_file) =>
                                    T!(add_year_from_basetime(ndt_noyear, *ndt_file))?
                            }
                        } else {
                            T!(from_mtime())?
                        }
                    }
                };
            
            let time_to_target: Duration = target_date
                .signed_duration_since(now);
            Ok(priority_for_time_left(time_to_target))
        }
        Priority::DaysFromToday(num_days) => {
            // (XX Base on noon on target day, too ?)
            Ok(priority_for_time_left(Duration::days((*num_days).into())))
        }
        Priority::Today => {
            let target_time = now
                .with_hour(12).unwrap()
                .with_minute(0).unwrap()
                .with_second(0).unwrap();
            Ok(priority_for_time_left(target_time.signed_duration_since(now)))
        }
        Priority::Tonight => {
            let target_time = now
                .with_hour(18).unwrap()
                .with_minute(0).unwrap()
                .with_second(0).unwrap();
            Ok(priority_for_time_left(target_time.signed_duration_since(now)))
        }
        // XX: what about mixing priorities, target date given *and* a priority level?
        Priority::Level(l) => Ok(modify_priority_with_mtime(f32::from(*l))),
        Priority::Ongoing => Ok(modify_priority_with_mtime(5.)), // ?
        Priority::Unknown => Ok(modify_priority_with_mtime(5.)), // ?
    }
}


impl<C: ParseContext> Default for Priority<C> {
    fn default() -> Self {
        Self::Unknown
    }
}

// If you're looking for an `impl From<NaiveDateTime> for Priority`,
// just use `Priority::Date(ndt)`.

impl<'s, B: Backing + Debug> FromParseableStr<'s, B> for Priority<StringParseContext<&'s B>>
    where &'s B: Backing
{
    fn from_parseable_str(
        s: &ParseableStr<'s, B>
    ) -> Result<Self, ParseError<StringParseContext<&'s B>>> {
        let s = s.trim();
        let ss = s.s;
        if ss == "ongoing" {
            // Note: now have `inside_parse_class`, too. XX inconsistent?
            return Ok(Priority::Ongoing);
        }
        if let Ok(level) = ManualPriorityLevel::from_str(ss) {
            return Ok(Priority::Level(level));
        }
        if let Ok((ndt_wowy, rest)) =
            parse_dat(
                &s.trim(),
                &flexible_parse_dat_options(
                    Some(NaiveTime::from_hms_opt(12, 0, 0).unwrap())))
        {
            let rest = rest.drop_whitespace();
            if rest.is_empty() {
                return Ok(Priority::Date(ndt_wowy));
            } else {
                return Err(parse_error! {
                    message: format!("garbage after date/time"),
                    context: rest.into()
                });
            }
        }
        Err(parse_error! {
            message: format!("invalid priority string {:?} \
                              (valid are 'ongoing', 1..99, 2019-12-11 style date (and time))",
                             s.s),
            context: s.into()
        })
    }
}

#[derive(Default, Debug, Clone)]
struct Dependencies {
    // XX Arc for clone efficiency?
    keys: BTreeSet<DependencyKey>,
}

impl Dependencies {
    // XX move this into `impl ParseableStr for Dependencies`!
    pub fn from_parseable_str<'s, B: Backing + Debug>(
        s: &ParseableStr<'s, B>
    ) -> Result<Self, ParseError<StringParseContext<&'s B>>>
        where &'s B: Backing
    {
        let mut keys = BTreeSet::new();
        for s in s.split_str(",", true)
            .map(|s| s.trim())
            .filter(|s| !s.is_empty()) // optim: only the *last* segment can be empty
        {
            keys.insert(
                (
                    || -> Result<Option<DependencyKey>, ParseError<_>> {
                        let (d, rest) =
                            // It's OK to fail parsing here as we've got a
                            // fallback.
                            match parse_dat(&s, &PARSE_DAT_OPTIONS_FOR_INSIDE) {
                                Ok(v) => v,
                                Err(_) => return Ok(None)
                            };
                        let ndt = d.into_naive_date_time()?;
                        let dk = DependencyKey::from(ndt);
                        // Given we succeeded at getting a date, from
                        // now on we report errors.
                        if keys.contains(&dk) {
                            Err(parse_error! {
                                message: format!("duplicate dependency entry {ndt}"),
                                context: (&s).into()
                            })?
                        }
                        T!(expect_str_or_eos(rest, "-"))?;
                        Ok(Some(dk))
                    }
                )()?.unwrap_or_else(
                    || {
                        // We take the string as is. If the user meant
                        // it to be a date after all, they should get
                        // an error report later when the dependency
                        // isn't found.
                        DependencyKey::from(s.s)
                    }
                ));
        }
        Ok(Dependencies { keys })
    }
}

impl Deref for Dependencies {
    type Target = BTreeSet<DependencyKey>;

    fn deref(&self) -> &Self::Target {
        &self.keys
    }
}


#[derive(Debug, Clone)]
struct TaskInfoDeclarations<C: ParseContext> {
    tasksize: TaskSize,
    priority: Priority<C>,
    importance: Importance,
    dependencies: Dependencies,
}

// Like with TaskInfoDeclarationsBuilder, derive cannot be used since
// it erroneously requires C to impl Default.
impl<C: ParseContext> Default for TaskInfoDeclarations<C> {
    fn default() -> Self {
        Self {
            tasksize: Default::default(),
            priority: Default::default(),
            importance: Default::default(),
            dependencies: Default::default(),
        }
    }
}

impl<'s, B: Backing> IntoOwningBacking<B::Owned>
    for TaskInfoDeclarations<StringParseContext<&'s B>>
where
    &'s B: Backing,
    B::Owned: Backing
{
    type Owning = TaskInfoDeclarations<StringParseContext<B::Owned>>;

    fn into_owning_backing(self) -> Self::Owning {
        let TaskInfoDeclarations { tasksize, priority, importance, dependencies } = self;
        TaskInfoDeclarations {
            tasksize,
            priority: priority.into_owning_backing(),
            importance,
            dependencies,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum WorkflowStatus {
    None,

    Open,
    Done,
    Obsolete,
    Wontfix,

    Dupe,
    Future,

    Notforme,
    Applied,
    Active,
    Rejected,
}

impl WorkflowStatus {
    // XX use the library for this pls.
    const INSTANCES: &'static [WorkflowStatus] = {
        use WorkflowStatus::*;
        &[
            None,

            Open,
            Done,
            Obsolete,
            Wontfix,

            Dupe,
            Future,

            Notforme,
            Applied,
            Active,
            Rejected,
        ]
    };

    /// Returns multiple entries if there are aliases, 0 entry for
    /// `WorkflowStatus::None`.
    pub fn uppercase_strs(self) -> &'static[&'static str] {
        match self {
            WorkflowStatus::None => &[],
            WorkflowStatus::Open => &["OPEN", "TODO"],
            WorkflowStatus::Done => &["DONE"],
            WorkflowStatus::Obsolete => &["OBSOLETE"],
            WorkflowStatus::Wontfix => &["WONTFIX"],
            WorkflowStatus::Dupe => &["DUPE"],
            WorkflowStatus::Future => &["FUTURE"],
            WorkflowStatus::Notforme => &["NOTFORME"],
            WorkflowStatus::Applied => &["APPLIED"],
            WorkflowStatus::Active => &["ACTIVE"],
            WorkflowStatus::Rejected => &["REJECTED"],
        }
    }
    pub fn is_active(self) -> bool {
        match self {
            WorkflowStatus::None => false,
            WorkflowStatus::Open => true,
            WorkflowStatus::Done => false,
            WorkflowStatus::Obsolete => false,
            WorkflowStatus::Wontfix => false,
            WorkflowStatus::Dupe => false, // ?
            WorkflowStatus::Future => true, // ?
            WorkflowStatus::Notforme => false,
            WorkflowStatus::Applied => true, // ?
            WorkflowStatus::Active => true,
            WorkflowStatus::Rejected => false,
        }
    }
}

impl FromStr for WorkflowStatus {
    type Err = ();

    /// Translate a full match on a string (a directory name) to a
    /// `WorkflowStatus`.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // This match just exists as a guard to remind you to add a
        // case below if the enum changes.
        match WorkflowStatus::None {
            WorkflowStatus::None => (),
            WorkflowStatus::Open => (),
            WorkflowStatus::Done => (),
            WorkflowStatus::Obsolete => (),
            WorkflowStatus::Wontfix => (),
            WorkflowStatus::Dupe => (),
            WorkflowStatus::Future => (),
            WorkflowStatus::Notforme => (),
            WorkflowStatus::Applied => (),
            WorkflowStatus::Active => (),
            WorkflowStatus::Rejected => (),
        }
        match s {
            // This should never be used as a folder name, since need
            // the `OPEN` string in the file name to introduce the `{
            // .. }` section anyway.
            // "open" => Ok(WorkflowStatus::Open),

            "done" => Ok(WorkflowStatus::Done),
            "obsolete" => Ok(WorkflowStatus::Obsolete),
            "wontfix" => Ok(WorkflowStatus::Wontfix),

            "dupe" => Ok(WorkflowStatus::Dupe),
            "future" => Ok(WorkflowStatus::Future),

            "notforme" => Ok(WorkflowStatus::Notforme),
            "applied" => Ok(WorkflowStatus::Applied),
            "active" => Ok(WorkflowStatus::Active),
            "rejected" => Ok(WorkflowStatus::Rejected),

            _ => Err(())
        }
    }
}

impl TryFrom<&Path> for WorkflowStatus {
    type Error = ();

    /// Go through the path segments of the parent directory of the
    /// path (from the right) to decide on the `WorkflowStatus`. Only
    /// directory names fully matching a status name are considered.
    fn try_from(path: &Path) -> Result<Self, Self::Error> {
        let mut anc = path.ancestors();
        let _ = anc.next(); // path itself
        for dir in anc {
            if let Some(file_name) = dir.file_name() {
                match file_name.to_str() {
                    Some(segment) =>
                        if let Ok(status) = WorkflowStatus::from_str(segment) {
                            return Ok(status);
                        },
                    None => warning!("cannot decode file name {file_name:?} \
                                      in path {dir:?} to string")
                }
            }
        }
        Err(())
    }
}

// Not implementing `Hash` to avoid accidentally introducing
// undeterminsim (in particular, trace output order).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DependencyKey {
    String(KString),
    NaiveDateTime(NaiveDateTime),
}

// Order String before NaiveDateTime

impl PartialOrd for DependencyKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self {
            DependencyKey::String(s1) => match other {
                DependencyKey::String(s2) => s1.partial_cmp(s2),
                DependencyKey::NaiveDateTime(_) => Some(Ordering::Less)
            }
            DependencyKey::NaiveDateTime(t1) => match other {
                DependencyKey::String(_) => Some(Ordering::Greater),
                DependencyKey::NaiveDateTime(t2) => t1.partial_cmp(t2)
            }
        }
    }
}

impl Ord for DependencyKey {
    fn cmp(&self, other: &Self) -> Ordering {
        match self {
            DependencyKey::String(s1) => match other {
                DependencyKey::String(s2) => s1.cmp(s2),
                DependencyKey::NaiveDateTime(_) => Ordering::Less
            }
            DependencyKey::NaiveDateTime(t1) => match other {
                DependencyKey::String(_) => Ordering::Greater,
                DependencyKey::NaiveDateTime(t2) => t1.cmp(t2)
            }
        }
    }
}


impl From<NaiveDateTime> for DependencyKey {
    fn from(d: NaiveDateTime) -> Self {
        DependencyKey::NaiveDateTime(d)
    }
}

impl From<&str> for DependencyKey {
    fn from(d: &str) -> Self {
        DependencyKey::String(KString::from_ref(d))
    }
}

#[derive(Debug, Clone)]
struct TaskInfo<C: ParseContext + Clone> {
    /// Real primary key (unlike `dependency_key` which isn't always available)
    id: usize,
    /// Path to the file
    path: PathBuf,
    /// The state with regards to the workflow that the task is in, as
    /// determined by the presence of 'OPEN' in the file name but
    /// absence of one of the overriding status names in one of the
    /// parent dirs, or if present the latter, from the directory
    /// segment closest to the file.
    workflow_status: WorkflowStatus,
    /// The key to use when referring to this entry as dependency.
    dependency_key: Option<DependencyKey>,
    /// The current modification time.
    mtime: SystemTime,
    /// The priority calculated based on the current time, and on
    /// dependant tasks (priority inheritance).  Set by
    /// `set_initial_priority_level_for` and
    /// `set_as_dependency_for_priority`.
    calculated_priority: Cell<Option<f32>>,
    /// The information specified by the user after the `OPEN` (or,
    /// FUTURE, also `TODO`?) between curly braces.
    declarations: TaskInfoDeclarations<C>,
}

impl<C: ParseContext + Clone> TaskInfo<C> {
    fn priority_level_at(&self, now: NaiveDateTime) -> Result<f32, ParseError<C>> {
        priority_level(&self.declarations.priority,
                       self.declarations.tasksize,
                       &self.dependency_key,
                       self.mtime,
                       now)
        // HACK, proper place?
            .map(|level| level + (
                (self.declarations.importance.level as i32 - DEFAULT_IMPORTANCE_LEVEL as i32) * 2
            ) as f32)
    }

    fn set_initial_priority_level_for(&self, now: NaiveDateTime) -> Result<(), ParseError<C>> {
        assert!(self.calculated_priority.get().is_none());
        self.calculated_priority.set(Some(T!(self.priority_level_at(now))?));
        Ok(())
    }

    fn calculated_priority(&self) -> f32 {
        self.calculated_priority.get().expect(
            "calculated_priority field has been filled via a call to \
             `set_initial_priority_level_for` earlier")
    }

    fn set_as_dependency_for_priority(&self, priority: f32) {
        let own_prio = self.calculated_priority.get().unwrap();
        let target_prio = priority - 0.1;
        if own_prio > target_prio {
            self.calculated_priority.set(Some(target_prio));
        }
    }
}

// derive is rediculous, requiring us to require Clone here for no
// reason.
impl<C: ParseContext + Clone> PartialEq for TaskInfo<C> {
    fn eq(&self, _other: &Self) -> bool {
        false // f32 are never eq?  or do we have to ?
    }
}

impl<C: ParseContext + Clone> PartialOrd for TaskInfo<C> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.calculated_priority.get().unwrap().partial_cmp(
            &other.calculated_priority.get().unwrap())
    }
}

struct TaskInfoDeclarationsBuilder<C: ParseContext> {
    tasksize: OnceCell<TaskSize>,
    priority: OnceCell<Priority<C>>,
    importance: OnceCell<Importance>,
    dependencies: OnceCell<Dependencies>,
}

// Can't *derive* Defaul due to it requiring C to implement Default,
// too (just why? even Priority<C>'s default() works without it!),
// thus:
impl<C: ParseContext> Default for TaskInfoDeclarationsBuilder<C> {
    fn default() -> Self {
        Self {
            tasksize: Default::default(),
            priority: Default::default(),
            importance: Default::default(),
            dependencies: Default::default(),
        }
    }
}

macro_rules! def_builder_setter {
    { $method_name:ident, $field_name:ident, $field_type:ty } => {
        fn $method_name(&self, val: $field_type, came_from: &ParseableStr<'s, B>)
                        -> Result<(), ParseError<StringParseContext<&'s B>>> {
            if let Some(old) = &self.$field_name.get() {
                return Self::old_err(format!("{old:?}"), came_from);
            }
            self.$field_name.set(val).expect("same setter only called once");
            Ok(())
        }
    }
}

impl<'s, B: Backing + Debug> TaskInfoDeclarationsBuilder<StringParseContext<&'s B>>
    where &'s B: Backing + Debug
{
    fn old_err(
        old: String,
        came_from: &ParseableStr<'s, B>
    ) -> Result<(), ParseError<StringParseContext<&'s B>>>
    {
        Err(parse_error! {
            message: format!("key {:?} occurred before with value {old}", came_from.s),
            context: came_from.into()
        })
    }

    def_builder_setter!(set_tasksize, tasksize, TaskSize);
    def_builder_setter!(set_priority, priority, Priority<StringParseContext<&'s B>>);
    def_builder_setter!(set_dependencies, dependencies, Dependencies);
    def_builder_setter!(set_importance, importance, Importance);
}

impl<C: ParseContext> From<TaskInfoDeclarationsBuilder<C>> for TaskInfoDeclarations<C> {
    fn from(value: TaskInfoDeclarationsBuilder<C>) -> Self {
        let tasksize = value.tasksize.into_inner().unwrap_or_default();
        let priority = value.priority.into_inner().unwrap_or_default();
        let importance = value.importance.into_inner().unwrap_or_default();
        let dependencies = value.dependencies.into_inner().unwrap_or_default();
        Self { priority, importance, dependencies, tasksize }
    }
}

// fn is_word_char(c: char) -> bool {
//     c == '_' || c == '-' || c.is_ascii_alphanumeric()
// }

fn is_ascii_digit_char(c: char) -> bool {
    c.is_ascii_digit()
}

fn expect_str_or_eos<'s, B: Backing>(
    s: ParseableStr<'s, B>,
    needle: &'s str
) -> Result<ParseableStr<'s, B>, ParseError<StringParseContext<&'s B>>>
    where &'s B: Backing
{
    s.drop_whitespace().expect_str_or_eos(needle)
        .map(|v| v.drop_whitespace())
        .map_err(compose(ParseError::from,
                         |e| e.message_append(" or the end of the input segment")))
}

fn expect_comma_or_eos<'s, B: Backing>(
    s: ParseableStr<'s, B>
) -> Result<ParseableStr<'s, B>, ParseError<StringParseContext<&'s B>>>
    where &'s B: Backing
{
    T!(expect_str_or_eos(s, ","))
}

// The string representing one value after a key.  It is either a
// single word/integer followed by a comma / whitespace / eos, or [ ]
// with the same (but the trailing comma will be omitted, XX currently
// not), followed by the same. Even in "list" case, the [ ] are not
// part of it, `[single_value]` and `single_value` are treated the
// same. "List" context expects at least one value.
fn take_one_value_from<'s, B: Backing>(
    s: ParseableStr<'s, B>
) -> Result<(ParseableStr<'s, B>, ParseableStr<'s, B>), ParseError<StringParseContext<&'s B>>>
    where &'s B: Backing
{
    let mut rest = s.clone();
    rest = rest.drop_whitespace();
    if let Some(mut rest) = rest.drop_str("[") {
        // "list"
        rest = rest.drop_whitespace();
        let start = rest.clone();
        loop {
            // first expect a value -- ok, a bit of a joke, could
            // simply scan all the way to ] then.
            (_, rest) = rest.take_while(|c| c != ',' && c != ']');
            if rest.is_empty() {
                return Ok((start.up_to(&rest), rest));
            }
            if let Ok(s) = rest.expect_str(",") {
                rest = s;
            } else if let Ok(after) = rest.expect_str("]") {
                let after = T!(expect_comma_or_eos(after))?;
                return Ok((start.up_to(&rest), after));
            } else {
                return Err(parse_error! {
                    // or eos, but in context? "or '}'"
                    message: "expecting ',' or ']'".into(),
                    context: rest.into(),
                });
            }
            rest = rest.drop_whitespace();
        }
    } else {
        // single value
        let (value, rest) = s.take_while(|c| c != ',');
        let rest = T!(expect_comma_or_eos(rest))?;
        Ok((value.trim(), rest))
    }
}

#[cfg(test)]
#[test]
fn t_take_one_value_from() {
    let backing = String::new();
    let t = |s| take_one_value_from(ParseableStr { backing: &backing, position: 0, s });
    let ok = |p1, s1, p2, s2| Ok((ParseableStr { backing: &backing, position: p1, s: s1 },
                                  ParseableStr { backing: &backing, position: p2, s: s2 }));
    // let err = |position, msg: &str| Err(parse_error! { message: msg.into(), position });

    assert_eq!(t(" 1, b"), ok(1, "1", 4, "b"));
    assert_eq!(t("1 , b"), ok(0, "1", 4, "b"));
    assert_eq!(t(" 1"), ok(1, "1", 2, ""));
    assert_eq!(t(" 12"), ok(1, "12", 3, ""));
    assert_eq!(t("Abcd_X"), ok(0, "Abcd_X", 6, ""));
    assert_eq!(t("Abcd_X "), ok(0, "Abcd_X", 7, ""));
    assert_eq!(t("1  b"), ok(0, "1  b", 4, ""));
    assert_eq!(t("  "), ok(2, "", 2, ""));
    assert_eq!(t(" [a] "), ok(2, "a", 5, ""));
    assert_eq!(t(" [a,b] "), ok(2, "a,b", 7, ""));
    assert_eq!(t(" [a b] "), ok(2, "a b", 7, ""));
    assert_eq!(t(" [a, b] "), ok(2, "a, b", 8, ""));
    assert_eq!(t(" [ a , b ] "), ok(3, "a , b ", 11, ""));
}

// Tries to parse a key/value pair, returns the rest after the value
// (which includes the comma and further arguments, to be handled by
// the caller)
fn inside_parse_key_val<'s, B: Backing + Debug>(
    builder: &TaskInfoDeclarationsBuilder<StringParseContext<&'s B>>,
    ident: &ParseableStr<'s, B>,
    rest: &ParseableStr<'s, B>,
) -> Result<ParseableStr<'s, B>, ParseError<StringParseContext<&'s B>>>
    where &'s B: Backing
{
    if let Some(rest) = rest.drop_str(":") {
        macro_rules! parse_to {
            { $setter:ident, $type:ident } => {{
                let (value, rest) = T!(take_one_value_from(rest))?;
                builder.$setter($type::from_parseable_str(&value)?, &value)?;
                Ok(rest)
            }}
        }

        match ident.s {
            "s" | "size" | "tasksize" => parse_to!(set_tasksize, TaskSize),
            // "due" should only accept a date?
            "p" | "prio" | "priority" | "due" => parse_to!(set_priority, Priority),
            "d" | "dep" | "depends" | "dependencies" => parse_to!(set_dependencies, Dependencies),
            "i" | "imp" | "importance" => parse_to!(set_importance, Importance),

            _ => Err(parse_error! {
                message: format!("unknown key {:?}", ident.s),
                context: ident.into()
            })
        }
    } else {
        Err(parse_error! {
            message: "missing ':' after possible keyword".into(),
            context: rest.into(),
        })
    }
}

// XX move to an enum with library support.
const INSIDE_PARSE_CASES: &[&str] = &[
    "chore",
    "decide",
    "occasionally",
    "relaxed",
    "today",
    "tonight",
    "ongoing",
];

// too hard to make const
fn assert_inside_parse_case(s: &str) {
    let mut cases = INSIDE_PARSE_CASES;
    while !cases.is_empty() {
        let case = cases[0];
        if case == s {
            return;
        }
        cases = &cases[1..];
    }
    panic!("invalid case {:?}", s);
}

// Attempt to parse an identifier as a 'class' name; nothing must come
// after `ident` (except comma or further arguments, to be checked by
// the caller). Returns the values to be set.
fn inside_parse_class<'t, B: Backing, C: ParseContext>(
    ident: &ParseableStr<'t, B>,
) -> Option<(TaskSize, Priority<C>)>
{
    match ident.s {
        // Special keys that have no values

        // The idea with some of these was rather
        // to make classes of things; but yeah,
        // the only relevant information out of
        // this is priority (and size).

        "chore" => {
            assert_inside_parse_case(ident.s);
            // Pick it up at times when doing mind
            // numbing things. Small.
            Some((TaskSize::Minutes, Priority::Level(7)))
        }
        "decide" => {
            assert_inside_parse_case(ident.s);
            // Something small to decide on; but
            // somewhat higher priority since it
            // is still probably somewhat time
            // relevant (things in the future
            // depend on the decision).
            Some((TaskSize::Minutes, Priority::Level(4)))
        }
        "occasionally" => {
            assert_inside_parse_case(ident.s);
            // Pick it up when the right occasion
            // or state of mind happens.
            Some((TaskSize::Hours, Priority::Level(7)))
        }
        "relaxed" => {
            assert_inside_parse_case(ident.s);
            // Things to do at some particular
            // time of week. Or as a filler? When
            // feeling relaxed?
            Some((TaskSize::Hours, Priority::Level(8)))
        }
        "today" => {
            assert_inside_parse_case(ident.s);
            // Things to do today. Or if missed,
            // the next day..
            Some((TaskSize::Hours, Priority::Today))
        }
        "tonight" => {
            assert_inside_parse_case(ident.s);
            // Things to do before going home or on the way.
            Some((TaskSize::Minutes, Priority::Tonight))
        }
        "ongoing" => {
            assert_inside_parse_case(ident.s);
            // Hmm, no due date, no priority? Or,
            // just expecting to work on this
            // 20-50% every (day or) week until
            // done? But I did give it a special
            // priority! -- XX should I just try
            // Priority::from? Actually try all
            // parsers in some sequence?
            Some((TaskSize::Weeks, Priority::Ongoing))
        }
        _ => None
    }
}

// Expect non-key-value-pair values which aren't one of the constant
// 'class' strings.
fn inside_parse_variable<'s, B: Backing + Debug>(
    builder: &TaskInfoDeclarationsBuilder<StringParseContext<&'s B>>,
    p: &ParseableStr<'s, B>,
) -> Result<ParseableStr<'s, B>, ParseError<StringParseContext<&'s B>>>
    where &'s B: Backing + Debug
{
    let mut part;
    let rest;
    if let Some((part_, rest_)) = p.split_at_str(",") {
        part = part_;
        rest = rest_;
    } else {
        part = p.clone();
        rest = p.eos();
    };
    part = part.trim();

    if let Some(prio) = part.opt_parse::<ManualPriorityLevel>() {
        T!(builder.set_priority(Priority::Level(prio), &part))?;
    } else {
        let ndt = T!(parse_date_time_argument(&part, true))?;
        T!(builder.set_priority(Priority::Date(ndt), &part))?;
    }
    Ok(rest)
}

// Receives the whole string between '{' and '}'
fn parse_inside<'s, B: Backing + Debug>(
    s: &ParseableStr<'s, B>
) -> Result<TaskInfoDeclarations<StringParseContext<&'s B>>,
            ParseError<StringParseContext<&'s B>>>
where &'s B: Backing
{
    let builder = TaskInfoDeclarationsBuilder::default();
    let mut p = s.clone();
    loop {
        p = p.drop_whitespace();
        match p.take_identifier() {
            Ok((ident, rest)) => {
                if rest.starts_with(":") {
                    p = T!(inside_parse_key_val(&builder, &ident, &rest))?;
                } else if let Ok(rest) = expect_comma_or_eos(rest) {
                    // The ident is a single value, not part of key-value pair
                    if let Some((task_size, priority)) = inside_parse_class(&ident) {
                        T!(builder.set_tasksize(task_size, &ident))?;
                        T!(builder.set_priority(priority, &ident))?;
                        p = rest;
                    } else {
                        // Are there any identifiers that are not a
                        // class? 
                        // p = inside_parse_variable(&mut builder, p)?
                        // But so far no (integer priorities are not identifiers), thus:    
                        return Err(parse_error! {
                            message: format!(
                                "unknown class {:?}, known are: {INSIDE_PARSE_CASES:?}",
                                ident.s
                            ),
                            context: ident.into()
                        });
                    }
                } else {
                    // "ident non-colon": 
                    // p = inside_parse_variable(&mut builder, p)?
                    // currently not valid, even a date always starts
                    // with a number, not identifier, thus:
                    return Err(parse_error! {
                        message: format!(
                            "expecting a class (identifier), keyword, date or priority"),
                        context: ident.into()
                    });
                }
            }
            Err(_e) => if p.is_empty() {
                return Ok(builder.into());
            } else {
                // Not an identifier, so might be a date or number or
                // similar non-enumerated thing.
                p = T!(inside_parse_variable(&builder, &p))?
            }
        }
    }
}

struct ParseDatOptions {
    /// If None, the time must be present in the input string
    default_time: Option<NaiveTime>,
    weekday_is_optional: bool,
    /// Separator between year, month and mday
    date_separator: Separator,
    /// Separator between hour, minute and seconds
    time_separator: Separator,
    /// Separator between date and time as well as between time and wday
    separator_between_parts: Separator,
}

enum ParseTimeWdayErrorKind {
    NoProperTime,
    NoProperWeekday,
}

// This is really the continuation of parsing the date part of a
// datetime value; it expects a separator at first. Returns (hour,
// minute, second) along weekday info and the input rest.
fn parse_time_wday<'s, B: Backing>(
    s: ParseableStr<'s, B>,
    options: &ParseDatOptions,
) -> Result<((u8, u8, u8), Option<(Weekday, usize)>, ParseableStr<'s, B>),
            (ParseTimeWdayErrorKind, ParseError<StringParseContext<&'s B>>)>
    where &'s B: Backing
{
    let ((hour, minute, second), rest) =
        (|| -> Result<((u8, u8, u8), ParseableStr<'s, B>),
                      ParseError<StringParseContext<&'s B>>> {
            let rest = T!(s.expect_separator(&options.separator_between_parts))?;

            let (digits, rest_after_digits) = rest.take_while(is_ascii_digit_char);
            let is_separator_less = match digits.len() {
                4 | 6 => true,
                _ => false
            };
            let ((hh, mm, opt_ss), rest) = if is_separator_less {
                if options.time_separator.required {
                    // XX actually take ':' from time_separator
                    return Err(parse_error! {
                            message: format!("got {} digits where time is expected, but expecting ':' \
                                              separator between digit pairs", digits.len()),
                            context: digits.into(),
                    });
                }
                let (hh, rest) = digits.split_at(2);
                let (mm, rest) = rest.split_at(2);
                match digits.len() {
                    4 => {
                        assert!(rest.is_empty());
                        ((hh, mm, None), rest_after_digits)
                    }
                    6 => {
                        let (ss, rest) = rest.split_at(2);
                        assert!(rest.is_empty());
                        ((hh, mm, Some(ss)), rest_after_digits)
                    }
                    _ => unreachable!()
                }
            } else {
                let msg = "digit as part of time";
                let (hh, rest) = T!(rest.take_nrange_while(1, 2, is_ascii_digit_char, msg))?;
                let rest = T!(rest.expect_separator(&options.time_separator))?;
                let (mm, rest) = T!(rest.take_n_while(2, is_ascii_digit_char, msg))?;
                let rest = T!(rest.expect_separator(&options.time_separator))?;
                if let Some((ss, rest)) = rest.take_n_while(2, is_ascii_digit_char, msg).ok() {
                    ((hh, mm, Some(ss)), rest)
                } else {
                    ((hh, mm, None), rest)
                }
            };

            let hour = u8::from_str(hh.s).map_err(|e| parse_error! {
                message: format!("can't parse hour {:?}: {e}", hh.s),
                context: hh.into()
            })?;
            let minute = u8::from_str(mm.s).map_err(|e| parse_error! {
                message: format!("can't parse minute {:?}: {e}", mm.s),
                context: mm.into()
            })?;
            let second = if let Some(ss) = opt_ss {
                u8::from_str(ss.s).map_err(|e| parse_error! {
                    message: format!("can't parse second {:?}: {e}", ss.s),
                    context: ss.into()
                })?
            } else {
                0
            };
            Ok(((hour, minute, second), rest))
        })().map_err(|e| (ParseTimeWdayErrorKind::NoProperTime, e))?;

    let time = (hour, minute, second);

    let weekday_result = (|| -> Result<((Weekday, usize), ParseableStr<_>), ParseError<_>> {
        // "_Sun"
        let rest = T!(rest.expect_separator(&options.separator_between_parts))?;
        let (wdaystr, rest) = rest.take_while(|c: char| c.is_ascii_alphabetic());
        match wdaystr.len() {
            2 | 3 | 4 | 5 | 6 | 7 | 8 => (),
            _ => Err(parse_error! { message: format!("no match for weekday name"),
                                    context: (&wdaystr).into() })?
        }
        let wday = Weekday::from_str(wdaystr.s).map_err(
            |e| parse_error! { message: format!("unknown weekday name {:?}: {e}", wdaystr.s),
                               context: (&wdaystr).into() })?;
        Ok(((wday, wdaystr.position), rest))
    })();
    match weekday_result {
        Ok((weekday_and_position, rest)) => Ok((time, Some(weekday_and_position), rest)),
        Err(e) => if options.weekday_is_optional {
            Ok((time, None, rest))
        } else {
            Err((ParseTimeWdayErrorKind::NoProperWeekday, e))
        }
    }
}

/// "09-29_205249_Sun"; does not verify weekday (just returns it)
/// because it doesn't have the year. If `weekday_is_optional` is
/// true, still parse weekday if present, but do not report match
/// failures.
fn parse_dat_without_year<'s, B: Backing + Debug>(
    s: &ParseableStr<'s, B>,
    options: &ParseDatOptions,
) -> Result<
        (NaiveDateTimeWithoutYear<StringParseContext<&'s B>>,
         Option<(Weekday, usize)>,
         ParseableStr<'s, B>),
    ParseError<StringParseContext<&'s B>>>
where &'s B: Backing
{
    let rest = s;
    let (month, rest) = T!(rest.take_nrange_while(
        1, 2, is_ascii_digit_char, "digit as part of month number"))?;
    let month = u8::from_str(month.s).map_err(|e| parse_error! {
        message: format!("can't parse month {:?}: {e}", month.s),
        context: month.into()
    })?;
    let rest = T!(rest.expect_separator(&options.date_separator))?;
    let (day, rest) = T!(rest.take_nrange_while(
        1, 2, is_ascii_digit_char, "digit as part of day number"))?;
    let day = u8::from_str(day.s).map_err(|e| parse_error! {
        message: format!("can't parse day {:?}: {e}", day.s),
        context: day.into()
    })?;

    // Try parsing the time; it is allowed to fail if
    // options.time_is_optional is true, but if time succeeds and
    // options.weekday_is_optional is false and weekday fails, then
    // the whole thing should fail, too!
    match parse_time_wday(rest.clone(), options) {
        Ok(((hour, minute, second), opt_weekday_position, rest)) => {
            let ndt_no_year = NaiveDateTimeWithoutYear {
                context: s.into(),
                month,
                day,
                hour,
                minute,
                second,
            };
            Ok((ndt_no_year, opt_weekday_position, rest))
        }
        Err((ParseTimeWdayErrorKind::NoProperTime, _e)) => {
            if let Some(dt) = options.default_time {
                let ndt_no_year = NaiveDateTimeWithoutYear {
                    context: s.into(),
                    month,
                    day,
                    hour: dt.hour() as u8,
                    minute: dt.minute() as u8,
                    second: dt.second() as u8,
                };
                Ok((ndt_no_year, None, rest))
            } else {
                Err(parse_error! {
                    message: format!("time is required but missing after {:?}",
                                     s.up_to(&rest).s),
                    context: rest.into()
                })
            }
        },
        Err((ParseTimeWdayErrorKind::NoProperWeekday, e)) => T!(Err(e))
    }
}

#[derive(Debug, Clone)]
pub enum NaiveDateTimeWithOrWithoutYear<C: ParseContext> {
    NaiveDateTime(NaiveDateTime),
    NaiveDateTimeWithoutYear(NaiveDateTimeWithoutYear<C>),
}

impl<'s, B: Backing> IntoOwningBacking<B::Owned>
    for NaiveDateTimeWithOrWithoutYear<StringParseContext<&'s B>>
where &'s B: Backing,
      B::Owned: Backing
{
    type Owning = NaiveDateTimeWithOrWithoutYear<StringParseContext<B::Owned>>;

    fn into_owning_backing(self) -> Self::Owning {
        use NaiveDateTimeWithOrWithoutYear::*;
        match self {
            NaiveDateTime(v) => NaiveDateTime(v),
            NaiveDateTimeWithoutYear(ndt_noyear) =>
                NaiveDateTimeWithoutYear(ndt_noyear.into_owning_backing())
        }
    }
}

impl<C: ParseContext> NaiveDateTimeWithOrWithoutYear<C> {
    pub fn into_naive_date_time(self) -> Result<NaiveDateTime, ParseError<C>> {
        match self {
            NaiveDateTimeWithOrWithoutYear::NaiveDateTime(ndt) => Ok(ndt),
            NaiveDateTimeWithOrWithoutYear::NaiveDateTimeWithoutYear(ndtwy) =>
                ndtwy.into_naive_date_time()
        }
    }
}

// "2024-09-29_205249_Sun" -- XX update
fn parse_dat<'s, B: Backing + Debug>(
    s: &ParseableStr<'s, B>,
    options: &ParseDatOptions,
) -> Result<(NaiveDateTimeWithOrWithoutYear<StringParseContext<&'s  B>>,
             ParseableStr<'s, B>),
            ParseError<StringParseContext<&'s B>>>
where &'s B: Backing
{
    match T!(s.take_n_while(4, is_ascii_digit_char, "digit as part of year number")) {
        Ok((year, rest)) => {
            let rest = T!(rest.expect_separator(&options.date_separator))?;
            let (datetime_no_year, opt_weekday_and_position, rest) =
                T!(parse_dat_without_year(&rest, options))?;
            let datetime = datetime_no_year.with_year(
                u16::from_str(year.s).map_err(
                    |e| parse_error! {
                        message: e.to_string(),
                        context: year.into()
                    })?.into())?;
            if let Some((weekday, _weekday_position)) = opt_weekday_and_position {
                let date_weekday = datetime.weekday();
                if date_weekday != weekday {
                    Err(parse_error! {
                        message: format!("invalid weekday: given {weekday}, \
                                          but date is for {date_weekday}"),
                        context: s.into()
                    })?
                }
            }
            Ok((NaiveDateTimeWithOrWithoutYear::NaiveDateTime(datetime), rest))
        }
        Err(_e1) => {
            let (datetime_no_year, _opt_weekday_and_position, rest) =
                T!(parse_dat_without_year(s, options))?;
            // TODO: Pass on the weekday
            Ok((NaiveDateTimeWithOrWithoutYear::NaiveDateTimeWithoutYear(datetime_no_year), rest))
        }
    }
}

// For parsing user input, to allow "2024-09-29 20:52:49 Sun" and
// similar formats (see parse_date_time_argument tests).
fn flexible_parse_dat_options(
    default_time: Option<NaiveTime>
) -> ParseDatOptions {
    ParseDatOptions {
        default_time,
        weekday_is_optional: true,
        date_separator: Separator {
            required: true,
            alternatives: &["-", "/"],
        },
        time_separator: Separator {
            required: false, // XX only if then requiring exactly 6 digits !
            alternatives: &[":"],
        },
        separator_between_parts: Separator {
            required: true,
            alternatives: &["_", " "],
        },
    }
}

// For command line arguments and the date in `OPEN{2024-11-10}` or `OPEN{11-10}`.
fn parse_date_time_argument<'s, B: Backing + Debug>(
    s: &ParseableStr<'s, B>,
    time_is_optional: bool,
) -> Result<NaiveDateTimeWithOrWithoutYear<StringParseContext<&'s B>>,
            ParseError<StringParseContext<&'s B>>>
where &'s B: Backing
{
    let (ndt, rest) = parse_dat(
        s, &flexible_parse_dat_options(
            if time_is_optional {
                Some(NaiveTime::from_hms_opt(12, 0, 0).unwrap())
            } else {
                None
            }))?;
    let rest = rest.trim_start();
    if rest.is_empty() {
        Ok(ndt)
    } else {
        let ndt_string = s.up_to(&rest).trim_end();
        Err(parse_error! {
            message: format!("garbage after datetime string {:?}", ndt_string.s),
            context: rest.into()
        })
    }
}


const fn parse_path_parse_dat_options(weekday_is_optional: bool) -> ParseDatOptions {
    ParseDatOptions {
        default_time: None,
        weekday_is_optional,
        date_separator: Separator {
            required: true,
            alternatives: &["-"],
        },
        time_separator: Separator {
            required: false,
            alternatives: &[],
        },
        separator_between_parts: Separator {
            required: true,
            alternatives: &["_"],
        },
    }
}

const PARSE_DAT_OPTIONS_FOR_PATH: ParseDatOptions = parse_path_parse_dat_options(true);
const PARSE_DAT_OPTIONS_FOR_INSIDE: ParseDatOptions = parse_path_parse_dat_options(false);

fn is_word_character(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn is_word_boundary(c: Option<char>) -> bool {
    match c  {
        Some(c) => !is_word_character(c),
        None => true
    }
}

// Find "OPEN"/"TODO", "DONE" and other workflow markers, together
// with the found strings, sorted by their positions. Note: this does
// *not* consider `{..}` blocks, see `find_all_parsed_markers` for
// that. (Performance: there could be a better algorithm.)
fn find_all_markers<'s, B: Backing>(
    s: &ParseableStr<'s, B>
) -> Vec<(WorkflowStatus, ParseableStr<'s, B>, ParseableStr<'s, B>)>
{
    let mut found = Vec::new();
    for status in WorkflowStatus::INSTANCES {
        for key in status.uppercase_strs() {
            let mut p = s.clone();
            while let Some((string, rest)) = p.find_str_rest(key) {
                p = rest.clone();
                let before = s.s[0..string.position].chars().rev().next();
                let after = rest.first();
                if is_word_boundary(before) && is_word_boundary(after) {
                    found.push((*status, string, rest));
                }
                // Else don't need to backtrack since all our marker
                // names are \w+, thus no overlap possible.
            }
        }
    }
    found.sort_by_key(|(_status, string, _rest)| string.position);
    found
}

#[cfg(test)]
#[test]
fn t_find_all_markers() {
    let backing = String::new();
    let t = |s: &'static str| {
        find_all_markers(&ParseableStr { backing: &backing, position: 0, s })
            .into_iter().map(|(status, string, _rest)| {
                (status, string.position)
            }).collect::<Vec<_>>()
    };
    use WorkflowStatus::*;
    assert_eq!(t("foo bar"), []);
    assert_eq!(t("foo OPEN"), [(Open, 4)]);
    assert_eq!(t("foo OPEN TODO"), [(Open, 4), (Open, 9)]);
    assert_eq!(t("foo TODO OPEN"), [(Open, 4), (Open, 9)]);
    assert_eq!(t("TODO  DONE OPEN"), [(Open, 0), (Done, 6), (Open, 11)]);
    assert_eq!(t("foo TODO_ DONE POPEN DONE4 /DONE+"), [(Done, 10), (Done, 28)]);
}

// Parse a marker with its optional `{..}` block, return parsed value
// and rest (after the block if given).
fn parse_marker<'s, B: Backing + Debug>(
    (_status, string, rest): (WorkflowStatus, ParseableStr<'s, B>, ParseableStr<'s, B>)
) -> Result<(Option<TaskInfoDeclarations<StringParseContext<&'s B>>>,
             ParseableStr<'s, B>),
            ParseError<StringParseContext<&'s B>>>
where &'s B: Backing
{
    if let Some(rest) = rest.clone().drop_str("{") {
        if let Some((inside, rest)) = rest.clone().split_at_str("}") {
            Ok((Some(T!(parse_inside(&inside))?), rest))
        } else {
            Err(parse_error! {
                message: format!("missing closing '}}' after '{}{{'", string.s),
                context: rest.into()
            })
        }
    } else {
        Ok((None, rest))
    }
}

// Find and parse all markers (with their parsed block, if any),
// skipping over marker strings that are within blocks.
fn find_all_parsed_markers<'s, B: Backing + Debug>(
    markers: Vec<(WorkflowStatus, ParseableStr<'s, B>, ParseableStr<'s, B>)>
) -> Result<Vec<(Option<TaskInfoDeclarations<StringParseContext<&'s B>>>,
                 WorkflowStatus,
                 ParseableStr<'s, B>,
                 ParseableStr<'s, B>)>,
            ParseError<StringParseContext<&'s B>>>
where &'s B: Backing
{
    let mut parsed = Vec::new();
    let mut ms = markers.into_iter();
    let mut cur_marker = ms.next();
    loop {
        if let Some(marker) = cur_marker {
            let (taskinfodecl, rest) = T!(parse_marker(marker.clone()))?;
            parsed.push((taskinfodecl, marker.0, marker.1, rest.clone()));
            loop {
                cur_marker = ms.next();
                if let Some((_, string, _)) = cur_marker.as_ref() {
                    if rest.position > string.position {
                        // skip that marker
                    } else {
                        break
                    }
                } else {
                    break
                }
            }
        } else {
            break;
        }
    }
    Ok(parsed)
}

enum MarkerStatus<C: ParseContext> {
    Open((TaskInfoDeclarations<C>, WorkflowStatus)),
    /// ex-Open? No, just whatever non-Open status we have seen.
    NotOpen(WorkflowStatus),
    None
}


// Find the last parsed `WorkflowStatus::Open` marker, if any, and/or
// the last `WorkFlowStatus` (last such marker after the `Open`, if
// any, otherwise `Open`, or whatever status was found if there's no
// `Open`).
fn find_marker_status<'s, B: Backing + Debug>(
    markers: Vec<(Option<TaskInfoDeclarations<StringParseContext<&'s B>>>,
                  WorkflowStatus,
                  ParseableStr<'s, B>,
                  ParseableStr<'s, B>)>
) -> Result<MarkerStatus<StringParseContext<&'s B>>,
            ParseError<StringParseContext<&'s B>>>
where &'s B: Backing
{
    let mut open: Option<(TaskInfoDeclarations<StringParseContext<&'s B>>,
                          ParseableStr<'s, B>)> = None;
    let mut last_status: Option<WorkflowStatus> = None;
    let mut previous_was_open = false;
    for (decl, status, string, _rest) in markers {
        match status {
            WorkflowStatus::None => (), // ? XX what is None for again?
            WorkflowStatus::Open => if previous_was_open {
                Err(parse_error! {
                    message: format!(
                        "adjacent opening markers found without a closing marker inbetween: \
                         {:?} then {:?}",
                        // ^ XX again want multi-position errors? ! todo.
                        open.as_ref().expect("set at same time as previous_was_open").1,
                        string),
                    context: string.into()
                })?
            } else {
                previous_was_open = true;
                // Overwrite previous decl even if there is none now,
                // yes: old times do not make sense any more, right?
                open = Some((decl.unwrap_or_else(Default::default), string));
                last_status = Some(WorkflowStatus::Open);
            },
            // (Also check that only one closing marker is given?
            // Actually it may be OK to have multiple!)
            WorkflowStatus::Done |
            WorkflowStatus::Obsolete |
            WorkflowStatus::Wontfix |
            WorkflowStatus::Dupe |
            WorkflowStatus::Future |
            WorkflowStatus::Notforme |
            WorkflowStatus::Applied |
            WorkflowStatus::Active |
            WorkflowStatus::Rejected => {
                previous_was_open = false;
                last_status = Some(status);
            }
        }
    }
    if let Some(info) = open {
        Ok(MarkerStatus::Open((info.0, last_status.expect("set when `open` was set"))))
    } else {
        Ok(if let Some(status) = last_status {
            MarkerStatus::NotOpen(status)
        } else {
            MarkerStatus::None
        })
    }
}


fn parse_path(
    id: usize, verbose: bool, region: &Region<PathBuf>, item: &FilePathType,
    show_backtrace: bool
) -> Result<TaskInfo<StringParseContext<String>>>
{
    let path = item.to_path_buf(region);
    if verbose { eprintln!("parse_path({path:?})") };
    let backing: String = item.file_name.to_str().ok_or_else(
        || anyhow!("the file name in path {path:?} does not have valid encoding"))?
        .into();
    let file_name = ParseableStr::from(&backing);

    let (dependency_key_ndt, declarations, workflow_status_from_filename
    ) = (|| -> Result<(Option<NaiveDateTime>,
                       TaskInfoDeclarations<_>,
                       WorkflowStatus), ParseError<_>> {
        let declarations: TaskInfoDeclarations<_>;
        let workflow_status_from_filename: WorkflowStatus;
        let opt_datetime;

        let possibly_datetime = || {
            if let Ok((datetime, _rest)) = parse_dat(
                &file_name, &PARSE_DAT_OPTIONS_FOR_PATH)
            {
                Some(datetime)
            } else {
                None
            }
        };

        let marker_status = find_marker_status(
            find_all_parsed_markers(
                find_all_markers(
                    &file_name))?)?;

        match marker_status {
            MarkerStatus::Open(decl_and_status) => {
                (declarations, workflow_status_from_filename) = decl_and_status;

                // Must have a date in file name -- currently. Should this change?
                // old comment, todo understand:
                // XX when to accept and how when a date is just not there?
                // Maybe if starts with 2020..2050 number then -, go all the
                // way.
                let (datetime, _rest) = T!(parse_dat(&file_name, &PARSE_DAT_OPTIONS_FOR_PATH))?;
                opt_datetime = Some(datetime);
            }
            MarkerStatus::NotOpen(status) => {
                declarations = Default::default();
                workflow_status_from_filename = status;
                opt_datetime = possibly_datetime();
                
            }
            MarkerStatus::None => {
                declarations = Default::default();
                workflow_status_from_filename = WorkflowStatus::None;
                opt_datetime = possibly_datetime();
            }
        }

        let opt_ndt = opt_datetime.map(|datetime| datetime.into_naive_date_time()).transpose()?;
        Ok((opt_ndt, declarations, workflow_status_from_filename))
    })().map_err(|e| {
        anyhow!("error parsing the file name of path {path:?}: {}",
                e.to_string_showing_location(show_backtrace))
    })?;

    let meta = path.metadata()
        .with_context(|| anyhow!("getting metadata of file {path:?}"))?;
    let mtime = meta.modified()
        .with_context(|| anyhow!("getting modified time of file {path:?}"))?;

    // Get workflow status from the *folder(s)* (above we got it from
    // the file name, only):
    let workflow_status = WorkflowStatus::try_from(&*path)
        .unwrap_or(workflow_status_from_filename);

    Ok(TaskInfo {
        id,
        dependency_key: dependency_key_ndt.map(DependencyKey::from),
        workflow_status,
        path,
        mtime,
        calculated_priority: None.into(),
        declarations: declarations.into_owning_backing(),
    })
}


fn main() -> Result<()> {
    let opts: Opts = Opts::from_args();
    let show_backtrace = true; // XX add option?
    let now: NaiveDateTime = if let Some(time) = &opts.time {
        let now_ndtwowy = parse_date_time_argument(&ParseableStr::new(time), true).map_err(
            |e| anyhow!("can't parse --time option value: {}",
                        e.to_string_showing_location(show_backtrace)))?;
        match now_ndtwowy {
            NaiveDateTimeWithOrWithoutYear::NaiveDateTime(ndt) => ndt,
            NaiveDateTimeWithOrWithoutYear::NaiveDateTimeWithoutYear(val) =>
                bail!("Date must contain year: {val:?}")
        }
    } else {
        let now_utc = Utc::now();
        now_utc.naive_local()
    };

    let default_directories = [".".into()];
    let directories = if opts.directories.is_empty() {
        &default_directories
    } else {
        &*opts.directories
    };

    let excludes = {
        let mut excludes = default_excludes(opts.all);
        // Need to see e.g. `test/priorities/reprocessing/.2024-10-03_173610_Thu- OPEN{prio: 2}`:
        excludes.exclude_dot_files = false;
        excludes
    };

    let itemopts = ItemOptions {
        dirs: false,
        files: true,
        other: false,
    };

    let mut taskinfos: Vec<Rc<TaskInfo<_>>> = Default::default();
    let mut taskinfo_by_key: BTreeMap<DependencyKey, Rc<TaskInfo<_>>> = Default::default();

    let region = Region::new();
    let mut errors = 0;

    for directory in directories {
        for (id, item) in recursive_file_path_types_iter(
            &region, region.store(directory.clone()), itemopts, &excludes, true,
        ).enumerate()
        {
            let item = item?; // XX context?
            if item.is_file() || item.is_dir() {
                match parse_path(id, opts.verbose, &region, &item, show_backtrace) {
                    Ok(taskinfo) => {
                        let taskinfo = Rc::new(taskinfo);
                        if let Some(key) = &taskinfo.dependency_key {
                            if let Some(old_taskinfo) = taskinfo_by_key.get(key) {
                                warning!("duplicate task for key {key:?}: {:?} {:?}",
                                         taskinfo.path,
                                         old_taskinfo.path);
                                errors += 1;
                            }
                            taskinfo_by_key.insert(key.clone(), taskinfo.clone());
                        }
                        taskinfos.push(taskinfo);
                    }
                    Err(e) => {
                        errors += 1;
                        warning!("{e}");
                    }
                }
            }
            // XX: if it's a symlink, check if it has different OPEN info?
        }
    }

    // Calculate "stand-alone" priorities
    for ti in &taskinfos {
        // If there is an error (due date without year completed based
        // on `now` but invalid month/day or so), can't continue since
        // would have to remove the entry from taskinfos and
        // taskinfo_by_key again, or deal with None value as priority
        // later on, none of which are good solutions. Leave it at
        // that for now.
        ti.set_initial_priority_level_for(now).map_err(|e| {
            anyhow!("Date Error in {:?}: {}",
                    e.context.backing,
                    e.to_string_showing_location(show_backtrace))
        })?;
    }

    // Verify dependency links and exert priority inheritance
    for ti in &taskinfos {
        // Do not make priorities on dependencies relevant for (a)
        // files which are not obviously tasks, (b) tasks which
        // are not open.
        if !ti.workflow_status.is_active() {
            continue;
        }

        // Need to recurse to update with the new base number; todo:
        // this is inefficient (O(n^2)).
        let mut recur = |ti: &TaskInfo<_>, list_of_seen| {
            let list_of_seen = cons(ti.id, list_of_seen);
            for dependency in ti.declarations.dependencies.iter() {
                if let Some(dependency_ti) = taskinfo_by_key.get(dependency) {
                    if !list_of_seen.contains(&dependency_ti.id) {
                        dependency_ti.set_as_dependency_for_priority(ti.calculated_priority());
                    }
                } else {
                    // XX hmm, done items should not give an error but be
                    // treated as done!
                    // XX also, "2024-09-27_201849" must be fine, use parsed version!
                    warning!("unknown dependency {dependency:?} in: {:?}",
                             ti.path);
                    errors += 1;
                }
                // XXX where is the recursive call to recur ??
            }
        };
        recur(ti, &List::Null)
    }

    if opts.verbose {
        dbg!(&taskinfos);
    }

    // Sort them
    taskinfos.sort_by(|a, b| {
        a.calculated_priority().partial_cmp(&b.calculated_priority()).expect("no NaN")
            .then_with(
                || a.path.partial_cmp(&b.path).expect("path comparisons never fail"))
    });

    // Print them
    (|| -> std::io::Result<()> {
        let mut out = stdout().lock();
        for ti in &taskinfos {
            if !ti.workflow_status.is_active() {
                continue;
            }
            if !opts.no_priority {
                let s = format!("{:.2}", ti.calculated_priority());
                out.write_all(s.as_bytes())?;
                out.write_all(b"\t")?;
            }
            out.write_all(ti.path.as_os_str().as_bytes())?;
            out.write_all(b"\n")?;
        }
        Ok(())
    })().context("printing to stdout")?;

    if errors > 0 {
        eprintln!("{errors} error(s) shown as warnings");
        exit(1);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;

    use super::*;
    use chrono::NaiveDate;

    // Only implement PartialEq for the tests--don't want it in normal
    // code to prevent accidental comparisons with the year
    // potentially missing.

    // It's actually OK to compare NaiveDateTimeWithoutYear given
    // tests where we compare to other without-year values and only
    // want to know if the parsing step was correct.
    fn naivedatetimewithoutyear_partial_eq<C: ParseContext>(
        a: &NaiveDateTimeWithoutYear<C>,
        b: &NaiveDateTimeWithoutYear<C>
    ) -> bool {
        macro_rules! cmp {
            { $field_name:tt } => {
                a.$field_name == b.$field_name
            }
        }
        cmp!(context) &&
            cmp!(month) &&
            cmp!(day) &&
            cmp!(hour) &&
            cmp!(minute) &&
            cmp!(second)
    }

    impl<C: ParseContext> PartialEq for NaiveDateTimeWithOrWithoutYear<C> {
        fn eq(&self, other: &Self) -> bool {
            match self {
                NaiveDateTimeWithOrWithoutYear::NaiveDateTime(ndt1) => match other {
                    NaiveDateTimeWithOrWithoutYear::NaiveDateTime(ndt2) => ndt1 == ndt2,
                    NaiveDateTimeWithOrWithoutYear::NaiveDateTimeWithoutYear(_) => panic!("can't compare")
                },
                NaiveDateTimeWithOrWithoutYear::NaiveDateTimeWithoutYear(d) => match other {
                    NaiveDateTimeWithOrWithoutYear::NaiveDateTime(_) => panic!("can't compare"),
                    NaiveDateTimeWithOrWithoutYear::NaiveDateTimeWithoutYear(d2) =>
                        naivedatetimewithoutyear_partial_eq(d, d2)
                }
            }
        }
    }

    impl<C: ParseContext> PartialEq for Priority<C> {
        fn eq(&self, other: &Self) -> bool {
            match self {
                Priority::Date(d1) => match other {
                    Priority::Date(d2) => d1 == d2,
                    _ => false
                }
                Priority::DaysFromToday(d1) => match other {
                    Priority::DaysFromToday(d2) => d1 == d2,
                    _ => false
                }
                Priority::Today => match other {
                    Priority::Today => true,
                    _ => false
                }
                Priority::Tonight => match other {
                    Priority::Tonight => true,
                    _ => false
                }
                Priority::Ongoing => match other {
                    Priority::Ongoing => true,
                    _ => false
                }
                Priority::Unknown => match other {
                    Priority::Unknown => true,
                    _ => false
                }
                Priority::Level(l1) => match other {
                    Priority::Level(l2) => l1 == l2,
                    _ => false
                }
            }
        }
    }

    #[test]
    fn t_parse_priority() {
        // Hack for `backing`, to avoid having to use ugly macros like
        // in parsers.rs...
        let backing = RefCell::new(String::new());

        fn t_<'t>(backing: &'t String, s: &'t str)
            -> Priority<StringParseContext<&'t String>>
        {
            Priority::from_parseable_str(&ParseableStr{ backing,
                                                        position: 0,
                                                        s }).unwrap()
        }
        let t = |s| {
            let mut backing = backing.borrow_mut();
            backing.clear();
            backing.push_str(s);
            t_(&*backing, s).into_owning_backing()
        };
        let te = |s: &str| {
            let mut backing = backing.borrow_mut();
            backing.clear();
            backing.push_str(s);
            Priority::from_parseable_str(
                &ParseableStr {
                    backing: &*backing,
                    position: 0,
                    s
                }).err().unwrap()
                .to_string_showing_location(false)
        };
        let ymd_hms = |y, m, d, h, min, s| {
            Priority::Date(NaiveDateTimeWithOrWithoutYear::NaiveDateTime(
                NaiveDate::from_ymd_opt(y, m, d).unwrap().and_hms_opt(h, min, s).unwrap()))
        };
        assert_eq!(t("2024-11-01 11:37"), ymd_hms(2024, 11, 01, 11, 37, 0));
        assert_eq!(t("2024-11-01 11:37:13"), ymd_hms(2024, 11, 01, 11, 37, 13));
        assert_eq!(te("2024-11-01 12:00}"), "garbage after date/time at \"}\"");
        assert_eq!(te("2024-11-01 12:00:01 a"), "garbage after date/time at \"a\"");
        assert_eq!(t("2024-11-01 12:00:01  "), ymd_hms(2024, 11, 01, 12, 0, 1));
        assert_eq!(t("2024-11-01 120001  "), ymd_hms(2024, 11, 01, 12, 0, 1));
        assert_eq!(t("2024-11-01 020001  "), ymd_hms(2024, 11, 01, 2, 0, 1));
        assert_eq!(t("2024-11-01 2:00:01  "), ymd_hms(2024, 11, 01, 2, 0, 1));
        assert_eq!(te("2024-11-01 2:0:01  "), "garbage after date/time at \"2:0:01  \"");
        assert_eq!(te("2024-11-01 0200:01  "), "garbage after date/time at \":01  \"");
        assert_eq!(t("2024-11-01 0200  "), ymd_hms(2024, 11, 01, 2, 0, 0));
        assert_eq!(te("2024-11-01 200  "), "garbage after date/time at \"200  \"");

        let p_md_hms = |position, month, day, hour, minute, second| {
            Priority::Date(NaiveDateTimeWithOrWithoutYear::NaiveDateTimeWithoutYear(
                NaiveDateTimeWithoutYear {
                    context: StringParseContext { position, backing: &*backing.borrow() },
                    month,
                    day,
                    hour,
                    minute,
                    second,
                })).into_owning_backing()
        };
        assert_eq!(t("11-01 11:37"), p_md_hms(0, 11, 01, 11, 37, 0));
        assert_eq!(t("11-1 11:37:13"), p_md_hms(0, 11, 01, 11, 37, 13));
        assert_eq!(te("11-01 12:00}"), "garbage after date/time at \"}\"");
        assert_eq!(te("11-1 12:00:01 a"), "garbage after date/time at \"a\"");
        assert_eq!(t("11-01 12:00:01  "), p_md_hms(0, 11, 01, 12, 0, 1));
        assert_eq!(t("  11-01 120001  "), p_md_hms(2, 11, 01, 12, 0, 1));
        assert_eq!(t("11-01 020001  "), p_md_hms(0, 11, 01, 2, 0, 1));
        assert_eq!(t("11-01 2:00:01  "), p_md_hms(0, 11, 01, 2, 0, 1));
        assert_eq!(te("11-01 2:0:01  "), "garbage after date/time at \"2:0:01  \"");
        assert_eq!(te("11-01 0200:01  "), "garbage after date/time at \":01  \"");
        assert_eq!(t("11-01 0200  "), p_md_hms(0, 11, 01, 2, 0, 0));
        assert_eq!(te("11-01 200  "), "garbage after date/time at \"200  \"");
        
    }

    #[test]
    fn t_parse_date_time_argument() {
        // Hack for `backing`, to avoid having to use ugly macros like
        // in parsers.rs...
        let backing = RefCell::new(String::new());

        let ok: Result<NaiveDateTimeWithOrWithoutYear<StringParseContext<String>>, _>  =
            Ok(
                NaiveDateTimeWithOrWithoutYear::NaiveDateTime(
                    NaiveDateTime::new(NaiveDate::from_ymd_opt(2024, 9, 29).unwrap(),
                                       NaiveTime::from_hms_opt(20, 52, 49).unwrap())));
        fn _t<'t>(backing: &'t String, s: &'t str) ->
            Result<NaiveDateTimeWithOrWithoutYear<StringParseContext<&'t String>>,
                   ParseError<StringParseContext<&'t String>>>
        {
            parse_date_time_argument(&ParseableStr { backing,
                                                     position: 0,
                                                     s }, false)
        }
        let t = |s| {
            let backing = backing.borrow();
            _t(&*backing, s)
                .map(|v| v.into_owning_backing())
                .map_err(|v| v.into_owning_backing())
        };

        // hack: fill in backing so that `t` uses it.
        let te = |s| {
            {
                let mut backing = backing.borrow_mut();
                backing.clear();
                backing.push_str(s);
            }
            t(s).unwrap_err().to_string_showing_location(false)
        };
        assert_eq!(t("2024-09-29_205249_Sun"), ok);
        assert_eq!(t("2024-09-29 20:52:49 Sun"), ok);
        assert_eq!(te("2024-09-29 20:52:49 Mon"),
                   "invalid weekday: given Mon, but date is for Sun at \"2024-09-29 20:52:49 Mon\"");
        assert_eq!(t("2024/09/29 20:52:49 Sun"), ok);
        assert_eq!(t("2024/09/29 20:52:49"), ok);
        assert_eq!(te("2024/09/29 20:52:49 foo"),
                   "garbage after datetime string \"2024/09/29 20:52:49\" at \"foo\"");
        assert_eq!(te("2024-09-29_205249_Mon"),
                   "invalid weekday: given Mon, but date is for Sun at \"2024-09-29_205249_Mon\"");
        assert_eq!(t("2024-09-29_205249 Sun"), ok);
        assert_eq!(t("2024-09-29_205249 Sun   "), ok);
        assert_eq!(t("2024-09-29_205249 Sunday   "), ok);
        assert_eq!(t("2024-09-29_20:52:49 Sun   "), ok);
        assert_eq!(te("2024-09-29_2052:49 Sun   "),
                   "garbage after datetime string \"2024-09-29_2052\" at \":49 Sun   \"");
        assert_eq!(t("2024-09-29_20:5249 Sun   "), ok);
        // allow this?:
        assert_eq!(te("2024-09-29_205249  Sun   "),
                   "garbage after datetime string \"2024-09-29_205249\" at \"Sun   \"");
        assert_eq!(te("2024-09-29_205249 Sund"),
                   "garbage after datetime string \"2024-09-29_205249\" at \"Sund\"");
        assert_eq!(te("2024-09-29_205249 Sun Sun"),
                   "garbage after datetime string \"2024-09-29_205249 Sun\" at \"Sun\"");
        assert_eq!(te("2024/09/29"),
                   "time is required but missing after \"09/29\" at end of input");
        assert_eq!(te("2024/09/29 foo"),
                   "time is required but missing after \"09/29\" at \" foo\"");

        let ok2: Result<NaiveDateTimeWithOrWithoutYear<StringParseContext<String>>, _> =
            Ok(
                NaiveDateTimeWithOrWithoutYear::NaiveDateTime(
                    NaiveDateTime::new(NaiveDate::from_ymd_opt(2024, 9, 29).unwrap(),
                                       NaiveTime::from_hms_opt(12, 0, 0).unwrap())));
        // This t is with value `true`
        fn t_<'t>(backing: &'t String, s: &'t str)
                  -> Result<NaiveDateTimeWithOrWithoutYear<StringParseContext<&'t String>>,
                            ParseError<StringParseContext<&'t String>>>
        {
            parse_date_time_argument(&ParseableStr { backing,
                                                     position: 0, s }, true)
        }
        let t = |s| {
            t_(&*backing.borrow(), s)
                .map(|v| v.into_owning_backing())
                .map_err(|v| v.into_owning_backing())
        };
        let te = |s| t(s).unwrap_err().to_string_showing_location(false);
        assert_eq!(t("2024/09/29"), ok2);
        assert_eq!(t("2024/09/29 20:52:49 Sun"), ok);
        assert_eq!(te("2024/09/29 foo"),
                   "garbage after datetime string \"2024/09/29\" at \"foo\"");
    }
}

