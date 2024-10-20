use std::{path::{PathBuf, Path},
          convert::TryFrom,
          time::SystemTime,
          str::FromStr, collections::{BTreeSet, BTreeMap},
          rc::Rc, cell::Cell, ops::Deref, process::exit, io::{stdout, Write},
          os::unix::prelude::OsStrExt, cmp::Ordering};

use anyhow::{Result, anyhow, Context};
use chrono::{NaiveDate, NaiveDateTime, NaiveTime, Weekday, Datelike, Duration, Utc, DateTime, Timelike};
use clap::Parser;
use kstring::KString;

use chj_rustbin::{parse::{parsers::{ParseableStr, IntoParseable, FromParseableStr,
                                    Separator},
                          parse_error::ParseError},
                  impl_item_options_from,
                  io::file_path_type::{ItemOptions, FilePathType, recursive_file_path_types_iter},
                  io::excludes::default_excludes,
                  region::Region,
                  conslist::{cons, List},
                  fp::compose};
use chj_rustbin::{parse_error, T};

fn _warning(s: String) {
    eprintln!("WARNING: {s}");
}
macro_rules! warning {
    { $($fmt:tt)* } => {
        _warning(format!($($fmt)*))
    }
}


#[derive(clap::Parser, Debug)]
/// Parse a folder with todo files with "OPEN.." markers.
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
    directories: Vec<PathBuf>
}

impl_item_options_from!{Opts}


#[derive(Debug, Clone, Copy)]
pub enum TaskSize {
    Minutes,
    Hours,
    Days,
    Weeks,
    Months,
    Unknown
}

impl TaskSize {
    pub fn in_weeks(self) -> f32 {
        match self {
            TaskSize::Minutes => 1./48., // 1./12./4., 30 minutes = 1/12 day programming capacity
            TaskSize::Hours => 1./8., // half a day, 4 work days a week
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

impl FromParseableStr for TaskSize {
    type Err = ParseError;

    fn from_parseable_str(s: ParseableStr) -> Result<Self, Self::Err> {
        let s = s.trim();
        match s.s {
            "h"|"hour"|"hours" => Ok(TaskSize::Hours),
            "d"|"day"|"days" => Ok(TaskSize::Days),
            "w"|"week"|"weeks" => Ok(TaskSize::Weeks),
            "mo"|"mon"|"month"|"months" => Ok(TaskSize::Months),
            "mi"|"min"|"minute"|"minutes" => Ok(TaskSize::Minutes),
            _ => Err(parse_error! {
                message: format!("unknown task size {:?} (must be \
                                  h|d|w|mo|month|months|mi|min|minuteminutes",
                                 s.s),
                position: s.position
            })
        }
    }
}

pub type ManualPriorityLevel = u8;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Priority {
    /// Due on that date
    Date(NaiveDateTime),
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

// rough, hack. UTC. good enough when days are the main thing I want.
fn naive_from_systemtime(st: SystemTime) -> NaiveDateTime {
    let dt : DateTime<Utc> = DateTime::from(st);
    //dt.naive_local() // XX hmmm, local based on what zone???
    dt.naive_utc()
}

/// A number that can be compared, to directly order jobs to choose
/// which one should be worked on.
pub fn priority_level(
    priority: &Priority,
    tasksize: &TaskSize,
    _key: &Option<DependencyKey>,
    mtime: SystemTime,
    now: NaiveDateTime
) -> f32 {
    let priority_for_time_left = |time_to_target: Duration| {

            let time_secs = time_to_target.num_seconds().max(0);
            let programming_secs_per_week = 7.*60.*60.*24.*4.;
            let time_weeks: f32 = (time_secs as f32) / programming_secs_per_week * 5.;// XXXXHACK

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
        prio + age_sec * (1. / (60.*60.*24.*7.))
    };
    match priority {
        Priority::Date(ndt) => {
            let time_to_target: Duration = ndt.signed_duration_since(now);
            priority_for_time_left(time_to_target)
        }
        Priority::DaysFromToday(num_days) => {
            // (XX Base on noon on target day, too ?)
            priority_for_time_left(Duration::days((*num_days).into()))
        }
        Priority::Today => {
            let target_time = now
                .with_hour(12).unwrap()
                .with_minute(0).unwrap()
                .with_second(0).unwrap();
            priority_for_time_left(target_time.signed_duration_since(now))
        }
        Priority::Tonight => {
            let target_time = now
                .with_hour(18).unwrap()
                .with_minute(0).unwrap()
                .with_second(0).unwrap();
            priority_for_time_left(target_time.signed_duration_since(now))
        }
        // XX: what about mixing priorities, target date given *and* a priority level?
        Priority::Level(l) => modify_priority_with_mtime(f32::from(*l)),
        Priority::Ongoing => modify_priority_with_mtime(5.), // ?
        Priority::Unknown => modify_priority_with_mtime(5.), // ?
    }
}


impl Default for Priority {
    fn default() -> Self {
        Self::Unknown
    }
}

// If you're looking for an `impl From<NaiveDateTime> for Priority`,
// just use `Priority::Date(ndt)`.

impl FromParseableStr for Priority {
    type Err = ParseError;

    fn from_parseable_str(s: ParseableStr) -> Result<Self, Self::Err> {
        let s = s.trim();
        let ss = s.s;
        if ss == "ongoing" {
            // Note: now have `inside_parse_class`, too. XX inconsistent?
            return Ok(Priority::Ongoing)
        }
        if let Ok(level) = ManualPriorityLevel::from_str(ss) {
            return Ok(Priority::Level(level))
        }
        if let Ok((ndt, _str, rest)) = parse_dat(s.trim(), &flexible_parse_dat_options(
            Some(NaiveTime::from_hms_opt(12, 0, 0).unwrap())))
        {
            let rest = rest.drop_whitespace();
            if rest.is_empty() {
                return Ok(Priority::Date(ndt))
            } else {
                return Err(parse_error!{
                    message: format!("garbage after date/time"),
                    position: rest.position
                })
            }
        }
        Err(parse_error! {
            message: format!("invalid priority string {s:?} \
                              (valid are 'ongoing', 1..99, 2019-12-11 style date (and time))"),
            position: s.position
        })
    }
}

#[cfg(test)]
#[test]
fn t_parse_priority() {
    let t = |s: &str| Priority::from_parseable_str(ParseableStr::new(s)).unwrap();
    let te = |s: &str| Priority::from_parseable_str(ParseableStr::new(s)).err().unwrap()
        .to_string_in_context(s);
    let ymd_hms = |y, m, d, h, min, s| {
        Priority::Date(NaiveDate::from_ymd_opt(y, m, d).unwrap()
                       .and_hms_opt(h, min, s).unwrap())
    };
    assert_eq!(t("2024-11-01 11:37"), ymd_hms(2024,11,01, 11,37,0));
    assert_eq!(t("2024-11-01 11:37:13"), ymd_hms(2024,11,01, 11,37,13));
    assert_eq!(te("2024-11-01 12:00}"), "garbage after date/time at \"}\"");
    assert_eq!(te("2024-11-01 12:00:01 a"), "garbage after date/time at \"a\"");
    assert_eq!(t("2024-11-01 12:00:01  "), ymd_hms(2024,11,01, 12,0,1));
}

#[derive(Default, Debug, Clone)]
struct Dependencies {
    // XX Arc for clone efficiency?
    keys: BTreeSet<DependencyKey>
}

impl Dependencies {
    pub fn from_parseable_str(s: ParseableStr) -> Result<Self, ParseError> {
        let mut keys = BTreeSet::new();
        for s in s.split_str(",", true)
            .map(|s| s.trim())
            .filter(|s| !s.is_empty()) // optim: only the *last* segment can be empty
        {
            keys.insert(
                (
                    || -> Result<Option<DependencyKey>, ParseError> {
                        let (d, dstr, rest) =
                        // It's OK to fail parsing here as we've got a
                        // fallback.
                            match parse_dat(s, &PARSE_DAT_OPTIONS_FOR_INSIDE) {
                                Ok(v) => v,
                                Err(_) => return Ok(None)
                            };
                        let dk = DependencyKey::from(d);
                        // Given we succeeded at getting a date, from
                        // now on we report errors.
                        if keys.contains(&dk) {
                            Err(parse_error! {
                                message: format!("duplicate dependency entry {d}"),
                                position: dstr.position
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


#[derive(Debug, Clone, Default)]
struct TaskInfoDeclarations {
    tasksize: TaskSize,
    priority: Priority,
    dependencies: Dependencies,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum WorkflowStatus {
    None,

    Open,
    Done,
    Obsolete,

    Dupe,
    Future,

    Notforme,
    Applied,
    Active,
    Rejected,
}

impl WorkflowStatus {
    pub fn is_active(self) -> bool {
        match self {
            WorkflowStatus::None => false,
            WorkflowStatus::Open => true,
            WorkflowStatus::Done => false,
            WorkflowStatus::Obsolete => false,
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

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            // This should never be used as a folder name, since need
            // the `OPEN` string in the file name to introduce the `{
            // .. }` section anyway.
            // "open" => Ok(WorkflowStatus::Open),
            
            "done" => Ok(WorkflowStatus::Done),
            "obsolete" => Ok(WorkflowStatus::Obsolete),

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

    fn try_from(path: &Path) -> Result<Self, Self::Error> {
        let mut anc = path.ancestors();
        let _ = anc.next(); // the file name
        for dir in anc {
            if let Some(file_name) = dir.file_name() {
                match file_name.to_str() {
                    Some(segment) =>
                        if let Ok(status) = WorkflowStatus::from_str(segment) {
                            return Ok(status)
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
struct TaskInfo {
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
    declarations: TaskInfoDeclarations
}

impl TaskInfo {
    fn priority_level_at(&self, now: NaiveDateTime) -> f32 {
        priority_level(&self.declarations.priority,
                       &self.declarations.tasksize,
                       &self.dependency_key,
                       self.mtime,
                       now)
    }

    fn set_initial_priority_level_for(&self, now: NaiveDateTime) {
        assert!(self.calculated_priority.get().is_none());
        self.calculated_priority.set(Some(self.priority_level_at(now)));
    }

    fn calculated_priority(&self) -> f32 {
        self.calculated_priority.get().expect(
            "calculated_priority field has been filled via a call to \
             `set_initial_priority_level_for` earlier")
    }

    fn set_as_dependency_for_priority(&self, priority: f32) {
        let own_prio = self.calculated_priority.get().unwrap();
        let target_prio = priority - 0.1;
        if own_prio > target_prio  {
            self.calculated_priority.set(Some(target_prio));
        }
    }
}

impl PartialEq for TaskInfo {
    fn eq(&self, _other: &Self) -> bool {
        false // f32 are never eq?  or do we have to ?
    }
}

impl PartialOrd for TaskInfo {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.calculated_priority.get().unwrap().partial_cmp(
            &other.calculated_priority.get().unwrap())
    }
}


#[derive(Default)]
struct TaskInfoDeclarationsBuilder {
    tasksize: Option<TaskSize>,
    priority: Option<Priority>,
    dependencies: Option<Dependencies>,
}

macro_rules! def_builder_setter {
    { $method_name:ident, $field_name:ident, $field_type:ty } => {
        fn $method_name(&mut self, val: $field_type, came_from: ParseableStr)
                        -> Result<(), ParseError> {
            if let Some(old) = &self.$field_name {
                return Self::old_err(format!("{old:?}"), came_from);
            }
            self.$field_name = Some(val);
            Ok(())
        }
    }
}
    
impl TaskInfoDeclarationsBuilder {
    fn old_err(old: String, came_from: ParseableStr) -> Result<(), ParseError> {
        Err(parse_error! {
            message: format!("key {:?} occurred before with value {old}", came_from.s),
            position: came_from.position
        })
    }

    def_builder_setter!(set_tasksize, tasksize, TaskSize);
    def_builder_setter!(set_priority, priority, Priority);
    def_builder_setter!(set_dependencies, dependencies, Dependencies);
}

impl From<TaskInfoDeclarationsBuilder> for TaskInfoDeclarations {
    fn from(value: TaskInfoDeclarationsBuilder) -> Self {
        let tasksize = value.tasksize.unwrap_or_default();
        let priority = value.priority.unwrap_or_default();
        let dependencies = value.dependencies.unwrap_or_default();
        Self { priority, dependencies, tasksize }
    }
}

fn is_word_char(c: char) -> bool {
    c == '_' || c == '-' || c.is_ascii_alphanumeric()
}

fn is_ascii_digit_char(c: char) -> bool {
    c.is_ascii_digit()
}

fn expect_str_or_eos<'s>(s: ParseableStr<'s>, needle: &str)
                         -> Result<ParseableStr<'s>, ParseError> {
    s.drop_whitespace().expect_str_or_eos(needle)
        .map(ParseableStr::drop_whitespace)
        .map_err(compose(ParseError::from,
                         |e| e.message_append(" or the end of the input segment")))
}

fn expect_comma_or_eos(s: ParseableStr) -> Result<ParseableStr, ParseError> {
    T!(expect_str_or_eos(s, ","))
}

fn take_value_drop_whitespace(s: ParseableStr)
                              -> Result<(ParseableStr, ParseableStr), ParseError> {
    let mut rest = T!(s.expect1_matching(is_word_char, "[a-zA-Z_0-9-]+"))?;
    rest = rest.drop_while(is_word_char);
    let value = s.up_to(rest);
    rest = rest.drop_whitespace();
    Ok((value, rest))
}


// The string representing one value after a key.  It is either a
// single word/integer followed by a comma / whitespace / eos, or [ ]
// with the same (but the trailing comma will be omitted, XX currently
// not), followed by the same. Even in "list" case, the [ ] are not
// part of it, `[single_value]` and `single_value` are treated the
// same. "List" context expects at least one value.
fn take_one_value_from(s: ParseableStr) -> Result<(ParseableStr, ParseableStr), ParseError> {
    let mut rest = s;
    rest = rest.drop_whitespace();
    if let Some(mut rest) = rest.drop_str("[") {
        // "list"
        rest = rest.drop_whitespace();
        let start = rest;
        loop {
            // first expect a value
            (_, rest) = T!(take_value_drop_whitespace(rest))?;
            if rest.is_empty() {
                return Ok((start.up_to(rest), rest))
            }
            // then a separator or end marker
            match rest.expect_str(",") {
                Ok(s) => rest = s,
                Err(_) => match rest.expect_str("]") {
                    Ok(mut after) => {
                        after = T!(expect_comma_or_eos(after))?;
                        return Ok((start.up_to(rest), after))
                    }
                    Err(e) => Err(parse_error! {
                        // or eos, but in context? "or '}'"
                        message: "expecting ',' or ']'".into(),
                        position: e.position,
                    })?
                }
            }
            rest = rest.drop_whitespace();
        }
    } else {
        // single value
        let (value, rest) = T!(take_value_drop_whitespace(rest))?;
        let rest = T!(expect_comma_or_eos(rest))?;
        Ok((value, rest))
    }
}

#[cfg(test)]
#[test]
fn t_take_one_value_from() {
    let t = |s| take_one_value_from(ParseableStr { position: 0, s });
    let ok = |p1, s1, p2, s2| Ok((ParseableStr { position: p1, s: s1 },
                                  ParseableStr { position: p2, s: s2 }));
    let err = |position, msg: &str| Err(parse_error! { message: msg.into(), position });

    assert_eq!(t(" 1, b"), ok(1, "1", 4, "b"));
    assert_eq!(t("1 , b"), ok(0, "1", 4, "b"));
    assert_eq!(t(" 1"), ok(1, "1", 2, ""));
    assert_eq!(t(" 12"), ok(1, "12", 3, ""));
    assert_eq!(t("Abcd_X"), ok(0, "Abcd_X", 6, ""));
    assert_eq!(t("Abcd_X "), ok(0, "Abcd_X", 7, ""));
    assert_eq!(t("1  b"), err(3, "expected \",\" or the end of the input segment"));
    assert_eq!(t("  "), err(2, "expected [a-zA-Z_0-9-]+"));
    assert_eq!(t(" [a] "), ok(2, "a", 5, ""));
    assert_eq!(t(" [a,b] "), ok(2, "a,b", 7, ""));
    assert_eq!(t(" [a b] "), err(4, "expecting ',' or ']'"));
    assert_eq!(t(" [a, b] "), ok(2, "a, b", 8, ""));
    assert_eq!(t(" [ a , b ] "), ok(3, "a , b ", 11, ""));
}

// Tries to parse a key/value pair, returns the rest after the value
// (which includes the comma and further arguments, to be handled by
// the caller)
fn inside_parse_key_val<'s>(
    builder: &mut TaskInfoDeclarationsBuilder,
    ident: ParseableStr,
    rest: ParseableStr<'s>,
) -> Result<ParseableStr<'s>, ParseError> {
    if let Some(rest) = rest.drop_str(":") {
        macro_rules! parse_to {
            { $setter:ident, $type:ident } => {{
                let (value, rest) = T!(take_one_value_from(rest))?;
                builder.$setter($type::from_parseable_str(value)?, value)?;
                Ok(rest)
            }}
        }

        match ident.s {
            "s"|"size"|"tasksize" => parse_to!(set_tasksize, TaskSize),
            // "due" should only accept a date?
            "p"|"prio"|"priority"|"due" => parse_to!(set_priority, Priority),
            "d"|"dep"|"depends"|"dependencies" => parse_to!(set_dependencies, Dependencies),

            _ => Err(parse_error! {
                message: format!("unknown key {:?}", ident.s),
                position: ident.position
            })
        }
    } else {
        Err(parse_error! {
            message: "missing ':' after possible keyword".into(),
            position: rest.position,
        })
    }
}

// Attempt to parse an identifier as a 'class' name; nothing must come
// after `ident` (except comma or further arguments, to be checked by
// the caller). Returns the values to be set.
fn inside_parse_class(
    ident: ParseableStr,
) -> Option<(TaskSize, Priority)> {

    match ident.s {
        // Special keys that have no values

        // The idea with some of these was rather
        // to make classes of things; but yeah,
        // the only relevant information out of
        // this is priority (and size).

        "chore" => {
            // Pick it up at times when doing mind
            // numbing things. Small.
            Some((TaskSize::Minutes, Priority::Level(7)))
        }
        "decide" => {
            // Something small to decide on; but
            // somewhat higher priority since it
            // is still probably somewhat time
            // relevant (things in the future
            // depend on the decision).
            Some((TaskSize::Minutes, Priority::Level(4)))
        }
        "occasionally" => {
            // Pick it up when the right occasion
            // or state of mind happens.
            Some((TaskSize::Hours, Priority::Level(7)))
        }
        "relaxed" => {
            // Things to do at some particular
            // time of week. Or as a filler? When
            // feeling relaxed?
            Some((TaskSize::Hours, Priority::Level(8)))
        }
        "today" => {
            // Things to do today. Or if missed,
            // the next day..
            Some((TaskSize::Hours, Priority::Today))
        }
        "tonight" => {
            // Things to do before going home or on the way.
            Some((TaskSize::Minutes, Priority::Tonight))
        }
        "ongoing" => {
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
fn inside_parse_variable<'s>(
    builder: &mut TaskInfoDeclarationsBuilder,
    p: ParseableStr<'s>,
) -> Result<ParseableStr<'s>, ParseError> {
    let mut part;
    let rest;
    if let Some((part_, rest_)) = p.split_at_str(",") {
        part = part_;
        rest = rest_;
    } else {
        part = p;
        rest = p.eos();
    };
    part = part.trim();

    if let Some(prio) = part.opt_parse::<ManualPriorityLevel>() {
        builder.set_priority(Priority::Level(prio), part)?;
    } else {
        let ndt = T!(parse_date_time_argument(part, true))?;
        builder.set_priority(Priority::Date(ndt), part)?;
    }
    Ok(rest)
}


// Receives the whole string between '{' and '}'
fn parse_inside<'s>(s: ParseableStr<'s>) -> Result<TaskInfoDeclarations, ParseError> {
    let mut builder = TaskInfoDeclarationsBuilder::default();
    let mut p = s;
    loop {
        p = p.drop_whitespace();
        match p.take_identifier() {
            Ok((ident, rest)) => {
                if rest.starts_with(":") {
                    p = inside_parse_key_val(&mut builder, ident, rest)?;
                } else if let Ok(rest) = expect_comma_or_eos(rest) {
                    // The ident is a single value, not part of key-value pair
                    if let Some((task_size, priority)) = inside_parse_class(ident) {
                        builder.set_tasksize(task_size, ident)?;
                        builder.set_priority(priority, ident)?;
                        p = rest;
                    } else {
                        // Are there any identifiers that are not a
                        // class? 
                        // p = inside_parse_variable(&mut builder, p)?
                        // But so far no (integer priorities are not identifiers), thus:    
                        return Err(parse_error!{
                            message: format!("unknown class {:?}", ident.s),
                            position: ident.position
                        })
                    }
                } else {
                    // "ident non-colon": 
                    // p = inside_parse_variable(&mut builder, p)?
                    // currently not valid, even a date always starts
                    // with a number, not identifier, thus:
                    return Err(parse_error!{
                        message: format!(
                            "expecting a class (identifier), keyword, date or priority"),
                        position: ident.position
                    })
                }
            }
            Err(_e) => if p.is_empty() {
                return Ok(builder.into())
            } else {
                // Not an identifier, so might be a date or number or
                // similar variable thing.
                p = inside_parse_variable(&mut builder, p)?
            }
        }
    }
}

struct NaiveDateTimeWithoutYear {
    position: usize,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8
}

impl NaiveDateTimeWithoutYear {
    pub fn for_year(self, year: u16) -> Result<NaiveDateTime, ParseError> {
        let NaiveDateTimeWithoutYear { position, month, day, hour, minute, second } = self;
        let nd = NaiveDate::from_ymd_opt(year.into(), month.into(), day.into())
            .ok_or_else(|| {
                parse_error! {
                    message: format!("invalid month/day in year {year}"),
                    position
                }
            })?;
        let nt = NaiveTime::from_hms_opt(hour.into(), minute.into(), second.into())
            .ok_or_else(|| {
                parse_error! {
                    message: format!("invalid hh:mm:ss numbers {hour}:{minute}:{second}"),
                    position
                }
            })?;
        Ok(NaiveDateTime::new(nd, nt))
    }
}

struct ParseDatOptions {
    now_for_default_year: Option<NaiveDate>,
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
    NoProperWeekday
}

// This is really the continuation of parsing the date part of a
// datetime value; it expects a separator at first.
fn parse_time_wday<'s>(
    s: ParseableStr<'s>,
    options: &ParseDatOptions,
    month: u8,
    day: u8
) -> Result<(NaiveDateTimeWithoutYear, Option<(Weekday, usize)>, ParseableStr<'s>),
            (ParseTimeWdayErrorKind, ParseError)>
{
    let ((hour, minute, second), rest) =
        (|| -> Result<((u8, u8, u8), ParseableStr<'s>),
                      ParseError> {
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
                            position: digits.position,
                    })
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
                let (hh, rest) = T!(rest.take_n_while(2, is_ascii_digit_char, msg))?;
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
                position: hh.position
            })?;
            let minute = u8::from_str(mm.s).map_err(|e| parse_error! {
                message: format!("can't parse minute {:?}: {e}", mm.s),
                position: mm.position
            })?;
            let second = if let Some(ss) = opt_ss {
                u8::from_str(ss.s).map_err(|e| parse_error! {
                    message: format!("can't parse second {:?}: {e}", ss.s),
                    position: ss.position
                })?
            } else {
                0
            };
            Ok(((hour, minute, second), rest))
        })().map_err(|e| (ParseTimeWdayErrorKind::NoProperTime, e))?;
     
    let datetime = NaiveDateTimeWithoutYear {
        position: s.position, month, day, hour, minute, second
    };

    let weekday_result = (|| -> Result<((Weekday, usize), ParseableStr), ParseError> {
        // "_Sun"
        let rest = T!(rest.expect_separator(&options.separator_between_parts))?;
        let (wdaystr, rest) = rest.take_while(|c: char| c.is_ascii_alphabetic());
        match wdaystr.len() {
            2 | 3 | 4 | 5 | 6 | 7 | 8 => (),
            _ => Err(parse_error! { message: format!("no match for weekday name"),
                                    position: wdaystr.position })?
        }
        let wday = Weekday::from_str(wdaystr.s).map_err(
            |e| parse_error! { message: format!("unknown weekday name {:?}: {e}", wdaystr.s),
                               position: wdaystr.position })?;
        Ok(((wday, wdaystr.position), rest))
    })();
    match weekday_result {
        Ok((weekday_and_position, rest)) => Ok((datetime, Some(weekday_and_position), rest)),
        Err(e) => if options.weekday_is_optional {
            Ok((datetime, None, rest))
        } else {
            Err((ParseTimeWdayErrorKind::NoProperWeekday, e))
        }
    }
}

/// "09-29_205249_Sun"; does not verify weekday (just returns it)
/// because it doesn't have the year. If `weekday_is_optional` is
/// true, still parse weekday if present, but do not report match
/// failures.
fn parse_dat_without_year<'s>(
    s: ParseableStr<'s>,
    options: &ParseDatOptions
) -> Result<(NaiveDateTimeWithoutYear, Option<(Weekday, usize)>, ParseableStr<'s>),
            ParseError>
{
    let rest = s;
    let (month, rest) = T!(rest.take_n_while(
        2, is_ascii_digit_char, "digit as part of month number"))?;
    let month = u8::from_str(month.s).map_err(|e| parse_error! {
        message: format!("can't parse month {:?}: {e}", month.s),
        position: month.position
    })?;
    let rest = T!(rest.expect_separator(&options.date_separator))?;
    let (day, rest) = T!(rest.take_n_while(
        2, is_ascii_digit_char, "digit as part of day number"))?;
    let day = u8::from_str(day.s).map_err(|e| parse_error! {
        message: format!("can't parse day {:?}: {e}", day.s),
        position: day.position
    })?;

    // Try parsing the time; it is allowed to fail if
    // options.time_is_optional is true, but if time succeeds and
    // options.weekday_is_optional is false and weekday fails, then
    // the whole thing should fail, too!
    match parse_time_wday(rest, options, month, day) {
        Ok((ndt_no_year, opt_weekday_position, rest)) =>
            Ok((ndt_no_year, opt_weekday_position, rest)),
        Err((ParseTimeWdayErrorKind::NoProperTime, e)) => if let Some(dt) = options.default_time {
            let ndt_no_year = NaiveDateTimeWithoutYear {
                position: e.position,
                month,
                day,
                hour: dt.hour() as u8,
                minute: dt.minute() as u8,
                second: dt.second() as u8
            };
            Ok((ndt_no_year, None, rest))
        } else {
            Err(parse_error! {
                message: format!("time is required but missing after {:?}",
                s.up_to(rest).s),
                position: rest.position
            })
        },
        Err((ParseTimeWdayErrorKind::NoProperWeekday, e)) => Err(e)
    }
}

// "2024-09-29_205249_Sun"
fn parse_dat<'s>(
    s: ParseableStr<'s>, options: &ParseDatOptions
) -> Result<(NaiveDateTime, ParseableStr<'s>, ParseableStr<'s>), ParseError> {
    match T!(s.take_n_while(4, is_ascii_digit_char, "digit as part of year number")) {
        Ok((year, rest)) => {
            let rest = T!(rest.expect_separator(&options.date_separator))?;
            let (datetime_no_year, opt_weekday_and_position, rest) =
                T!(parse_dat_without_year(rest, options))?;
            let datetime = datetime_no_year.for_year(
                u16::from_str(year.s).map_err(
                    |e| parse_error! {
                        message: e.to_string(),
                        position: year.position
                    })?)?;
            if let Some((weekday, _weekday_position)) = opt_weekday_and_position {
                let date_weekday = datetime.weekday();
                if date_weekday != weekday {
                    Err(parse_error! {
                        message: format!("invalid weekday: given {weekday}, \
                                          but date is for {date_weekday}"),
                        position: s.position
                    })?
                }
            }
            Ok((datetime, s.up_to(rest), rest))
        }
        Err(e1) => {
            if let Some(_now_for_default_year) = options.now_for_default_year {
                // let (datetime_no_year, (weekday, weekday_position), rest) =
                //   parse_dat_without_year(s)?;
                todo!()
            } else {
                Err(e1)?
            }
        }
    }
}

// For parsing user input, to allow "2024-09-29 20:52:49 Sun" and
// similar formats (see parse_date_time_argument tests).
fn flexible_parse_dat_options(default_time: Option<NaiveTime>) -> ParseDatOptions {
    ParseDatOptions {
        now_for_default_year: None,
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
        }
    }
}

// For command line arguments.
fn parse_date_time_argument(s: ParseableStr, time_is_optional: bool)
                                -> Result<NaiveDateTime, ParseError> {
    let (ndt, ndt_string, rest) = parse_dat(
        s, &flexible_parse_dat_options(
            if time_is_optional {
                Some(NaiveTime::from_hms_opt(12, 0, 0).unwrap())
            } else {
                None
            }))?;
    let rest = rest.trim();
    if rest.is_empty() {
        Ok(ndt)
    } else {
        Err(parse_error!{
            message: format!("garbage after datetime string {:?}", ndt_string.s),
            position: rest.position
        })
    }
}

#[cfg(test)]
#[test]
fn t_parse_date_time_argument() {
    let ok = Ok(NaiveDateTime::new(NaiveDate::from_ymd_opt(2024, 9, 29).unwrap(),
                                   NaiveTime::from_hms_opt(20, 52, 49).unwrap()));
    let t = |s: &str| parse_date_time_argument(ParseableStr { position: 0, s }, false);
    let te = |s| t(s).unwrap_err().to_string_in_context(s);
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
    // disallow this one?
    assert_eq!(t("2024-09-29_20:5249 Sun   "), ok);
    // allow this one?
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

    let ok2 = Ok(NaiveDateTime::new(NaiveDate::from_ymd_opt(2024, 9, 29).unwrap(),
                                    NaiveTime::from_hms_opt(12, 0, 0).unwrap()));
    // This t is with value `true`
    let t = |s: &str| parse_date_time_argument(ParseableStr { position: 0, s }, true);
    let te = |s| t(s).unwrap_err().to_string_in_context(s);
    assert_eq!(t("2024/09/29"), ok2);
    assert_eq!(t("2024/09/29 20:52:49 Sun"), ok);
    assert_eq!(te("2024/09/29 foo"),
               "garbage after datetime string \"2024/09/29\" at \"foo\"");
}


const fn parse_path_parse_dat_options(weekday_is_optional: bool) -> ParseDatOptions {
    ParseDatOptions {
        now_for_default_year: None,
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

fn parse_path(id: usize, verbose: bool, region: &Region<PathBuf>, item: &FilePathType)
              -> Result<TaskInfo> {
    let path = item.to_path_buf(region);
    if verbose { eprintln!("parse_path({path:?})") };
    let file_name = item.file_name.to_str().ok_or_else(
        || anyhow!("the file name in path {path:?} does not have valid encoding"))?
        .into_parseable();

    let (dependency_key_ndt, declarations, have_open
    ) = (|| -> Result<(Option<NaiveDateTime>,
                       TaskInfoDeclarations,
                       bool),
                      ParseError> {
        let declarations: TaskInfoDeclarations;
        let have_open: bool;
        let opt_datetime;
        if let Some(rest) = file_name.after_str("OPEN") {
            have_open = true;
            if let Some(rest) = rest.drop_str("{") {
                if let Some((inside, rest)) = rest.split_at_str("}") {
                    if let Some(found) = rest.find_str("OPEN") {
                        return Err(parse_error! {
                            message: "more than one occurrence of 'OPEN'".into(),
                            position: found.position
                        });
                    }
                    declarations = T!(parse_inside(inside))?;
                } else {
                    return Err(parse_error! {
                        message: "missing closing '}' after 'OPEN{'".into(),
                        position: rest.position
                    })
                }
            } else {
                declarations = Default::default();
            }

            // XX when to accept and how when a date is just not there?
            // Maybe if starts with 2020..2050 number then -, go all the
            // way.
            let (datetime, _keystr, _rest) = T!(parse_dat(
                file_name, &PARSE_DAT_OPTIONS_FOR_PATH))?;
            opt_datetime = Some(datetime);

        } else {
            have_open = false;
            declarations = Default::default();

            // XX when to accept and how when a date is just not there?
            // Maybe if starts with 2020..2050 number then -, go all the
            // way.
            if let Ok((datetime, _keystr, _rest)) = parse_dat(
                file_name, &PARSE_DAT_OPTIONS_FOR_PATH)
            {
                opt_datetime = Some(datetime);
            } else {
                opt_datetime = None;
            }
        }


        Ok((opt_datetime, declarations, have_open))
    })().map_err(|e| {
        let ParseError { message, position, .. } = &e;
        anyhow!("error parsing the file name of path {path:?}: {message} at: {:?}{}",
                &file_name.s[*position..], e.backtrace())
    })?;

    let meta = path.metadata()
        .with_context(|| anyhow!("getting metadata of file {path:?}"))?;
    let mtime = meta.modified()
        .with_context(|| anyhow!("getting modified time of file {path:?}"))?;

    let workflow_status = if have_open {
        WorkflowStatus::try_from(&*path).unwrap_or(WorkflowStatus::Open)
    } else {
        // XX even if path has something?
        WorkflowStatus::None
    };
    Ok(TaskInfo {
        id,
        dependency_key: dependency_key_ndt.map(DependencyKey::from),
        workflow_status,
        path,
        mtime,
        calculated_priority: None.into(),
        declarations,
    })
}


fn main() -> Result<()> {
    let opts: Opts = Opts::from_args();
    let now: NaiveDateTime = if let Some(time) = &opts.time {
        parse_date_time_argument(ParseableStr::new(&time), true).map_err(
            |e| anyhow!("can't parse --time option value: {}", e.to_string_in_context(&time)))?
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

    let mut taskinfos: Vec<Rc<TaskInfo>> = Default::default();
    let mut taskinfo_by_key: BTreeMap<DependencyKey, Rc<TaskInfo>> = Default::default();

    let region = Region::new();
    let mut errors = 0;

    for directory in directories {
        for (id, item) in recursive_file_path_types_iter(
            &region, region.store(directory.clone()), itemopts, &excludes, true
        ).enumerate()
        {
            let item = item?; // XX context?
            if item.is_file() || item.is_dir() {
                match parse_path(id, opts.verbose, &region, &item) {
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
        ti.set_initial_priority_level_for(now);
    }

    // Verify dependency links and exert priority inheritance
    for ti in &taskinfos {
        // Do not make priorities on dependencies relevant for (a)
        // files which are not obviously tasks, (b) tasks which
        // are not open.
        if ! ti.workflow_status.is_active() {
            continue;
        }
        
        // Need to recurse to update with the new base number; todo:
        // this is inefficient (O(n^2)).
        let mut recur = |ti: &TaskInfo, list_of_seen| {
            let list_of_seen = cons(ti.id, list_of_seen);
            for dependency in ti.declarations.dependencies.iter() {
                if let Some(dependency_ti) = taskinfo_by_key.get(dependency) {
                    if ! list_of_seen.contains(&dependency_ti.id) {
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
            if ! ti.workflow_status.is_active() {
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
