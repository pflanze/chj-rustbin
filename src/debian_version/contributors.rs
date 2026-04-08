use std::{ops::RangeInclusive, str::FromStr};

use anyhow::{anyhow, bail, Context, Result};

use crate::util::range_utils::RangeUtils;

macro_rules! trim {
    { $var:tt } => {
        let $var = $var.trim();
    }
}

const MONTHS: &[&str] = &[
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December",
];

/// 0 = January, 11 = December
pub fn parse_month(s: &str) -> Option<u8> {
    MONTHS.iter().position(|m| *m == s).map(|i| i as u8)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct MonthYear {
    pub year: u16,
    pub month0: u8,
}

impl FromStr for MonthYear {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (mo, ye) = s
            .split_once(' ')
            .ok_or_else(|| anyhow!("expecting a space in month/date: {s:?}"))?;
        trim!(mo);
        let month0 =
            parse_month(mo).ok_or_else(|| anyhow!("unknown month {mo:?}"))?;
        trim!(ye);
        let year = ye.parse().with_context(|| anyhow!("year {ye:?}"))?;
        Ok(Self { month0, year })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Contributor<'t> {
    pub name: &'t str,
    pub active: RangeInclusive<MonthYear>,
}

impl<'t> PartialOrd for Contributor<'t> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.name.partial_cmp(&other.name) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        self.active.start().partial_cmp(other.active.start())
    }
}

impl<'t> Ord for Contributor<'t> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).expect("always succeeds")
    }
}

impl<'t> Contributor<'t> {
    /// Parse a single TSV line
    pub fn parse_from_line(s: &'t str) -> Result<Self> {
        let fields: Vec<_> = s.split('\t').collect();
        match fields.as_slice() {
            &[name, from, to] => {
                trim!(name);
                let from: MonthYear = from.parse()?;
                let to: MonthYear = to.parse()?;
                let active = from..=to;
                Ok(Self { name, active })
            }
            _ => bail!("expecting 3 fields, but got {fields:?}"),
        }
    }
}

pub const CONTRIBUTORS: &str =
    include_str!("../../include/debian-contributors.tsv");

#[derive(Debug, Clone)]
pub struct Contributors {
    pub all: Vec<Contributor<'static>>,
}

impl Contributors {
    pub fn new() -> Self {
        let mut all = vec![];
        let mut lines = CONTRIBUTORS.split('\n');
        // Check it?
        let _first_line = lines.next().expect("missing first line");
        for line in lines {
            trim!(line);
            if line.is_empty() {
                continue;
            }
            all.push(
                Contributor::parse_from_line(line)
                    .expect("correct included file"),
            );
        }
        Self { all }
    }

    pub fn filter_active_in_period(
        &self,
        range: RangeInclusive<MonthYear>,
    ) -> impl DoubleEndedIterator<Item = &Contributor<'static>> {
        self.all
            .iter()
            .filter(move |c| c.active.range_overlaps(&range))
    }
}

#[test]
fn t_contributors() {
    let c = Contributors::new();
    assert!(c.all.len() > 100 && c.all.len() < 10000);
    // panic!("{c:#?}");
}
