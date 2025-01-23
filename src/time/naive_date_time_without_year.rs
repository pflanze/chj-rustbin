use std::convert::TryInto;
use std::fmt::Display;

use chrono::{NaiveDate, NaiveDateTime, NaiveTime, Datelike, Timelike};
use crate::parse::parse_error::ParseError;
use crate::parse_error;


#[derive(Debug, Clone)]
pub struct NaiveDateTimeWithoutYear {
    pub position: usize,
    pub month: u8,
    pub day: u8,
    pub hour: u8,
    pub minute: u8,
    pub second: u8
}

impl NaiveDateTimeWithoutYear {
    pub fn for_year(&self, year: i32) -> Result<NaiveDateTime, ParseError> {
        let NaiveDateTimeWithoutYear { position, month, day, hour, minute, second } = self;
        let nd = NaiveDate::from_ymd_opt(year, (*month).into(), (*day).into())
            .ok_or_else(|| {
                parse_error! {
                    message: format!("invalid month/day in year {year}"),
                    position: *position
                }
            })?;
        let nt = NaiveTime::from_hms_opt((*hour).into(), (*minute).into(), (*second).into())
            .ok_or_else(|| {
                parse_error! {
                    message: format!("invalid hh:mm:ss numbers {hour}:{minute}:{second}"),
                    position: *position
                }
            })?;
        Ok(NaiveDateTime::new(nd, nt))
    }

    pub fn to_naive_date_time(&self) -> anyhow::Result<NaiveDateTime, ParseError> {
        Err(parse_error!{ message: "date is missing year".into(), position: self.position })
    }

    /// Whether `self` come before or after `other` or is the same,
    /// assuming the same year and that the dates are valid for that
    /// assumed year. `NaiveDateTimeWithoutYear` is *not* implementing
    /// Ord / PartialOrd because those also require PartialEq and it's
    /// unclear whether that should include position or not, also
    /// ordering is easily misunderstood, too, since year is actually
    /// unknown.
    pub fn compare(&self, other: &Self) -> std::cmp::Ordering {
        self.month.cmp(&other.month).then(
            self.day.cmp(&other.day).then(
                self.hour.cmp(&other.hour).then(
                    self.minute.cmp(&other.minute).then(
                        self.second.cmp(&other.second)))))
    }
}

impl Display for NaiveDateTimeWithoutYear {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { position: _, month, day, hour, minute, second  } = self;
        f.write_fmt(format_args!("{month:02}-{day:02} {hour:02}:{minute:02}:{second:02}"))
    }
}

/// Note: uses position 0 always!
impl From<&NaiveDateTime> for NaiveDateTimeWithoutYear {
    fn from(value: &NaiveDateTime) -> Self {
        Self {
            position: 0,
            month: value.month().try_into().expect("always fits"),
            day: value.day().try_into().expect("always fits"),
            hour: value.hour().try_into().expect("always fits"),
            minute: value.minute().try_into().expect("always fits"),
            second: value.second().try_into().expect("always fits"),
        }
    }
}
