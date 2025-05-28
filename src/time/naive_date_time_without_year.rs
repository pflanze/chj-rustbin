use std::convert::TryInto;
use std::fmt::Display;

use crate::parse::parse_error::{
    Backing, IntoOwningBacking, NoContext, ParseContext, ParseError,
    StringParseContext,
};
use crate::parse_error;
use chrono::{Datelike, NaiveDate, NaiveDateTime, NaiveTime, Timelike};

#[derive(Debug, Clone)]
pub struct NaiveDateTimeWithoutYear<C: ParseContext> {
    pub context: C,
    pub month: u8,
    pub day: u8,
    pub hour: u8,
    pub minute: u8,
    pub second: u8,
}

//Can't do this because C includes itself, right?
// impl<'s, C, B> IntoOwningParseContext<B> for NaiveDateTimeWithoutYear<C>
// where C: ParseContext,
//       B: Backing,
//       StringParseContext<B>: From<C>,
// {
//     type Owning = NaiveDateTimeWithoutYear<StringParseContext<B>>;

impl<'s, B: Backing> IntoOwningBacking<B::Owned>
    for NaiveDateTimeWithoutYear<StringParseContext<&'s B>>
where
    &'s B: Backing,
    B::Owned: Backing,
{
    type Owning = NaiveDateTimeWithoutYear<StringParseContext<B::Owned>>;

    fn into_owning_backing(self) -> Self::Owning {
        let Self {
            context,
            month,
            day,
            hour,
            minute,
            second,
        } = self;
        NaiveDateTimeWithoutYear {
            context: context.into_owning_backing(),
            month,
            day,
            hour,
            minute,
            second,
        }
    }
}

impl<C: ParseContext> NaiveDateTimeWithoutYear<C> {
    pub fn with_year(self, year: i32) -> Result<NaiveDateTime, ParseError<C>> {
        let NaiveDateTimeWithoutYear {
            context,
            month,
            day,
            hour,
            minute,
            second,
        } = self;
        (|| -> Result<NaiveDateTime, String> {
            let nd = NaiveDate::from_ymd_opt(year, month.into(), day.into())
                .ok_or_else(|| format!("invalid month/day in year {year}"))?;
            let nt = NaiveTime::from_hms_opt(
                hour.into(),
                minute.into(),
                second.into(),
            )
            .ok_or_else(|| {
                format!("invalid hh:mm:ss numbers {hour}:{minute}:{second}")
            })?;
            Ok(NaiveDateTime::new(nd, nt))
        })()
        .map_err(|message| {
            parse_error! {
                message, context
            }
        })
    }

    pub fn into_naive_date_time(
        self,
    ) -> anyhow::Result<NaiveDateTime, ParseError<C>> {
        Err(
            parse_error! { message: "date is missing year".into(), context: self.context },
        )
    }

    /// Whether `self` come before or after `other` or is the same,
    /// assuming the same year and that the dates are valid for that
    /// assumed year. `NaiveDateTimeWithoutYear` is *not* implementing
    /// Ord / PartialOrd because those also require PartialEq and it's
    /// unclear whether that should include position or not, also
    /// ordering is easily misunderstood, too, since year is actually
    /// unknown.
    pub fn compare<C2: ParseContext>(
        &self,
        other: &NaiveDateTimeWithoutYear<C2>,
    ) -> std::cmp::Ordering {
        self.month.cmp(&other.month).then(
            self.day.cmp(&other.day).then(
                self.hour.cmp(&other.hour).then(
                    self.minute
                        .cmp(&other.minute)
                        .then(self.second.cmp(&other.second)),
                ),
            ),
        )
    }
}

impl<C: ParseContext> Display for NaiveDateTimeWithoutYear<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            context: _,
            month,
            day,
            hour,
            minute,
            second,
        } = self;
        f.write_fmt(format_args!(
            "{month:02}-{day:02} {hour:02}:{minute:02}:{second:02}"
        ))
    }
}

/// Note: uses position 0 always!
impl From<&NaiveDateTime> for NaiveDateTimeWithoutYear<NoContext> {
    fn from(value: &NaiveDateTime) -> Self {
        Self {
            context: NoContext,
            month: value.month().try_into().expect("always fits"),
            day: value.day().try_into().expect("always fits"),
            hour: value.hour().try_into().expect("always fits"),
            minute: value.minute().try_into().expect("always fits"),
            second: value.second().try_into().expect("always fits"),
        }
    }
}
