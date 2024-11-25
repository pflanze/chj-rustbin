use chrono::{NaiveDate, NaiveDateTime, NaiveTime};
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
    pub fn for_year(&self, year: u16) -> Result<NaiveDateTime, ParseError> {
        let NaiveDateTimeWithoutYear { position, month, day, hour, minute, second } = self;
        let nd = NaiveDate::from_ymd_opt(year.into(), (*month).into(), (*day).into())
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
}
