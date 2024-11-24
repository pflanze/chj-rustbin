use crate::parse_error;


pub struct NaiveDateTimeWithoutYear {
    pub position: usize,
    pub month: u8,
    pub day: u8,
    pub hour: u8,
    pub minute: u8,
    pub second: u8
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
