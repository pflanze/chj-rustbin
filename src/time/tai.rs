use std::time::SystemTime;

use anyhow::{Result, anyhow, bail};
use tai64::Tai64N;
use chrono::{DateTime, Local, Utc};

use crate::{parseutil::{first_rest, take_while, char_is_white, parse_hex, drop_n},
            fp::complement,
            time::excel::exceldays_from_unixtime};


pub fn parse_timestamp(s: &str) -> Result<(Tai64N, &str)> {
    let (c0, r) = first_rest(s).ok_or_else(
        || anyhow!("empty line, missing timestamp"))?;
    if c0 != '@' {
        bail!("line does not start with @")
    }
    let (stamp, rest) = take_while(r, complement(char_is_white));
    if stamp.len() < 24 {
        bail!("timestamp string is too short")
    }
    let stamp8: [u8; 12] = parse_hex(stamp)?;
    let t = Tai64N::from_slice(&stamp8)?;
    Ok((t, drop_n(rest, 1, char_is_white)?))
}

pub trait Tai64Format {
    fn to_rfc2822_local(&self) -> String;
    fn to_rfc2822_utc(&self) -> String;
    fn to_datetime_utc(&self) -> DateTime<Utc>;
    fn to_exceldays(&self, offset_hours: f64) -> f64;
}

impl Tai64Format for Tai64N {
    fn to_rfc2822_local(&self) -> String {
        let t = self.to_system_time();
        let dt: DateTime<Local> = DateTime::from(t);
        dt.to_rfc2822()
    }

    fn to_rfc2822_utc(&self) -> String {
        let t = self.to_system_time();
        let dt: DateTime<Utc> = DateTime::from(t);
        dt.to_rfc2822()
    }

    fn to_datetime_utc(&self) -> DateTime<Utc> {
        let t = self.to_system_time();
        DateTime::from(t)
    }

    /// Convert to Excel's days-since-~1900 values. You need to pass
    /// the correct zone difference, and adapt it for DST. Panics on
    /// potential conversion errors.  offset_hours: `1.0` represents
    /// `+01:00`.
    fn to_exceldays(&self, offset_hours: f64) -> f64 {
        let st = self.to_system_time();
        let t = st.duration_since(SystemTime::UNIX_EPOCH)
            .expect("no overflows?").as_secs_f64();
        exceldays_from_unixtime(t, offset_hours)
    }
}
