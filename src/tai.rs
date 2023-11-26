use anyhow::{Result, anyhow, bail};
use tai64::Tai64N;
use chrono::{DateTime, Local, Utc};

use crate::{parseutil::{first_rest, take_while, char_is_white, parse_hex, drop_n},
            fp::complement};


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
}
