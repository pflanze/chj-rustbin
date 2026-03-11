use std::time::SystemTime;

use anyhow::Result;

pub trait AgeAt {
    fn age_secs_at(&self, now: SystemTime) -> Result<u64>;
    fn age_days_at(&self, now: SystemTime) -> Result<u64>;
}

impl AgeAt for SystemTime {
    fn age_secs_at(&self, now: SystemTime) -> Result<u64> {
        let age = now.duration_since(*self)?;
        Ok(age.as_secs())
    }

    fn age_days_at(&self, now: SystemTime) -> Result<u64> {
        Ok((self.age_secs_at(now)? + 12 * 3600) / (24 * 3600))
    }
}
