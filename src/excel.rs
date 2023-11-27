//! Translate between unixtime and Excel date-time values (days since
//! Excel's epoch)

const DAYS_AT_EPOCH: f64 = 25569.;

pub fn exceldays_from_unixtime(unixtime: f64) -> f64 {
    unixtime / 86400. + DAYS_AT_EPOCH
}

pub fn unixtime_from_exceldays(exceldays: f64) -> f64 {
    (exceldays - DAYS_AT_EPOCH) * 86400.
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_() {
        fn t(ut: f64, et: f64) {
            assert_eq!(exceldays_from_unixtime(ut), et);
            assert_eq!(ut, unixtime_from_exceldays(et));
        }
        t(1538352000., 43374.);
    }
}
