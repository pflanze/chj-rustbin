//! Translate between unixtime and Excel date-time values (days since
//! Excel's epoch)

const DAYS_AT_EPOCH: f64 = 25569.;

/// offset_hours: `1.0` represents `+01:00`.
pub fn exceldays_from_unixtime(unixtime: f64, offset_hours: f64) -> f64 {
    unixtime / 86400. + (DAYS_AT_EPOCH + offset_hours / 24.)
}

/// offset_hours: `1.0` represents `+01:00`.
pub fn unixtime_from_exceldays(exceldays: f64, offset_hours: f64) -> f64 {
    (exceldays - (DAYS_AT_EPOCH + offset_hours / 24.)) * 86400.
}

#[cfg(test)]
mod tests {
    use super::*;
    use approx::assert_relative_eq;

    #[test]
    fn t_() {
        fn t(ut: f64, et: f64) {
            assert_relative_eq!(exceldays_from_unixtime(ut, 0.), et);
            assert_relative_eq!(ut, unixtime_from_exceldays(et, 0.));
            assert_relative_eq!(exceldays_from_unixtime(ut, 1.), et + 1./24.);
            assert_relative_eq!(ut, unixtime_from_exceldays(et + 1./24., 1.));
        }
        t(1538352000., 43374.);
    }
}
