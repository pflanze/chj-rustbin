#[macro_export]
macro_rules! let_format_or {
    { $var:tt, $alternative:expr } => {
        let tmp;
        let $var = if let Some($var) = $var {
            tmp = $var.to_string();
            &tmp
        } else {
            $alternative
        };
    }
}
