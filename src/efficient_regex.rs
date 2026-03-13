//! Use more efficient byte based regex matching as long as the given
//! regular expression allows it

use std::{os::unix::ffi::OsStrExt, path::Path};

use log::{debug, info};
use regex::{bytes, Regex, RegexBuilder};

pub enum EfficientRegex {
    RegexForStrings(Regex),
    RegexForBytes(bytes::Regex),
}

impl EfficientRegex {
    pub fn new_either_from(re: &[Regex]) -> Self {
        let mut full_re: Vec<u8> = Vec::new();
        for (i, re) in re.iter().enumerate() {
            use std::io::Write;
            _ = write!(
                &mut full_re,
                "{}({})",
                if i == 0 { "" } else { "|" },
                re.as_str()
            );
        }
        let full_re_str = std::str::from_utf8(&full_re)
            .expect("re were guaranteed to be utf8");
        debug!("{full_re_str:?}");
        let mut b = RegexBuilder::new(&full_re_str);
        b.unicode(false);
        match b.build() {
            Ok(_) => Self::RegexForBytes(
                bytes::Regex::new(&full_re_str)
                    .expect("synt correct and verified OK for bytes?"),
            ),
            Err(_) => {
                info!("fallback to regex matching with unicode");
                Self::RegexForStrings(
                    Regex::new(&full_re_str).expect("syntactically correct"),
                )
            }
        }
    }

    pub fn is_match_path(&self, path: &Path) -> bool {
        match self {
            EfficientRegex::RegexForStrings(regex) => {
                // Would it be better security wise to
                // ignore non-UTF8 paths, or error out?
                let path_str = path.to_string_lossy();
                regex.is_match(&path_str)
            }
            EfficientRegex::RegexForBytes(regex) => {
                let path_bytes = path.as_os_str().as_bytes();
                regex.is_match(path_bytes)
            }
        }
    }
}
