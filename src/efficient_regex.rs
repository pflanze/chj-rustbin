//! Use more efficient byte based regex matching as long as the given
//! regular expression allows it

use std::{collections::BTreeSet, os::unix::ffi::OsStrExt, path::Path};

use log::{debug, info};
use regex::{bytes, Regex, RegexBuilder};

#[derive(Debug)]
pub enum EfficientRegex {
    RegexForStrings(Regex),
    RegexForBytes(bytes::Regex),
    FastMatch(FastMatch),
    FastMatches(Vec<FastMatch>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FastMatch {
    BackupOrCommon,
    Backup,
    Common,
}

impl FastMatch {
    pub fn is_match_path(self, path: &Path) -> bool {
        match self {
            FastMatch::BackupOrCommon => {
                let path_bytes = path.as_os_str().as_bytes();
                fast_match_emacs_backup(path_bytes)
                    || fast_match_common(path_bytes)
            }
            FastMatch::Backup => {
                let path_bytes = path.as_os_str().as_bytes();
                fast_match_emacs_backup(path_bytes)
            }
            FastMatch::Common => {
                let path_bytes = path.as_os_str().as_bytes();
                fast_match_common(path_bytes)
            }
        }
    }
}

pub fn file_name(s: &[u8]) -> &[u8] {
    if let Some((i, _)) = s.iter().enumerate().rfind(|(_, b)| **b == b'/') {
        &s[i + 1..]
    } else {
        s
    }
}

#[test]
fn t_file_name() {
    let t = file_name;
    assert_eq!(t(b"29"), b"29");
    assert_eq!(t(b""), b"");
    assert_eq!(t(b"/"), b"");
    assert_eq!(t(b"/a"), b"a");
    assert_eq!(t(b"abc/def/ghi"), b"ghi");
}

pub fn fast_match_common(s: &[u8]) -> bool {
    let len = s.len();
    if len == 0 {
        return false;
    }
    let file_name = file_name(s);
    file_name == b".METADATA-v2" || file_name == b".git"
}

pub fn fast_match_emacs_backup(s: &[u8]) -> bool {
    let len = s.len();
    (len > 0) && s[len - 1] == b'~'
}

pub fn regex_fast_match_function(re: &Regex) -> Option<FastMatch> {
    let re = re.as_str();
    match re {
        r"(^|/)\.(METADATA-v2|git)(/|$)" => Some(FastMatch::Common),
        "~$" => Some(FastMatch::Backup),
        _ => None,
    }
}

impl EfficientRegex {
    fn try_new_fast_match(re: &[Regex]) -> Option<Self> {
        let fast: BTreeSet<FastMatch> = re
            .iter()
            .map(|re| regex_fast_match_function(re))
            .filter_map(|v| v)
            .collect();
        if fast.len() == re.len() {
            match fast.len() {
                1 => {
                    return Some(EfficientRegex::FastMatch(
                        *fast.iter().next().expect("checked above"),
                    ))
                }
                2 => {
                    if fast.contains(&FastMatch::Backup)
                        && fast.contains(&FastMatch::Common)
                    {
                        return Some(EfficientRegex::FastMatch(
                            FastMatch::BackupOrCommon,
                        ));
                    }
                }
                _ => (),
            };
            Some(EfficientRegex::FastMatches(fast.into_iter().collect()))
        } else {
            None
        }
    }

    pub fn new_either_from(re: &[Regex]) -> Self {
        if let Some(x) = Self::try_new_fast_match(re) {
            info!("using efficient-regex fast path: {x:?}");
            return x;
        }

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
            EfficientRegex::FastMatch(fast_match) => {
                fast_match.is_match_path(path)
            }
            EfficientRegex::FastMatches(fast_matchs) => {
                for fast_match in fast_matchs {
                    if fast_match.is_match_path(path) {
                        return true;
                    }
                }
                false
            }
        }
    }
}
