//! Parse `/etc/debian_version` files from Debian

use std::{fmt::Display, str::FromStr};

use anyhow::{anyhow, bail, Context, Result};
use kstring::KString;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ReleaseNumber {
    pub major: u16,
    pub minor: Option<u16>,
}

impl ReleaseNumber {
    /// For older releases (before Etch), checks the full number, as
    /// major was used across release names. From Etch onwards, only
    /// major is checked to be equal.
    pub fn eq_release(self, other: Self) -> bool {
        if self.major >= 4 {
            self.major == other.major
        } else {
            self == other
        }
    }
}

impl Display for ReleaseNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { major, minor } = self;
        if let Some(minor) = minor {
            write!(f, "{major}.{minor}")
        } else {
            write!(f, "{major}")
        }
    }
}

impl FromStr for ReleaseNumber {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split('.');
        let p0 = parts.next().expect("split always returns at least 1 part");
        let major = u16::from_str(p0).with_context(|| {
            anyhow!("expecting a number string, got: {p0:?}")
        })?;
        if let Some(p1) = parts.next() {
            if let Some(_) = parts.next() {
                bail!("expecting major.minor version number, but got more parts: {s:?}");
            }
            let minor = Some(u16::from_str(p1).with_context(|| {
                anyhow!("expecting a number string, got: {p1:?}")
            })?);
            Ok(Self { major, minor })
        } else {
            Ok(Self { major, minor: None })
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ReleaseName(KString);

impl AsRef<str> for ReleaseName {
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

impl FromStr for ReleaseName {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() == 0 {
            bail!("release name cannot be the empty string");
        }
        if s.contains(|c: char| !c.is_ascii_alphabetic()) {
            bail!("release name cannot contain non-ascii characters: {s:?}");
        }
        Ok(Self(KString::from_ref(s)))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Release {
    Number(ReleaseNumber),
    Name(ReleaseName),
}

impl Release {
    pub fn number(&self) -> Option<ReleaseNumber> {
        match self {
            Release::Number(n) => Some(*n),
            Release::Name(_) => None,
        }
    }
}

impl FromStr for Release {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        match ReleaseNumber::from_str(s) {
            Ok(rn) => Ok(Release::Number(rn)),
            Err(e) => {
                if s.contains(|c: char| c.is_ascii_digit()) {
                    bail!(
                        "expecting release name or number, this contains digits \
                         but doesn't parse as a release number: {s:?}: {e:#}"
                    )
                }
                Ok(Release::Name(ReleaseName::from_str(s)?))
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DebianVersion {
    pub release: Release,
    /// If the version string contained a single slash, this is the
    /// part after the slash.
    pub rest: Option<KString>,
}

impl FromStr for DebianVersion {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        let mut parts = s.split('/');
        let p0 = parts.next().expect("split always returns at least 1 part");
        let release = Release::from_str(p0)?;
        if let Some(p1) = parts.next() {
            if let Some(_) = parts.next() {
                bail!("debian version can't contain more than one slash: {s:?}")
            }
            Ok(Self {
                release,
                rest: Some(KString::from_ref(p1)),
            })
        } else {
            Ok(Self {
                release,
                rest: None,
            })
        }
    }
}

#[test]
fn t_parse() -> Result<()> {
    let t = |s: &str| DebianVersion::from_str(s);
    assert_eq!(
        t("forky/sid")?,
        DebianVersion {
            release: Release::Name(ReleaseName(KString::from_ref("forky"))),
            rest: Some(KString::from_ref("sid"))
        }
    );
    // Presumably, once it's not sid anymore?:
    assert_eq!(
        t("forky")?,
        DebianVersion {
            release: Release::Name(ReleaseName(KString::from_ref("forky"))),
            rest: None
        }
    );
    // Or, perhaps:
    assert_eq!(
        t("forky/stable")?,
        DebianVersion {
            release: Release::Name(ReleaseName(KString::from_ref("forky"))),
            rest: Some(KString::from_ref("stable"))
        }
    );

    assert_eq!(
        t("13.4")?,
        DebianVersion {
            release: Release::Number(ReleaseNumber {
                major: 13,
                minor: Some(4)
            }),
            rest: None
        }
    );
    assert_eq!(
        t("7")?,
        DebianVersion {
            release: Release::Number(ReleaseNumber {
                major: 7,
                minor: None
            }),
            rest: None
        }
    );
    Ok(())
}
