//! Represent a Linux kernel version

use std::{
    collections::BTreeMap,
    fmt::Display,
    ops::{Range, RangeInclusive},
    path::Path,
    str::FromStr,
};

use anyhow::{anyhow, bail, Context, Result};
use bstr::ByteSlice;
use cj_path_util::path_util::AppendToPath;

use crate::script_tools::ScriptContext;

/// XX TODO: PartialOrd is probably not correct if extraversion is
/// non-`Null`
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct KernelVersion {
    pub version: u16,
    pub patchlevel: u16,
    pub sublevel: u16,
    pub extraversion: Option<String>,
    /// "+...", e.g. added by Debian, present in `uname -r`
    pub packageversion: Option<String>,
}

impl FromStr for KernelVersion {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim_end();
        let (version_str, rest) = s.split_once('.').ok_or_else(|| {
            anyhow!("missing dot after 'version' number in {s:?}")
        })?;
        let (patchlevel_str, rest) = rest.split_once('.').ok_or_else(|| {
            anyhow!("missing dot after 'patchlevel' number in {s:?}")
        })?;
        let (pre_rest, packageversion) = match rest.split_once('+') {
            Some((pr, pv)) => (pr, Some(pv)),
            None => (rest, None),
        };
        let (sublevel_str, extraversion) = match pre_rest.split_once('-') {
            Some((s, r)) => (s, Some(r)),
            None => (pre_rest, None),
        };
        Ok(Self {
            version: version_str.parse().with_context(|| {
                anyhow!("parsing 'version' number in {s:?}")
            })?,
            patchlevel: patchlevel_str.parse().with_context(|| {
                anyhow!("parsing 'patchlevel' number in {s:?}")
            })?,
            sublevel: sublevel_str.parse().with_context(|| {
                anyhow!("parsing 'sublevel' number in {s:?}")
            })?,
            extraversion: extraversion.map(ToOwned::to_owned),
            packageversion: packageversion.map(ToOwned::to_owned),
        })
    }
}

impl Display for KernelVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            version,
            patchlevel,
            sublevel,
            extraversion,
            packageversion,
        } = self;
        let mut s: Vec<u8> =
            format!("{version}.{patchlevel}.{sublevel}").into();
        use std::io::Write;
        if let Some(extraversion) = extraversion {
            _ = write!(s, "-{extraversion}");
        }
        if let Some(packageversion) = packageversion {
            _ = write!(s, "+{packageversion}");
        }
        f.write_str(std::str::from_utf8(&s).expect("inputs were all strings"))
    }
}

fn get_field<T: FromStr>(
    keyvals: &BTreeMap<String, String>,
    field: &str,
) -> Result<T>
where
    <T as FromStr>::Err: Send + Sync + std::error::Error + 'static,
{
    Ok(keyvals
        .get(field)
        .ok_or_else(|| anyhow!("missing {field:?} field"))?
        .parse()
        .with_context(|| anyhow!("parsing {field:?}"))?)
}

fn get_field_opt<T: FromStr>(
    keyvals: &BTreeMap<String, String>,
    field: &str,
) -> Result<Option<T>>
where
    <T as FromStr>::Err: Send + Sync + std::error::Error + 'static,
{
    let s = keyvals
        .get(field)
        .ok_or_else(|| anyhow!("missing {field:?} field"))?;
    if s.is_empty() {
        Ok(None)
    } else {
        Ok(Some(
            s.parse().with_context(|| anyhow!("parsing {field:?}"))?,
        ))
    }
}

impl KernelVersion {
    /// Read version from the `Makefile` in the given directory
    pub fn from_makefile_in_directory<'t>(
        ctx: &ScriptContext,
        directory: &Path,
    ) -> Result<Self> {
        let path = directory.append("Makefile");
        let m = ctx.catfile(path)?;
        let keyvals: BTreeMap<String, String> = m
            .lines()
            .map(|line| line.trim())
            .take_while(|line| !line.is_empty())
            .filter(|line| !line.starts_with('#'))
            .map(|line| -> Result<_> {
                let (key, val) = line.split_once('=').ok_or_else(|| {
                    anyhow!("file expected to start with key = val lines")
                })?;
                Ok((key.trim().to_owned(), val.trim().to_owned()))
            })
            .collect::<Result<_>>()?;
        let version = get_field(&keyvals, "VERSION")?;
        let patchlevel = get_field(&keyvals, "PATCHLEVEL")?;
        let sublevel = get_field(&keyvals, "SUBLEVEL")?;
        let extraversion = get_field_opt(&keyvals, "EXTRAVERSION")?;
        Ok(Self {
            version,
            patchlevel,
            sublevel,
            extraversion,
            packageversion: None,
        })
    }

    /// Gets the version of the running Linux kernel via `uname -r`
    pub fn of_running_kernel() -> Result<Self> {
        let ctx = ScriptContext::default(); // assumes no tracing
        let version = ctx.cmd_backtick("uname", ["-r"], &[])?;
        Self::from_str(version.to_str()?)
    }

    /// Drops the extraversion and packageversion
    pub fn sublevel_add(&self, sublevel_increment: u16) -> Self {
        let Self {
            version,
            patchlevel,
            sublevel,
            extraversion: _,
            packageversion: _,
        } = self;
        Self {
            version: *version,
            patchlevel: *patchlevel,
            sublevel: *sublevel + sublevel_increment,
            extraversion: None,
            packageversion: None,
        }
    }

    /// Drops the extraversion and packageversion and sets the sublevel to 0
    pub fn patchlevel_add(&self, patchlevel_increment: u16) -> Self {
        let Self {
            version,
            patchlevel,
            sublevel: _,
            extraversion: _,
            packageversion: _,
        } = self;
        Self {
            version: *version,
            patchlevel: *patchlevel + patchlevel_increment,
            sublevel: 0,
            extraversion: None,
            packageversion: None,
        }
    }

    pub fn sublevels(&self, other: &Self) -> Result<(u16, u16)> {
        if self.version != other.version {
            bail!("major version fields are not equal, {} vs. {} (in {self} vs. {other})",
                  self.version, other.version
            )
        }
        if self.patchlevel != other.patchlevel {
            bail!("patchlevel fields are not equal, {} vs. {} (in {self} vs. {other})",
                  self.patchlevel, other.patchlevel
            )
        }
        if self.extraversion.is_some() {
            bail!("left version has extraversion: {self}")
        }
        if other.extraversion.is_some() {
            bail!("right version has extraversion: {other}")
        }
        Ok((self.sublevel, other.sublevel))
    }

    pub fn sublevel_range_exclusive(&self, other: &Self) -> Result<Range<u16>> {
        let (a, b) = self.sublevels(other)?;
        Ok(a..b)
    }

    pub fn sublevel_range_inclusive(
        &self,
        other: &Self,
    ) -> Result<RangeInclusive<u16>> {
        let (a, b) = self.sublevels(other)?;
        Ok(a..=b)
    }

    pub fn patch_filename_to_next(&self) -> String {
        format!("patch-{self}-{}.xz", self.sublevel + 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_() -> Result<()> {
        let cases = &[
            (
                "7.0.10",
                KernelVersion {
                    version: 7,
                    patchlevel: 0,
                    sublevel: 10,
                    extraversion: None,
                    packageversion: None,
                },
            ),
            (
                "7.0.10-pre1",
                KernelVersion {
                    version: 7,
                    patchlevel: 0,
                    sublevel: 10,
                    extraversion: Some("pre1".into()),
                    packageversion: None,
                },
            ),
            (
                "6.12.115+deb13.1-amd64",
                KernelVersion {
                    version: 6,
                    patchlevel: 12,
                    sublevel: 115,
                    extraversion: None,
                    packageversion: Some("deb13.1-amd64".into()),
                },
            ),
        ];

        for (version_str, expected_version) in cases {
            let got = KernelVersion::from_str(version_str)
                .with_context(|| anyhow!("parsing {version_str:?}"))?;
            assert_eq!(&got, expected_version);
            assert_eq!(&expected_version.to_string(), version_str);
        }

        Ok(())
    }
}
