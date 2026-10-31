//! Parse (and generate?) `linux/.config` files

use std::{
    cmp::Ordering,
    collections::{btree_map::Entry, BTreeMap},
    fmt::Display,
    fs::File,
    io::{BufRead, BufReader, Read},
    path::Path,
    str::FromStr,
};

use anyhow::{anyhow, bail, Context, Result};

use crate::mystring::MyString24;

/// A present value ("is not set" is outside this type's scope)
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ConfigValue {
    /// "y" (compiled in)
    Y,
    /// "m" (module)
    M,
    String(MyString24),
    Integer(i128),
    IntegerHex(u64),
}

impl Display for ConfigValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConfigValue::Y => write!(f, "y"),
            ConfigValue::M => write!(f, "m"),
            ConfigValue::String(s) => write!(f, "{s:?}"),
            ConfigValue::Integer(n) => write!(f, "{n}"),
            ConfigValue::IntegerHex(n) => write!(f, "0x{n:x}"),
        }
    }
}

impl FromStr for ConfigValue {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim_end();
        if s == "y" {
            Ok(Self::Y)
        } else if s == "m" {
            Ok(Self::M)
        } else if let Some(rest) = s.strip_prefix('"') {
            if let Some(meat) = rest.strip_suffix('"') {
                // XX what string syntax is the .config format using?
                Ok(Self::String(meat.into()))
            } else {
                bail!("value starts with '\"' but does not end with one: {s:?}")
            }
        } else if let Some(hex) = s.strip_prefix("0x") {
            Ok(Self::IntegerHex(u64::from_str_radix(hex, 16)?))
        } else {
            Ok(Self::Integer(s.parse()?))
        }
    }
}

pub struct Config {
    /// `None` means, a config entry is present but "is not set"
    pub settings: BTreeMap<MyString24, Option<ConfigValue>>,
}

impl Config {
    pub fn from_path(path: &Path) -> Result<Self> {
        Self::from_file(
            path,
            File::open(&path)
                .with_context(|| anyhow!("opening {path:?} for reading"))?,
        )
    }

    pub fn from_file(path: &Path, file: impl Read) -> Result<Self> {
        let mut inp = BufReader::new(file);

        let mut settings: BTreeMap<MyString24, Option<ConfigValue>> =
            Default::default();
        let mut add = |key, val, line_no| -> Result<()> {
            match settings.entry(key) {
                Entry::Occupied(o) => {
                    bail!(
                        "duplicate entry for CONFIG_{} at {}:{line_no}",
                        o.key(),
                        path.display()
                    )
                }
                Entry::Vacant(v) => {
                    v.insert(val);
                    Ok(())
                }
            }
        };
        let mut line = String::new();
        let mut line_no: u64 = 1;
        while inp
            .read_line(&mut line)
            .with_context(|| anyhow!("reading from {path:?}"))?
            > 0
        {
            'parse: {
                let line = line.trim_end();
                if line.is_empty() {
                    break 'parse;
                }
                if line.starts_with("#") {
                    if let Some(rest) = line.strip_prefix("# CONFIG_") {
                        if let Some((key, rest)) = rest.split_once(' ') {
                            if rest.starts_with("is not set") {
                                add(MyString24::from(key), None, line_no)?;
                            } else {
                                bail!(
                                    "expecting 'is not set' after space after CONFIG_* \
                                     at {}:{line_no}",
                                    path.display()
                                )
                            }
                        }
                    } else {
                        // actual comments, ignore
                    }
                } else {
                    if let Some(rest) = line.strip_prefix("CONFIG_") {
                        if let Some((key, val)) = rest.split_once('=') {
                            let val = ConfigValue::from_str(val).with_context(
                                || {
                                    anyhow!(
                                        "parsing string {val:?} at {}:{line_no}",
                                        path.display()
                                    )
                                },
                            )?;
                            add(MyString24::from(key), Some(val), line_no)?;
                        }
                    } else {
                        bail!(
                            "unexpected line not starting with CONFIG_ \
                             at {}:{line_no}",
                            path.display()
                        )
                    }
                }
            }

            line.clear();
            line_no += 1;
        }
        Ok(Self { settings })
    }

    pub fn diff(&self, other: &Self) -> Vec<ConfigDiff> {
        let mut diff = Vec::new();
        let mut ia = self.settings.iter();
        let mut ib = other.settings.iter();
        let mut oa = ia.next();
        let mut ob = ib.next();
        loop {
            if let Some(a) = oa {
                if let Some(b) = ob {
                    match a.0.cmp(&b.0) {
                        Ordering::Less => {
                            diff.push(ConfigDiff {
                                name: a.0.to_owned(),
                                diff: ConfigValueDiff::Disappeared {
                                    from: a.1.clone(),
                                },
                            });
                            oa = ia.next();
                        }
                        Ordering::Equal => {
                            // Same config name
                            if let Some(va) = a.1 {
                                if let Some(vb) = b.1 {
                                    if va == vb {
                                        // unchanged: same enabled value
                                    } else {
                                        diff.push(ConfigDiff {
                                            name: a.0.clone(),
                                            diff: ConfigValueDiff::Changed {
                                                from: va.clone(),
                                                to: vb.clone(),
                                            },
                                        });
                                    }
                                } else {
                                    diff.push(ConfigDiff {
                                        name: a.0.clone(),
                                        diff: ConfigValueDiff::Disabled {
                                            from: va.clone(),
                                        },
                                    });
                                }
                            } else {
                                if let Some(vb) = b.1 {
                                    diff.push(ConfigDiff {
                                        name: a.0.clone(),
                                        diff: ConfigValueDiff::Enabled {
                                            to: vb.clone(),
                                        },
                                    });
                                } else {
                                    // unchanged: both disabled
                                }
                            }
                            oa = ia.next();
                            ob = ib.next();
                        }
                        Ordering::Greater => {
                            diff.push(ConfigDiff {
                                name: b.0.to_owned(),
                                diff: ConfigValueDiff::New { to: b.1.clone() },
                            });
                            ob = ib.next();
                        }
                    }
                } else {
                    diff.push(ConfigDiff {
                        name: a.0.to_owned(),
                        diff: ConfigValueDiff::Disappeared {
                            from: a.1.clone(),
                        },
                    });
                    for (key, opt_val) in ia {
                        diff.push(ConfigDiff {
                            name: key.to_owned(),
                            diff: ConfigValueDiff::Disappeared {
                                from: opt_val.clone(),
                            },
                        });
                    }
                    break;
                }
            } else {
                if let Some((key, opt_val)) = ob {
                    diff.push(ConfigDiff {
                        name: key.to_owned(),
                        diff: ConfigValueDiff::New {
                            to: opt_val.clone(),
                        },
                    });
                    for (key, opt_val) in ib {
                        diff.push(ConfigDiff {
                            name: key.to_owned(),
                            diff: ConfigValueDiff::New {
                                to: opt_val.clone(),
                            },
                        });
                    }
                }
                break;
            }
        }
        diff
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ConfigDiff {
    pub name: MyString24,
    pub diff: ConfigValueDiff,
}

impl Display for ConfigDiff {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { name, diff } = self;

        write!(f, "{name}: {diff}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ConfigValueDiff {
    /// New entry, might be disabled, though
    New { to: Option<ConfigValue> },
    /// Was an entry, might have been disabled, though
    Disappeared { from: Option<ConfigValue> },
    /// Did exist, still exists, but now is not disabled anymore
    Enabled { to: ConfigValue },
    /// Did exist and was enabled, still exists, but now is disabled
    Disabled { from: ConfigValue },
    /// Was and is enabled but changed value
    Changed { from: ConfigValue, to: ConfigValue },
}

struct OptionConfigValue<'s>(&'s Option<ConfigValue>);
impl<'s> Display for OptionConfigValue<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(val) = self.0 {
            write!(f, "= {val}")
        } else {
            f.write_str("not set")
        }
    }
}

impl Display for ConfigValueDiff {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConfigValueDiff::New { to } => {
                write!(f, "new, {}", OptionConfigValue(to))
            }
            ConfigValueDiff::Disappeared { from } => {
                write!(f, "disappeared, was {}", OptionConfigValue(from))
            }
            ConfigValueDiff::Enabled { to } => write!(f, "now enabled, {to}"),
            ConfigValueDiff::Disabled { from } => {
                write!(f, "now disabled, was {from}")
            }
            ConfigValueDiff::Changed { from, to } => {
                write!(f, "changed from {from} to {to}")
            }
        }
    }
}

impl ConfigValueDiff {
    /// Whether a value was or now is not set, and was not present at all in the
    /// other case (i.e. is mostly uninteresting)
    pub fn is_not_set_new_or_disappeared(&self) -> bool {
        match self {
            ConfigValueDiff::New { to } => to.is_none(),
            ConfigValueDiff::Disappeared { from } => from.is_none(),
            ConfigValueDiff::Enabled { to: _ } => false,
            ConfigValueDiff::Disabled { from: _ } => false,
            ConfigValueDiff::Changed { from: _, to: _ } => false,
        }
    }
}

#[cfg(test)]
mod tests {
    // use super::*;

    #[test]
    fn t_diff() {
        eprintln!("TODO XX");
    }
}
