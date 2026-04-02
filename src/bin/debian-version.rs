//! Reimplementation of `debian-version` from chj-scripts repository,
//! which has gotten too hacky und untyped.

use std::{
    io::{stdout, Write},
    str::FromStr,
};

use anyhow::{anyhow, bail, Context, Result};
use chj_rustbin::{
    debian_version::{DebianVersion, Release, ReleaseName, ReleaseNumber},
    let_format_or,
};
use clap::Parser;

macro_rules! rn {
    { $e:expr } => { {
        use std::str::FromStr;
        ReleaseNumber::from_str(stringify!($e)).expect("enter correct release number constant")
    } }
}

#[derive(Debug)]
struct PointRelease {
    number: ReleaseNumber,
    release_date: &'static str,
}

macro_rules! point_releases {
    { [ ] => { $($collected:tt)* } } => {{
        vec![ $($collected)* ]
    }};

    { [ [ $rn:expr, $rdate:expr ], $($rest:tt)* ] => { $($collected:tt)* } } => {
        point_releases!{
            [ $($rest)* ]
            => {
                $($collected)*
                    PointRelease {
                        number: {
                            use std::str::FromStr;
                            ReleaseNumber::from_str($rn)
                                .expect("enter correct release number constant")
                        },
                        release_date: $rdate
                    },
            }
        }
    };

    { $($e:tt)* } => { {
        point_releases!{[$($e)*] => {} }
    } }
}

#[derive(Debug)]
struct ReleaseInfo {
    name: &'static str,
    number: ReleaseNumber,
    release_date: &'static str,
    num_packages_binary: &'static str,
    num_packages_source: Option<&'static str>,
    num_developers: Option<&'static str>,
    securitysupport_termination_date: Option<&'static str>,
    comments: &'static str,
    point_releases: Vec<PointRelease>,
}

impl ReleaseInfo {
    fn fmt(
        &self,
        show_point_releases: bool,
        show_comments: bool,
        mark_release: Option<ReleaseNumber>,
        mut out: impl Write,
    ) -> Result<(), std::io::Error> {
        let ReleaseInfo {
            name,
            number,
            release_date,
            num_packages_binary,
            num_packages_source,
            num_developers,
            securitysupport_termination_date,
            comments,
            point_releases,
        } = self;

        let prefix = if let Some(mark_release) = mark_release {
            release_prefix(self.number, mark_release)
        } else {
            ""
        };

        let_format_or!(num_packages_source, "?");
        let_format_or!(num_developers, "?");
        let_format_or!(securitysupport_termination_date, "?");
        write!(
            out,
            "{prefix}name               : {name}\n\
             {prefix}number             : {number}\n\
             {prefix}release_date       : {release_date}\n\
             {prefix}securitysupport    : {securitysupport_termination_date}\n\
             {prefix}num_packages_binary: {num_packages_binary}\n\
             {prefix}num_packages_source: {num_packages_source}\n\
             {prefix}num_developers     : {num_developers}\n\
             "
        )?;
        if show_point_releases {
            writeln!(out, "{prefix}point_releases:")?;
            for pr in point_releases {
                let PointRelease {
                    number,
                    release_date,
                } = pr;
                let prefix = if let Some(mark_release) = mark_release {
                    release_prefix(*number, mark_release)
                } else {
                    ""
                };
                let mut line = format!("{prefix} ");
                let number = number.to_string();
                pad_string_dotadjust(&number, 3, 3, &mut line);
                line.push_str(release_date);
                line.push('\n');
                out.write_all(line.as_bytes())?;
            }
        }
        if show_comments {
            writeln!(out, "{prefix}comments:")?;
            for line in comments.split('\n') {
                writeln!(out, "{prefix}{line}")?;
            }
        }
        Ok(())
    }
}

// The single ReleaseInfo is for the sid distribution
fn releases() -> (Vec<ReleaseInfo>, ReleaseInfo) {
    (
        vec![
            ReleaseInfo {
                name: "_",
                number: rn!(0.93), // "0.93 release 6",
                release_date: "1995-10-26",
                num_packages_binary: "hundreds",
                num_packages_source: None,
                num_developers: Some("? (leader Ian Murdock)"),
                securitysupport_termination_date: None,
                comments: r#"
                    "Debian was created by Ian Murdock in 1993".
                    Ian Murdock "I'm going to take a break." 1994-04-01 [april?]
                    (https://lists.debian.org/debian-announce/1994/msg00005.html).
                    https://lists.debian.org/debian-announce/1995/msg00007.html"#,
                point_releases: point_releases![]
            },

            ReleaseInfo {
                name: "_",
                number: rn!(1.1),
                release_date: "1996-06-17",
                num_packages_binary: "474 + 50 non-free",
                num_packages_source: None,
                num_developers: Some(    "100 (leader Bruce Perens)"),
                securitysupport_termination_date: None,
                comments: r#"
                    Ian Murdock stepped down 1996-03-04, handed over to Bruce Perens
                    (https://lists.debian.org/debian-announce/1996/msg00003.html).
                    https://lists.debian.org/debian-announce/1996/msg00021.html"#,
                point_releases: point_releases![]

              },

            // https://lists.debian.org/debian-announce/1996/msg00026.html
            // Wikipedia says "nearly two hundred volunteers"
            ReleaseInfo {
                name: "_",
                number: rn!(1.2),
                release_date: "1996-12-12",
                num_packages_binary: "848",
                num_packages_source: None,
                num_developers: Some("120 (leader Bruce Perens)"),
                securitysupport_termination_date: None,
                comments: "",
                point_releases: point_releases![]
            },

            // https://lists.debian.org/debian-announce/1997/msg00013.html
            ReleaseInfo {
                name: "_",
                number: rn!(1.3),
                release_date: "1997-06-05",
                num_packages_binary: "974",
                num_packages_source: None,
                num_developers: Some("200 (leader Bruce Perens)"),
                securitysupport_termination_date: None,
                comments: "",
                point_releases: point_releases![]
            },

            // Is the 1500+ number for binary packages really?
            // https://lists.debian.org/debian-announce/1998/msg00015.html
            ReleaseInfo {
                name: "hamm",
                number: rn!(2.0),
                release_date: "1998-07-23",
                num_packages_binary: "1500+",
                num_packages_source: None,
                num_developers: Some("400+ (leader Ian Jackson)"),
                securitysupport_termination_date: None,
                comments: "",
                point_releases: point_releases![]
            },

            ReleaseInfo {
                name: "slink",
                number: rn!(2.1),
                release_date: "1999-03-09",
                num_packages_binary: "2250+",
                num_packages_source: None,
                num_developers: Some(">400"),
                securitysupport_termination_date: None,
                comments: r#"
                    "Debian Project Adopts a Constitution" December 14, 1998
                    https://lists.debian.org/debian-announce/1998/msg00047.html

                    First to include apt [ehr, no, was already used for upgrade in hamm; aha:
                        the "apt" tool, vs. apt-get];
                      Alpha + Sparc
                    https://lists.debian.org/debian-announce/1999/msg00005.html"#,
                point_releases: point_releases![]
            },

            ReleaseInfo {
                name: "potato",
                number: rn!(2.2),
                release_date: "2000-08-15",
                num_packages_binary: "3900+",
                num_packages_source: Some("2600+"),
                num_developers: Some( "450+"),
                securitysupport_termination_date: None,
                comments: r#"
                    PowerPC and ARM
                    Introduction of "Testing" distribution ("unstable" did already exist)"#,
                point_releases: point_releases![]
            },

            ReleaseInfo {
                name: "woody",
                number: rn!(3.0),
                release_date: "2002-07-19",
                num_packages_binary: "~8500",
                num_packages_source: None,
                num_developers: Some("900+"),
                securitysupport_termination_date: None,
                comments: "First on DVD media",
                point_releases: point_releases![]
            },

            ReleaseInfo {
                name: "sarge",
                number: rn!(3.1),
                release_date: "2005-06-06",
                num_packages_binary: "~15000",
                num_packages_source: None,
                num_developers: Some( "1500+"),
                securitysupport_termination_date: None,
                comments: r#"
                    *unofficial* AMD64 port; new installer (with RAID, XFS and LVM support)
                    'aptitude as the selected tool for package management'"#,
                point_releases: point_releases![]
            },

            ReleaseInfo {
                name: "etch",
                number: rn!(4.0),
                release_date: "2007-04-08",
                num_packages_binary: "~18200",
                num_packages_source: None,
                num_developers: Some("1030+"),
                securitysupport_termination_date: Some("2010-02-15"),
                comments: "Includes AMD64, but drops m68k (which was still in unstable, though)",
                point_releases: point_releases![]
            },

            ReleaseInfo {
                name: "lenny",
                number: rn!(5.0),
                release_date: "2009-02-14",
                num_packages_binary: ">23000",
                num_packages_source: Some(">12000"),
                num_developers: None,
                securitysupport_termination_date: None,
                comments: r#"
                    Adds support for Marvell's Orion platform, .. now supports several Netbooks, .. also contains the build tools for Emdebian .. includes the new ARM EABI port, ..
                    With the integration of X.Org 7.3 the X server autoconfigures itself with most hardware.
                    Overall improvements for notebooks have been introduced, such as out of the box support of CPU frequency scaling.
                    The availability and updates of OpenJDK, GNU Java compiler, GNU Java bytecode interpreter, Classpath and other free versions of Sun's Java technology, into Debian GNU/Linux 5.0 allow us to ship Java-based applications in Debian's "main" repository.
                    Further improvements in system security include the installation of available security updates before the first reboot by the Debian Installer, the reduction of setuid root binaries and open ports in the standard installation, and the use of GCC hardening features in the builds of several security-critical packages. Various applications have specific improvements, too. PHP for example is now built with the Suhosin hardening patch.
                    In addition to the regular installation media, Debian GNU/Linux can now also be directly used without prior installation. The special images used, known as live images, are available for CDs, USB sticks, and netboot setups. Initially, these are provided for the amd64 and i386 architectures only.
                "#,
                point_releases: point_releases![]
            },

            ReleaseInfo {
                name: "squeeze",
                number: rn!(6.0),
                release_date: "2011-02-06",
                num_packages_binary: ">29000",
                num_packages_source: Some("<15000"),
                num_developers: None,
                securitysupport_termination_date: Some("2016-02-12"),
                comments: r#""#,
                point_releases: point_releases![]
            },

            ReleaseInfo {
                name: "wheezy",
                number: rn!(7.0),
                release_date: "2013-05-04",
                num_packages_binary: ">36000",
                num_packages_source: Some("<17500"),
                num_developers: None,
                securitysupport_termination_date: None,
                comments: r#""#,
                point_releases: point_releases![
                    ["7.1", "2013-06-15"],
                    ["7.2", "2013-10-12"],
                    ["7.3", "2013-12-14"],
                    ["7.4", "2014-02-08"],
                    ["7.5", "2014-04-26"],
                    ["7.6", "2014-07-12"],
                    ["7.7", "2014-10-18"],
                    ["7.8", "2015-01-10"],
                    ["7.9", "2015-09-05"],
                    ["7.10", "2016-04-02"],
                    ["7.11", "2016-06-04 (final update)"],
                ]
            },

            ReleaseInfo {
                name: "jessie",
                number: rn!(8.0),
                release_date: "2015-04-25",
                num_packages_binary: ">43000",
                num_packages_source: Some("20000..20100"),
                num_developers: None,
                securitysupport_termination_date: Some("2020-06-30 (LTS 5y)"),
                comments: r#""#,
                point_releases: point_releases![
                    ["8.1", "2015-06-06"],
                    ["8.2", "2015-09-05"],
                    ["8.3", "2016-01-23"],
                    ["8.4", "2016-04-02"],
                    ["8.5", "2016-06-04"],
                    ["8.6", "2016-09-17"],
                    ["8.7", "2017-01-14"],
                    ["8.8", "2017-05-06"],
                    ["8.9", "2017-08-14"],
                    ["8.10", "2018-02-28"],
                    ["8.11", "2018-08-31 (final update)"],
                ],
            },

            ReleaseInfo {
                name: "stretch",
                number: rn!(9.0),
                release_date: "2017-06-17",
                num_packages_binary: "?",
                num_packages_source: None,
                num_developers: None,
                securitysupport_termination_date: Some("2022-06 (5y)"),
                comments: r#"
                    https://www.debian.org/News/2017/20170617
                    Thanks to the Reproducible Builds project, over 90% of the source packages included in Debian 9 will build bit-for-bit identical binary packages .
                    X display system no longer requires "root" privileges to run.
                    [..] feature the "modern" branch of GnuPG in the "gnupg" package. This brings with it elliptic curve cryptography, better defaults, a more modular architecture, and improved smartcard support.
                    Debug packages are easier to obtain and use in Debian 9 "Stretch". A new "dbg-sym" repository can be added to the APT source list to provide debug symbols automatically for many packages. # ?
                    Support for 32-bit Motorola/IBM PowerPC (powerpc) has been removed .
                "#,
                point_releases: point_releases![
                    ["9.1", "2017-08-14"],
                    ["9.2", "2017-11-09"],
                    ["9.3", "2018-02-28"],
                    ["9.4", "2018-06-07"],
                    ["9.5", "2018-08-31"],
                    ["9.6", "2018-12-11"],
                    ["9.7", "2019-01-23"],
                    ["9.8", "2019-02-16"],
                    ["9.9", "2019-07-01"],
                    ["9.10", "2019-09-07"],
                    ["9.11", "2019-09-08"],
                    // ^ https://www.debian.org/News/2019/20190908
                    // This point release is primarily an update to the
                    // recently-released 9.10, in order to resolve a critical problem
                    // with the installer that was discovered during image testing.
                    ["9.12", "2020-02-08"],
                    ["9.13", "2020-07-18 (final update)" ],
                ]
            },

            ReleaseInfo {
                name: "buster",
                number: rn!(10.0),
                release_date: "2019-07-06",
                num_packages_binary: "59000+",
                num_packages_source: Some("29000-"),
                num_developers: None,
                securitysupport_termination_date: Some("2024-07-06~"),
                comments: r#""#,
                point_releases: point_releases![
                    ["10.1", "2019-09-07"],
                    ["10.2", "2019-11-16"],
                    ["10.3", "2020-02-08"],
                    ["10.4", "2020-05-09"],
                    ["10.5", "2020-08-01"],
                    ["10.6", "2020-09-26"],
                    ["10.7", "2020-12-05"],
                    ["10.8", "2021-02-06"],
                    ["10.9", "2021-03-27"],
                    ["10.10", "2021-06-19"],
                    ["10.11", "2021-10-09"],
                    ["10.12", "2022-03-26"],
                    ["10.13", "2022-09-10"],
                    // ["10.14", ],
                    // ["10.15", ],
                    // ["10.16", ],
                ]
            },

            ReleaseInfo {
                name: "bullseye",
                number: rn!(11.0),
                release_date: "2021-08-14",
                num_packages_binary: "59551",
                num_packages_source: Some("30000+"),
                num_developers: None,
                securitysupport_termination_date: Some("2026-08-14~" ),
                comments: r#""#,
                point_releases: point_releases![
                    ["11.1", "2021-10-09"],
                    ["11.2", "2021-12-18"],
                    ["11.3", "2022-03-26"],
                    ["11.4", "2022-07-09"],
                    ["11.5", "2022-09-10"],
                    ["11.6", "2022-12-17"],
                    ["11.7", "2023-04-29"],
                    ["11.8", "2023-10-07"],
                    ["11.9", "2024-02-10"],
                    ["11.10", "2024-06-29"],
                    ["11.11", "2024-08-31"],
                ]
            },

            ReleaseInfo {
                name: "bookworm",
                number: rn!(12.0),
                release_date: "2023-06-10",
                num_packages_binary: "64419",
                num_packages_source: None,
                num_developers: None,
                securitysupport_termination_date: Some("2028-06-10~"),
                comments: r#""#,
                point_releases: point_releases![
                    ["12.1", "2023-07-22"],
                    ["12.2", "2023-10-07"],
                    // ["12.3", ], # never released? data corruption thus 12.4
                    ["12.4", "2023-12-10"],
                    ["12.5", "2024-02-10"],
                    ["12.6", "2024-06-29"],
                    ["12.7", "2024-08-31"],
                    ["12.8", "2024-11-09"],
                    ["12.9", "2025-01-11"],
                    ["12.10", "2025-03-15"],
                    ["12.11", "2025-05-17"],
                    ["12.12", "2025-09-06"],
                    ["12.13", "2026-01-10"],
                ]
            },

            ReleaseInfo {
                name: "trixie",
                number: rn!(13.0),
                release_date: "2025-08-09",
                num_packages_binary: "69830",
                num_packages_source: None,
                num_developers: None,
                securitysupport_termination_date: Some("2030-08-09~"),
                comments: r#""#,
                point_releases: point_releases![
                    ["13.1", "2025-09-06"],
                    ["13.2", "2025-11-15"],
                    ["13.3", "2026-01-10"],
                    ["13.4", "2026-03-14"],
                ]
            },

            // ReleaseInfo {
            //     name: "_",
            //     number: rn!(),
            //     release_date: ,
            //     num_packages_binary: ,
            //     num_packages_source: ,
            //     num_developers: ,
            //     securitysupport_termination_date: ,
            //     comments: r#""#,
            //     point_releases: point_releases![]
            // },

        ],
        // The next upcoming release (sid)
        ReleaseInfo {
            // Do not call this "sid", but the actual planned release name, if possible!
            name: "forky",
            number: rn!(14.0),
            release_date: "- (this is sid)",
            num_packages_binary: "",
            num_packages_source: None,
            num_developers: None,
            securitysupport_termination_date: None,
            comments: "",
            point_releases: point_releases![]
        }
    )
}

// .is_sorted() is unstable
fn check_sorted(mut items: impl Iterator<Item = ReleaseNumber>) -> Result<()> {
    let mut fst = items.next().expect(" at least 1");
    for item in items {
        if item > fst {
            fst = item;
        } else {
            bail!("release number {item:?} is not larger than {fst:?}")
        }
    }
    Ok(())
}

struct ReleaseInfos {
    releases: Vec<ReleaseInfo>,
    sid: ReleaseInfo,
}

impl ReleaseInfos {
    fn new() -> Self {
        let (releases, sid) = releases();

        // Some checks
        let max_release_number = releases
            .iter()
            .map(|r| r.number)
            .max()
            .expect("at least 1 release");
        assert!(sid.number > max_release_number);
        check_sorted(releases.iter().map(|r| r.number))
            .expect("fix data please");
        for release in &releases {
            let ReleaseInfo {
                name: _,
                number,
                release_date: _,
                num_packages_binary: _,
                num_packages_source: _,
                num_developers: _,
                securitysupport_termination_date: _,
                comments: _,
                point_releases,
            } = release;
            if !point_releases.is_empty() {
                for pr in point_releases {
                    assert!(pr.number.major == number.major);
                    assert!(pr.number > *number);
                }
                check_sorted(point_releases.iter().map(|r| r.number))
                    .expect("fix data");
            }
        }
        Self { releases, sid }
    }

    fn get_release_by_name(&self, name: &ReleaseName) -> Result<&ReleaseInfo> {
        let all_releases = self.all_releases();
        let items: Vec<_> = all_releases
            .iter()
            .filter(|r| r.name == name.as_ref())
            .collect();
        match items.len() {
            0 => bail!("unknown release name {:?}", name.as_ref()),
            1 => Ok(items[0]),
            _ => bail!(
                "buggy data: more than one release with name {:?}",
                name.as_ref()
            ),
        }
    }

    fn get_release_by_number(
        &self,
        number: ReleaseNumber,
    ) -> Result<&ReleaseInfo> {
        let all_releases = self.all_releases();
        let items: Vec<_> = all_releases
            .iter()
            .filter(|r| r.number.eq_release(number))
            .collect();
        match items.len() {
            0 => bail!("unknown release number {}", number.major),
            1 => Ok(items[0]),
            _ => bail!(
                "buggy data: more than one release with major number {}",
                number.major
            ),
        }
    }

    fn get_release(&self, release: &Release) -> Result<&ReleaseInfo> {
        match release {
            Release::Number(rn) => self.get_release_by_number(*rn),
            Release::Name(n) => self.get_release_by_name(n),
        }
    }

    fn all_releases(&self) -> Vec<&ReleaseInfo> {
        let mut all_releases: Vec<_> = self.releases.iter().collect();
        all_releases.push(&self.sid);
        all_releases
    }

    fn list_all_releases(
        &self,
        mark_version: ReleaseNumber,
        mut out: impl Write,
    ) -> Result<()> {
        let all_releases = self.all_releases();
        let name_max_len = all_releases
            .iter()
            .map(|r| r.name.len())
            .max()
            .expect("at least 1");
        for r in all_releases {
            // if r.name == "?" {
            //     continue;
            // }
            let rest = {
                let mut line = String::new();
                pad_string_leftadjust(r.name, name_max_len, &mut line);
                line.push(' ');
                let numstr = r.number.to_string();
                pad_string_dotadjust(&numstr, 3, 3, &mut line);
                line.push_str(r.release_date);
                line
            };
            let prefix = if r.number.eq_release(mark_version) {
                "* "
            } else {
                "  "
            };
            writeln!(out, "{prefix}{rest}")?;
        }
        Ok(())
    }
}

fn pad_string_leftadjust(s: &str, len: usize, out: &mut String) {
    out.push_str(s);
    let slen = s.len();
    if slen < len {
        for _ in 0..(len - slen) {
            out.push(' ');
        }
    }
}

fn pad_string_rightadjust(string: &str, len: usize, out: &mut String) {
    let slen = string.len();
    if slen < len {
        for _ in 0..(len - slen) {
            out.push(' ');
        }
    }
    out.push_str(string);
}

/// Adjust on the first '.' in `string`; if there is no dot, adjust
/// the right end on the dot position
fn pad_string_dotadjust(
    string: &str,
    left_of_dot: usize,
    right_of_dot: usize,
    out: &mut String,
) {
    let mut dotpositions =
        string
            .char_indices()
            .enumerate()
            .filter_map(
                |(chari, (bytei, c))| {
                    if c == '.' {
                        Some((chari, bytei))
                    } else {
                        None
                    }
                },
            );
    if let Some((_chari, bytei)) = dotpositions.next() {
        let left = &string[0..bytei];
        pad_string_rightadjust(left, left_of_dot, out);
        out.push('.');
        let right = &string[bytei + 1..];
        pad_string_leftadjust(right, right_of_dot, out);
    } else {
        pad_string_rightadjust(string, left_of_dot, out);
        for _ in 0..(1 + right_of_dot) {
            out.push(' ');
        }
    }
}

#[derive(clap::Parser, Debug)]
/// Show Debian system version, or a list of all Debian releases
#[clap(name = "debian-version from chj-rustbin")]
struct Opt {
    /// Show tight list
    #[clap(short, long)]
    list: bool,

    /// Show all in full
    #[clap(short, long)]
    all: bool,

    /// Show point releases
    #[clap(short, long)]
    point: bool,

    /// Show comments (a bit messy)
    #[clap(short, long)]
    comments: bool,

    /// Use the given version string instead of reading it from `/etc/debian_version`
    version: Option<String>,
}

fn release_prefix(r1: ReleaseNumber, r2: ReleaseNumber) -> &'static str {
    if r1 == r2 {
        "**"
    } else if r1.eq_release(r2) {
        // Never happens for releases before Etch
        "* "
    } else {
        "  "
    }
}

fn main() -> Result<()> {
    let Opt {
        list,
        all,
        point,
        comments,
        version,
    } = Opt::from_args();

    let tmp;
    let version = if let Some(version) = &version {
        version
    } else {
        tmp = std::fs::read_to_string("/etc/debian_version").with_context(
            || {
                anyhow!(
                "reading file '/etc/debian_version' -- this is for Debian only"
            )
            },
        )?;
        &tmp
    };
    let version = DebianVersion::from_str(version.trim())?;
    let infos = ReleaseInfos::new();
    let release_info: &ReleaseInfo = infos.get_release(&version.release)?;
    let release_number = version
        .release
        .number()
        .unwrap_or_else(|| release_info.number);
    let mut out = stdout().lock();
    if all {
        for r in infos.all_releases() {
            r.fmt(point, comments, Some(release_number), &mut out)?;
            write!(&mut out, "\n")?;
        }
    } else if list {
        infos.list_all_releases(release_number, &mut out)?;
    } else {
        release_info.fmt(
            point,
            comments,
            if point { Some(release_number) } else { None },
            &mut out,
        )?;
    }
    Ok(())
}
