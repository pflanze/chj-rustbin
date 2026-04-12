//! Reimplementation of `debian-version` from chj-scripts repository,
//! which has gotten too hacky und untyped.

use std::{
    io::{stdout, Write},
    str::FromStr,
};

use anyhow::{anyhow, Context, Result};
use chj_rustbin::debian_version::{
    contributors::Contributors, infos::Infos, parse::DebianVersion,
    release_info::ParsedReleaseInfo,
    release_info_with_contributors::ReleaseInfoWithContributors,
};
use clap::Parser;

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
                    "reading file '/etc/debian_version' -- reading the version \
                     is for Debian only; you can instead pass a version/name \
                     as an argument"
            )
            },
        )?;
        &tmp
    };
    let version = DebianVersion::from_str(version.trim())?;
    let infos: Infos<ReleaseInfoWithContributors> =
        Infos::<ParsedReleaseInfo>::new().into();
    let release_info = infos.get_release(&version.release)?;
    let release_number = version
        .release
        .number()
        .unwrap_or_else(|| release_info.number);
    let mut out = stdout().lock();
    if all {
        let contributors = Contributors::new();
        for r in &infos.infos {
            r.fmt(
                &contributors,
                point,
                comments,
                Some(release_number),
                &mut out,
            )?;
            write!(&mut out, "\n")?;
        }
    } else if list {
        infos.list_all_releases(release_number, &mut out)?;
    } else {
        let contributors = Contributors::new();
        release_info.fmt(
            &contributors,
            point,
            comments,
            if point { Some(release_number) } else { None },
            &mut out,
        )?;
    }
    Ok(())
}
