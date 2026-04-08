//! Reimplementation of `debian-version` from chj-scripts repository,
//! which has gotten too hacky und untyped.

use std::{
    io::{stdout, Write},
    str::FromStr,
};

use anyhow::{anyhow, Context, Result};
use chj_rustbin::debian_version::{
    parse::DebianVersion,
    release_infos::{ReleaseInfo, ReleaseInfos},
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
