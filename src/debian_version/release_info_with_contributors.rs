use std::{
    io::Write,
    ops::{Deref, RangeInclusive},
};

use anyhow::{bail, Result};

use crate::{
    debian_version::{
        contributors::{Contributors, MonthYear},
        parse::{Release, ReleaseName, ReleaseNumber},
        release_infos::{release_prefix, ParsedReleaseInfo, ReleaseInfos},
    },
    util::pad::{
        line_up_on_colon, pad_string_dotadjust, pad_string_leftadjust,
    },
};

#[derive(Debug)]
pub struct ReleaseInfoWithContributors {
    pub release_info: ParsedReleaseInfo,
    pub work_period: RangeInclusive<MonthYear>,
}

impl Deref for ReleaseInfoWithContributors {
    type Target = ParsedReleaseInfo;

    fn deref(&self) -> &Self::Target {
        &self.release_info
    }
}

impl ReleaseInfoWithContributors {
    pub fn num_contributors(&self, contributors: &Contributors) -> usize {
        contributors
            .filter_active_in_period(self.work_period.clone())
            .count()
    }

    pub fn fmt(
        &self,
        contributors: &Contributors,
        show_point_releases: bool,
        show_comments: bool,
        mark_release: Option<ReleaseNumber>,
        mut out: impl Write,
    ) -> Result<(), std::io::Error> {
        let ReleaseInfoWithContributors {
            release_info,
            work_period: _,
        } = self;

        let prefix = if let Some(mark_release) = mark_release {
            release_prefix(self.number, mark_release)
        } else {
            ""
        };

        let num_contributors = self.num_contributors(contributors);
        let mut unformatted = release_info.unformatted_lines_main();
        {
            use std::fmt::Write;
            _ = write!(
                &mut unformatted,
                "\n\
                 num_contributors: {num_contributors}"
            );
        }
        out.write_all(
            line_up_on_colon(prefix, unformatted.split('\n'))
                .expect("have ':'")
                .as_bytes(),
        )?;

        if show_point_releases {
            release_info.fmt_point_releases(mark_release, &mut out)?;
        }

        if show_comments {
            release_info.fmt_comments(mark_release, &mut out)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct ReleaseInfosWithContributors {
    pub releases_including_sid: Vec<ReleaseInfoWithContributors>,
}

impl From<ReleaseInfos> for ReleaseInfosWithContributors {
    fn from(value: ReleaseInfos) -> Self {
        let ReleaseInfos { mut releases, sid } = value;
        releases.push(sid);

        let mut releases_including_sid = vec![];
        let mut prev_month_year: Option<MonthYear> = None;
        for release_info in releases {
            let prev =
                prev_month_year.unwrap_or(MonthYear { year: 0, month0: 0 });
            let this = release_info.release_month_year;
            prev_month_year = this;
            let work_period = prev..=this.unwrap_or(MonthYear {
                year: 3000,
                month0: 0,
            });

            releases_including_sid.push(ReleaseInfoWithContributors {
                release_info,
                work_period,
            });
        }
        Self {
            releases_including_sid,
        }
    }
}

impl ReleaseInfosWithContributors {
    pub fn get_release_by_name(
        &self,
        name: &ReleaseName,
    ) -> Result<&ReleaseInfoWithContributors> {
        let all_releases = &self.releases_including_sid;
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

    pub fn get_release_by_number(
        &self,
        number: ReleaseNumber,
    ) -> Result<&ReleaseInfoWithContributors> {
        let all_releases = &self.releases_including_sid;
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

    pub fn get_release(
        &self,
        release: &Release,
    ) -> Result<&ReleaseInfoWithContributors> {
        match release {
            Release::Number(rn) => self.get_release_by_number(*rn),
            Release::Name(n) => self.get_release_by_name(n),
        }
    }

    pub fn list_all_releases(
        &self,
        mark_version: ReleaseNumber,
        mut out: impl Write,
    ) -> Result<()> {
        let all_releases = &self.releases_including_sid;
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
                pad_string_dotadjust(&numstr, 3, Some(3), '.', &mut line);
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
