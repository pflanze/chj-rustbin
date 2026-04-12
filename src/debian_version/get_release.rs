//! Operations on `Infos<T>` where `T` implements `trait
//! ReleaseNameAndNumber`

use anyhow::{bail, Result};

use crate::debian_version::{
    infos::Infos,
    parse::{Release, ReleaseName, ReleaseNumber},
};

pub trait ReleaseNameAndNumber {
    fn name(&self) -> &str;
    fn number(&self) -> ReleaseNumber;
}

impl<T: ReleaseNameAndNumber> Infos<T> {
    pub fn get_release_by_name(&self, name: &ReleaseName) -> Result<&T> {
        let all_releases = &self.infos;
        let items: Vec<_> = all_releases
            .iter()
            .filter(|r| r.name() == name.as_ref())
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

    pub fn get_release_by_number(&self, number: ReleaseNumber) -> Result<&T> {
        let all_releases = &self.infos;
        let items: Vec<_> = all_releases
            .iter()
            .filter(|r| r.number().eq_release(number))
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

    pub fn get_release(&self, release: &Release) -> Result<&T> {
        match release {
            Release::Number(rn) => self.get_release_by_number(*rn),
            Release::Name(n) => self.get_release_by_name(n),
        }
    }
}
