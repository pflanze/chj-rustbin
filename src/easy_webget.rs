use std::path::Path;

use anyhow::Result;
use bstr::BString;
use cj_path_util::{
    temp_file::unbuffered_temp_file_for, temp_path::TempfileError,
};
use external_webget::{
    Request, RequestError, ResponseToFile, ResponseToVec, WebGet,
};

pub trait EasyWebget {
    fn get_to_file(
        &self,
        url: &str,
        to: impl AsRef<Path>,
    ) -> Result<(), GetToFileError>;
    fn get(self, url: &str) -> Result<BString, RequestError>;
}

#[derive(Debug, thiserror::Error)]
pub enum GetToFileError {
    #[error("{0}")]
    TempfileError(#[from] TempfileError),
    #[error("{0}")]
    RequestError(#[from] RequestError),
}

impl EasyWebget for WebGet {
    fn get_to_file(
        &self,
        url: &str,
        to: impl AsRef<Path>,
    ) -> Result<(), GetToFileError> {
        let request = Request {
            url: url.into(),
            follow: true,
            headers: Default::default(),
        };
        let (temp_path, file) =
            unbuffered_temp_file_for(to.as_ref().to_owned(), None)?
                .into_constituents();
        let () = self.request(&request, ResponseToFile(file))?;
        temp_path.persist()?;
        Ok(())
    }

    fn get(self, url: &str) -> Result<BString, RequestError> {
        let request = Request {
            url: url.into(),
            follow: true,
            headers: Default::default(),
        };
        self.request(&request, ResponseToVec)
    }
}
