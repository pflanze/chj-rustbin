//! A make system (file-based like `make`) for use within Rust programs

use std::{io::ErrorKind, path::Path};

use anyhow::{anyhow, Context};

#[derive(Debug, thiserror::Error)]
pub enum MakeError<DependencyError, BuildError>
where
    DependencyError: std::error::Error + Send + Sync + 'static,
    BuildError: std::error::Error + Send + Sync + 'static,
{
    #[error("preparing dependencies: {0:#}")]
    Dependency(#[from] DependencyError),
    #[error("running the build action: {0:#}")]
    BuildAction(BuildError),
    #[error("running the build action: {0:#}")]
    FileHandling(anyhow::Error),
    #[error("target {0:?} is missing and building is disabled")]
    Missing(Box<Path>),
}

// Conflicting implementation:
//
// impl<DependencyError1, BuildError1, DependencyError2, BuildError2>
//     From<MakeError<DependencyError1, BuildError1>>
//     for MakeError<DependencyError2, BuildError2>
// where
//     DependencyError2: From<DependencyError1>,
//     BuildError2: From<BuildError1>,
// ..
//
// Thus instead:

impl<DependencyError, BuildError> MakeError<DependencyError, BuildError>
where
    DependencyError: std::error::Error + Send + Sync + 'static,
    BuildError: std::error::Error + Send + Sync + 'static,
{
    pub fn into_make_error<DependencyError2, BuildError2>(
        self,
    ) -> MakeError<DependencyError2, BuildError2>
    where
        DependencyError2:
            From<DependencyError> + std::error::Error + Send + Sync + 'static,
        BuildError2:
            From<BuildError> + std::error::Error + Send + Sync + 'static,
    {
        match self {
            MakeError::Dependency(e) => {
                MakeError::Dependency(DependencyError2::from(e))
            }
            MakeError::BuildAction(e) => {
                MakeError::BuildAction(BuildError2::from(e))
            }
            MakeError::FileHandling(e) => MakeError::FileHandling(e),
            MakeError::Missing(path) => MakeError::Missing(path),
        }
    }
}

pub trait IntoPathIterator {
    fn into_path_iterator(&self) -> impl Iterator<Item = &Path>;
}

impl<P: AsRef<Path>> IntoPathIterator for &[P] {
    fn into_path_iterator(&self) -> impl Iterator<Item = &Path> {
        self.iter().map(|p| p.as_ref())
    }
}

impl<const N: usize, P: AsRef<Path>> IntoPathIterator for [P; N] {
    fn into_path_iterator(&self) -> impl Iterator<Item = &Path> {
        self.iter().map(|p| p.as_ref())
    }
}

impl IntoPathIterator for () {
    fn into_path_iterator(&self) -> impl Iterator<Item = &Path> {
        let paths: &'static [&'static Path] = &[];
        paths.iter().copied()
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum FileMakeMode {
    /// If a target is missing, build it using the given build action
    #[default]
    Build,
    /// If a target is missing, return a Missing error
    ReportMissing,
}

#[derive(Debug, Default)]
pub struct FileMake {
    pub mode: FileMakeMode,
}

impl FileMake {
    /// Run `build_action` if `target` does not exist or is older than any
    /// of the paths in `dependencies`.
    ///
    /// `target` can be a directory, but symlinks are not accepted.
    pub fn make<
        'path,
        TargetPath,
        Dependencies,
        DependencyError,
        BuildError,
        F,
    >(
        &self,
        target: TargetPath,
        dependencies: Dependencies,
        build_action: F,
    ) -> Result<TargetPath, MakeError<DependencyError, BuildError>>
    where
        TargetPath: AsRef<Path>,
        Dependencies: IntoPathIterator,
        DependencyError: std::error::Error + Send + Sync + 'static,
        BuildError: std::error::Error + Send + Sync + 'static,
        F: Fn(&TargetPath, &Dependencies) -> Result<(), BuildError>,
    {
        let target_ref = target.as_ref();
        let build = || {
            match self.mode {
                FileMakeMode::Build => (),
                FileMakeMode::ReportMissing => {
                    return Err(MakeError::Missing(
                        target_ref.to_owned().into(),
                    ))
                }
            }

            eprintln!("making target {target_ref:?}");
            build_action(&target, &dependencies).map_err(MakeError::BuildAction)
        };
        match target_ref.symlink_metadata() {
            Ok(target_metadata) => {
                if target_metadata.is_symlink() {
                    Err(MakeError::FileHandling(anyhow!(
                        "target path exists but is a symlink: {target_ref:?}"
                    )))?
                }

                let target_modified = target_metadata
                    .modified()
                    .with_context(|| {
                        anyhow!(
                            "getting modified time for target {target_ref:?}"
                        )
                    })
                    .map_err(MakeError::FileHandling)?;

                let mut needs_build = false;

                for dependency in dependencies.into_path_iterator() {
                    let dependency_metadata = dependency
                        .metadata()
                        .with_context(|| {
                            anyhow!(
                            "can't get metadata of dependency {dependency:?}"
                        )
                        })
                        .map_err(MakeError::FileHandling)?;
                    if dependency_metadata.is_symlink() {
                        Err(MakeError::FileHandling(anyhow!(
                        "dependency path exists but is a symlink: {dependency:?}"
                    )))?
                    }
                    let dependency_modified = dependency_metadata
                    .modified()
                    .with_context(|| {
                        anyhow!(
                            "getting modified time for dependency {dependency:?}"
                        )
                    })
                    .map_err(MakeError::FileHandling)?;
                    // Do not use `>=` as that fails when hard-linking (or
                    // cp -a) files, them having identical mtime. Be
                    // careful, though, overwrites to the dependency would
                    // also update the target!
                    if dependency_modified > target_modified {
                        needs_build = true;
                    }
                }

                if needs_build {
                    build()?;
                } else {
                    eprintln!("target {target_ref:?} is up to date");
                }
            }
            Err(e) => match e.kind() {
                ErrorKind::NotFound => build()?,
                _ => (),
            },
        }
        Ok(target)
    }
}
