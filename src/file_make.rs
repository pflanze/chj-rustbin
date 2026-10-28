//! A make system (file-based like `make`) for use within Rust programs

use std::{io::ErrorKind, path::Path, sync::Arc};

use anyhow::{anyhow, Context};

use crate::utillib::into_arc_path::IntoArcPath;

#[derive(Debug, thiserror::Error)]
pub enum MakeError<E: std::error::Error + Send + Sync + 'static> {
    #[error("{0}")]
    BuildAction(#[from] E),
    #[error("{0}")]
    FileHandling(anyhow::Error),
}

pub fn file_make<TP, E, A>(
    target: TP,
    dependencies: &[&Path],
    build_action: A,
) -> Result<Arc<Path>, MakeError<E>>
where
    TP: IntoArcPath,
    E: std::error::Error + Send + Sync + 'static,
    A: Fn(&Arc<Path>, &[&Path]) -> Result<(), E>,
{
    let target = target.into_arc_path();
    let build = || {
        eprintln!("making target {target:?}");
        build_action(&target, dependencies).map_err(MakeError::BuildAction)
    };
    match target.symlink_metadata() {
        Ok(target_metadata) => {
            if target_metadata.is_symlink() {
                Err(MakeError::FileHandling(anyhow!(
                    "target path exists but is a symlink: {target:?}"
                )))?
            }

            let target_modified = target_metadata
                .modified()
                .with_context(|| {
                    anyhow!("getting modified time for target {target:?}")
                })
                .map_err(MakeError::FileHandling)?;

            let mut needs_build = false;

            for dependency in dependencies {
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
                eprintln!("target {target:?} is up to date");
            }
        }
        Err(e) => match e.kind() {
            ErrorKind::NotFound => build()?,
            _ => (),
        },
    }
    Ok(target)
}
