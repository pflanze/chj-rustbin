use std::{process::Command, str::Utf8Error};

#[derive(Debug, thiserror::Error)]
pub enum GitDirError {
    #[error("Git does not seem to be available")]
    NoGit,
    #[error("can't decode Git path output as UTF-8")]
    Utf8(#[from] Utf8Error),
    #[error(
        "Git exited with code {code} and error {:?}",
        String::from_utf8_lossy(stderr)
    )]
    GitError { stderr: Vec<u8>, code: i32 },
    #[error("expected a string from `git rev-parse --git-dir` ending with .git, but got: {0:?}")]
    ExpectedDotGit(String),
}

/// Return None if cwd not in a Git repository, Some with the path if
/// cwd is, and return an error if the path cannot be decoded as a
/// string or Git is not available.
pub fn git_working_dir() -> Result<Option<String>, GitDirError> {
    let output = Command::new("git")
        .args(&["rev-parse", "--git-dir"])
        .output()
        .map_err(|_| GitDirError::NoGit)?;

    let code: i32 = output.status.code().unwrap_or(-1);

    match code {
        0 => {
            // Path to git state dir
            let dir = std::str::from_utf8(&output.stdout)?.trim();
            // Make working dir path out of it
            if let Some(wd) = dir.strip_suffix(".git") {
                Ok(Some(wd.to_owned()))
            } else {
                Err(GitDirError::ExpectedDotGit(dir.to_owned()))
            }
        }
        128 => Ok(None),
        _ => Err(GitDirError::GitError {
            stderr: output.stderr,
            code,
        }),
    }
}
