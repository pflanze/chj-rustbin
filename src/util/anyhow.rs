use std::fmt::Display;

/// Wrapper around `anyhow::Error` that implements `std::error::Error`
#[derive(Debug)]
pub struct AnyhowError(pub anyhow::Error);

impl Display for AnyhowError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl std::error::Error for AnyhowError {}

impl From<anyhow::Error> for AnyhowError {
    fn from(value: anyhow::Error) -> Self {
        Self(value)
    }
}
