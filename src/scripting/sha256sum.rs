use std::path::Path;

use anyhow::{anyhow, bail, Context, Result};
use bstr::ByteSlice;

use crate::script_tools::cmd_capturing_both;

// Once again.

/// Call out to `sha256sum` tool to calculate the SHA-256 hash as a
/// hex number string
pub fn sha256sum(path: &Path) -> Result<String> {
    let (out, err) = cmd_capturing_both("sha256sum", &[path], &[])?;
    if let Some((hash, _rest)) = out.split_once_str(b" ") {
        String::from_utf8(hash.to_vec()).with_context(|| {
            anyhow!(
                "decoding output of sha256sum tool, {out:?} (stderr: {err:?})"
            )
        })
    } else {
        bail!("sha256sum has not returned a splittable output, but {out:?} (stderr: {err:?})")
    }
}
