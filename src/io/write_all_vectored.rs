use std::io::{IoSlice, Write};

use anyhow::{bail, Result};

pub fn write_all_vectored(
    ioslices: &[IoSlice],
    tot_size: usize,
    mut outp: impl Write,
) -> Result<()> {
    let written = outp.write_vectored(&ioslices)?;
    if written != tot_size {
        bail!(
            "could only write {written} out of {tot_size} bytes; \
             todo: change to `write_all_vectored` once stable"
        )
    }
    Ok(())
}
