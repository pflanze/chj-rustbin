use anyhow::{anyhow, bail, Context, Result};
use filetime::FileTime;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::str::FromStr;

fn main() -> Result<()> {
    let path = "test/priorities.mtimes";
    let file = BufReader::new(File::open(path).with_context(|| anyhow!("opening {path:?}"))?);

    for (index, line) in file.lines().enumerate() {
        let line = line.with_context(|| anyhow!("reading from {path:?}"))?;
        let parts: Vec<&str> = line.split("\t").collect();

        if parts.len() != 2 {
            bail!("In {path:?}: line {} does not have 2 fields separated by tab", index + 1);
        }
        let timestamp = parts[0];
        let filename = parts[1];

        let timestamp = i64::from_str(timestamp).with_context(|| {
            anyhow!("In {path:?}: parsing the timestamp on line {}", index + 1)
        })?;

        filetime::set_file_mtime(
            filename,
            FileTime::from_unix_time(timestamp, 0),
        )
        .with_context(|| {
            anyhow!("In {path:?}: setting modification time for {:?}", filename)
        })?;
    }

    Ok(())
}
