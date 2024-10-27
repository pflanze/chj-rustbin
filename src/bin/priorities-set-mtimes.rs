use anyhow::{anyhow, Context};
use filetime::FileTime;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::str::FromStr;

fn main() -> anyhow::Result<()> {
    let file = BufReader::new(File::open("test/priorities.mtimes")?);

    for (index, line) in file.lines().enumerate() {
        let line = line?;
        let parts: Vec<&str> = line.split("\t").collect();

        if parts.len() != 2 {
            anyhow::bail!("The line {} in incorrect", index + 1);
        }
        let timestamp = parts[0];
        let filename = parts[1];

        let timestamp = i64::from_str(timestamp).with_context(|| {
            anyhow!("Parsing the timestamp on line {}", index + 1)
        })?;

        filetime::set_file_mtime(
            filename,
            FileTime::from_unix_time(timestamp, 0),
        )
        .with_context(|| {
            anyhow!("Setting modification time for {}", filename)
        })?;
    }

    Ok(())
}
