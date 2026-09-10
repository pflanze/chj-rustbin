use std::{
    fs::File,
    io::{BufRead, BufReader},
    ops::BitXor,
    path::Path,
};

use regex::bytes::Regex;

/// Report whether a file contains at least one line matching (or not
/// matching if `invert`) `regex`
///
/// Returns the matched line and the zero-based line number.
pub fn file_lines_grep(
    path: &Path,
    regex: &Regex,
    invert: bool,
    line_terminator: u8,
) -> Result<Option<(usize, Vec<u8>)>, std::io::Error> {
    let mut input = BufReader::new(File::open(path)?);
    let mut line = Vec::new();
    let mut line_no = 0;
    while input.read_until(line_terminator, &mut line)? > 0 {
        let trimmed = if line.last().copied() == Some(line_terminator) {
            &line[0..line.len() - 1]
        } else {
            &line
        };
        if regex.is_match(trimmed).bitxor(invert) {
            return Ok(Some((line_no, line)));
        }
        line_no += 1;
        line.clear();
    }
    Ok(None)
}

/// Report whether a file has contents that matches `regex`, with the
/// whole contents taken as the input for a single match (i.e. not
/// line-based)
///
/// Returns the file contents if it matched.
pub fn file_contents_grep(
    path: &Path,
    regex: &Regex,
    invert: bool,
) -> Result<Option<Vec<u8>>, std::io::Error> {
    let contents = std::fs::read(path)?;
    if regex.is_match(&contents).bitxor(invert) {
        Ok(Some(contents))
    } else {
        Ok(None)
    }
}
