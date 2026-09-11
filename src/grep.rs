use std::{
    fs::File,
    io::{BufRead, BufReader},
    num::{NonZeroU32, NonZeroU64},
    ops::{BitXor, Range},
    path::Path,
};

use regex::bytes::Regex;

/// Position within a file, limited to 32-bit values
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Position64 {
    pub line: NonZeroU32,
    pub column: u32,
}

/// Position within a file with no risk for truncation
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Position128 {
    pub line: NonZeroU64,
    pub column: u64,
}

#[test]
fn t_position_size() {
    assert_eq!(size_of::<Position64>(), 8);
    assert_eq!(size_of::<Option<Position64>>(), 8);
    assert_eq!(size_of::<Position128>(), 16);
    assert_eq!(size_of::<Option<Position128>>(), 16);
}

#[derive(Debug, thiserror::Error)]
#[error("position has a line or column value too large to be converted to Position64: {0:?}")]
pub struct PositionTruncationError(pub Position128);

impl TryFrom<Position128> for Position64 {
    type Error = PositionTruncationError;
    fn try_from(value: Position128) -> Result<Self, PositionTruncationError> {
        let Position128 { line, column } = value;

        let line: u32 = line
            .get()
            .try_into()
            .map_err(|_| PositionTruncationError(value))?;
        let column: u32 = column
            .try_into()
            .map_err(|_| PositionTruncationError(value))?;

        Ok(Position64 {
            line: line
                .try_into()
                .expect("always succeeds because line was already non-zero"),
            column: column as u32,
        })
    }
}

/// Report whether a file contains at least one line matching (or not
/// matching if `invert`) `regex`
///
/// Returns the start position of the match (with column set to 0 if
/// `invert` is true) and the matched line. Note that the position is
/// byte based!
pub fn file_lines_grep(
    path: &Path,
    regex: &Regex,
    invert: bool,
    line_terminator: u8,
) -> Result<Option<(Position128, Vec<u8>)>, std::io::Error> {
    let mut input = BufReader::new(File::open(path)?);
    let mut line = Vec::new();
    let mut line_no: u64 = 0;
    while input.read_until(line_terminator, &mut line)? > 0 {
        let trimmed = if line.last().copied() == Some(line_terminator) {
            &line[0..line.len() - 1]
        } else {
            &line
        };
        let m = regex.find(trimmed);
        let is_match = m.is_some();
        if is_match.bitxor(invert) {
            let position = {
                let line = unsafe {
                    // Safe because the addition guarantees that the value is never zero
                    NonZeroU64::new_unchecked(line_no.saturating_add(1))
                };
                let column = if let Some(m) = m { m.start() as u64 } else { 0 };
                Position128 { line, column }
            };

            return Ok(Some((position, line)));
        }
        line_no = line_no.saturating_add(1);
        line.clear();
    }
    Ok(None)
}

pub struct ContentsWithMatchRange {
    pub contents: Vec<u8>,
    /// Note that the range is byte based!
    pub range: Option<Range<usize>>,
}

impl ContentsWithMatchRange {
    /// Calculate the start position by counting the line terminators
    /// in the contents before the match
    pub fn start_position(&self, line_terminator: u8) -> Option<Position128> {
        self.range.as_ref().map(|range| {
            let start_pos = range.start;
            let runup = &self.contents[0..start_pos];
            let mut last_terminator_pos: usize = 0;
            let mut line: u64 = 1;
            for (i, b) in runup.iter().enumerate() {
                if *b == line_terminator {
                    line = line.saturating_add(1);
                    last_terminator_pos = i;
                }
            }
            let column_usize = start_pos - last_terminator_pos;
            // XX how do better? saturating_into?
            let column: u64 = column_usize as u64;
            Position128 {
                line: line.try_into().expect(
                    "guaranteed since started at 1 and only doing saturating add"
                ),
                column
            }
        })
    }
}

/// Report whether a file has contents that matches `regex`, with the
/// whole contents taken as the input for a single match (i.e. not
/// line-based)
///
/// Returns the file contents if it matched (modulo inversion), with
/// range if the regex actually did match.
pub fn file_contents_grep(
    path: &Path,
    regex: &Regex,
    invert: bool,
) -> Result<Option<ContentsWithMatchRange>, std::io::Error> {
    let contents = std::fs::read(path)?;
    let m = regex.find(&contents);
    let is_match = m.is_some();
    if is_match.bitxor(invert) {
        let range = m.map(|m| m.start()..m.end());
        Ok(Some(ContentsWithMatchRange { contents, range }))
    } else {
        Ok(None)
    }
}
