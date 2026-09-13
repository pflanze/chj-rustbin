use std::{
    fs::File,
    io::{BufRead, BufReader},
    num::{NonZeroU32, NonZeroU64},
    ops::{BitXor, ControlFlow, Range},
    path::Path,
    sync::Arc,
};

use internal_iterator::InternalIterator;
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

pub fn trim_line_terminator(line: &[u8], line_terminator: u8) -> &[u8] {
    if line.last().copied() == Some(line_terminator) {
        &line[0..line.len() - 1]
    } else {
        &line
    }
}

/// Report lines in the file matching (or not matching if `invert`)
/// `regex`
///
/// Note: the iterator (InternalIterator to be precise) only reports
/// the first match for a line!
///
pub fn file_lines_grep<'regex>(
    path: &Path,
    regex: &'regex Regex,
    invert: bool,
    line_terminator: u8,
) -> Result<FileLinesGrep<'regex>, std::io::Error> {
    let input = BufReader::new(File::open(path)?);
    let line = Vec::new();
    let line_no: u64 = 0;
    Ok(FileLinesGrep {
        regex,
        invert,
        line_terminator,
        input,
        line,
        line_no,
    })
}

/// Results for `file_lines_grep`
///
/// Returns the start position of the match (with column set to 0 if
/// `invert` is true) and the matched line. Note that the position is
/// byte based!
///
/// Note: only reports the first match for a line!
///
pub struct FileLinesGrep<'regex> {
    regex: &'regex Regex,
    invert: bool,
    line_terminator: u8,
    input: BufReader<File>,
    line: Vec<u8>,
    line_no: u64,
}

impl<'regex> InternalIterator for FileLinesGrep<'regex> {
    type Item = Result<(Position128, Vec<u8>), std::io::Error>;

    fn try_for_each<R, F>(self, mut f: F) -> ControlFlow<R>
    where
        F: FnMut(Self::Item) -> ControlFlow<R>,
    {
        let Self {
            regex,
            invert,
            line_terminator,
            mut input,
            mut line,
            mut line_no,
        } = self;

        while match input.read_until(line_terminator, &mut line) {
            Ok(n) => n,
            Err(e) => return f(Err(e)),
        } > 0
        {
            let trimmed = trim_line_terminator(&line, line_terminator);
            let m = regex.find(trimmed);
            let is_match = m.is_some();
            if is_match.bitxor(invert) {
                let position = {
                    let line = unsafe {
                        // Safe because the addition guarantees that the value is never zero
                        NonZeroU64::new_unchecked(line_no.saturating_add(1))
                    };
                    let column =
                        if let Some(m) = m { m.start() as u64 } else { 0 };
                    Position128 { line, column }
                };

                f(Ok((position, line.clone())))?;
            }
            line_no = line_no.saturating_add(1);
            line.clear();
        }
        ControlFlow::Continue(())
    }
}

pub struct ContentsWithMatchRange {
    pub contents: Arc<[u8]>,
    /// Note that the range is byte based!
    pub range: Range<usize>,
}

impl ContentsWithMatchRange {
    /// Calculate the start position by counting the line terminators
    /// in the contents before the match
    pub fn start_position(&self, line_terminator: u8) -> Position128 {
        let Self { contents, range } = self;
        let start_pos = range.start;
        let runup = &contents[0..start_pos];
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
                "guaranteed since started at 1 and only doing saturating add",
            ),
            column,
        }
    }
}

/// Report whether a file has contents that matches `regex`, with the
/// whole contents taken as the input for a single match (i.e. not
/// line-based)
///
/// Returns the file contents if it matched (modulo inversion), with
/// range if the regex actually did match.
pub fn file_contents_grep<'regex>(
    path: &Path,
    regex: &'regex Regex,
) -> Result<FileContentsGrep<'regex>, std::io::Error> {
    let contents = std::fs::read(path)?.into();
    Ok(FileContentsGrep { regex, contents })
}

pub struct FileContentsGrep<'regex> {
    regex: &'regex Regex,
    contents: Arc<[u8]>,
}

impl<'regex> InternalIterator for FileContentsGrep<'regex> {
    type Item = ContentsWithMatchRange;

    fn try_for_each<R, F>(self, mut f: F) -> ControlFlow<R>
    where
        F: FnMut(Self::Item) -> ControlFlow<R>,
    {
        let Self { regex, contents } = self;

        for m in regex.find_iter(&contents) {
            let range = m.start()..m.end();
            f(ContentsWithMatchRange {
                contents: Arc::clone(&contents),
                range,
            })?;
        }
        ControlFlow::Continue(())
    }
}
