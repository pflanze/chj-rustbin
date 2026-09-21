use std::{
    fs::File,
    io::BufReader,
    num::{NonZeroU32, NonZeroU64},
    ops::{BitXor, ControlFlow, Deref, Range},
    path::Path,
    sync::Arc,
};

use internal_iterator::InternalIterator;
use regex::bytes::Regex;

use crate::{
    by_tmp_ref_iterator::ByTmpRefIterator,
    lines_by_tmp_ref::{
        trim_line_terminator, ContentLinesByTmpRef, ReadLinesByTmpRef,
    },
};

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

/// Report lines in the file at the given path that match (or do not
/// match if `invert`) `regex`
///
/// Note: the iterator (InternalIterator to be precise) only reports
/// the first match for a line!
///
pub fn file_lines_grep<'regex>(
    path: &Path,
    regex: &'regex Regex,
    invert: bool,
    line_terminator: u8,
) -> Result<LinesGrep<'regex, ReadLinesByTmpRef>, std::io::Error> {
    let input = BufReader::new(File::open(path)?);
    let line_no: u64 = 0;
    let lines = ReadLinesByTmpRef::new(input, line_terminator);
    Ok(LinesGrep {
        regex,
        invert,
        line_terminator,
        lines,
        line_no,
    })
}

/// Report lines in the given content that match (or do not match if
/// `invert`) `regex`
///
/// Note: the iterator (InternalIterator to be precise) only reports
/// the first match for a line!
///
pub fn content_lines_grep<'regex>(
    content: Arc<[u8]>,
    regex: &'regex Regex,
    invert: bool,
    line_terminator: u8,
) -> Result<LinesGrep<'regex, ContentLinesByTmpRef>, std::io::Error> {
    let line_no: u64 = 0;
    let lines = ContentLinesByTmpRef::new(content, line_terminator);
    Ok(LinesGrep {
        regex,
        invert,
        line_terminator,
        lines,
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
pub struct LinesGrep<'regex, Lines> {
    regex: &'regex Regex,
    invert: bool,
    line_terminator: u8,
    lines: Lines,
    line_no: u64,
}

impl<'regex, Lines: ByTmpRefIterator<Item = [u8], Error = std::io::Error>>
    InternalIterator for LinesGrep<'regex, Lines>
{
    type Item = Result<(Position128, Vec<u8>), std::io::Error>;

    fn try_for_each<R, F>(self, mut f: F) -> ControlFlow<R>
    where
        F: FnMut(Self::Item) -> ControlFlow<R>,
    {
        let Self {
            regex,
            invert,
            line_terminator,
            mut lines,
            mut line_no,
        } = self;

        while let Some(line) = match lines.next_tmp_ref() {
            Ok(l) => l,
            Err(e) => return f(Err(e)),
        } {
            let trimmed = trim_line_terminator(line, line_terminator);
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

                f(Ok((position, line.to_owned())))?;
            }
            line_no = line_no.saturating_add(1);
        }
        ControlFlow::Continue(())
    }
}

pub struct ContentsWithMatchRange {
    pub contents: Contents,
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
) -> Result<ContentsGrep<'regex>, std::io::Error> {
    let contents = file_contents(path)?;
    Ok(ContentsGrep { regex, contents })
}

pub struct ContentsGrep<'regex> {
    regex: &'regex Regex,
    contents: Contents,
}

impl<'regex> InternalIterator for ContentsGrep<'regex> {
    type Item = ContentsWithMatchRange;

    fn try_for_each<R, F>(self, mut f: F) -> ControlFlow<R>
    where
        F: FnMut(Self::Item) -> ControlFlow<R>,
    {
        let Self { regex, contents } = self;

        for m in regex.find_iter(&contents) {
            let range = m.start()..m.end();
            f(ContentsWithMatchRange {
                contents: contents.clone(),
                range,
            })?;
        }
        ControlFlow::Continue(())
    }
}

#[derive(Debug, Clone)]
pub struct Contents {
    pub contents: Arc<[u8]>,
    pub range: Range<usize>,
}

// No need, Deref works!
// impl Index<Range<usize>> for Contents {
// }

impl Deref for Contents {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.contents[self.range.clone()]
    }
}

impl Contents {
    pub fn as_slice(&self) -> &[u8] {
        let Self { contents, range } = self;
        &contents[range.clone()]
    }

    pub fn grep<'regex, 's>(
        self,
        regex: &'regex Regex,
    ) -> ContentsGrep<'regex> {
        ContentsGrep {
            regex,
            contents: self,
        }
    }
}

pub fn file_contents(path: &Path) -> Result<Contents, std::io::Error> {
    let contents: Arc<[u8]> = std::fs::read(path)?.into();
    let range = 0..contents.len();
    Ok(Contents { contents, range })
}
