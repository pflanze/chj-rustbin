use std::{
    fs::File,
    io::BufReader,
    num::{NonZeroU32, NonZeroU64},
    ops::{Add, BitXor, ControlFlow, Deref, Range},
    path::Path,
    sync::Arc,
};

use bstr::{BStr, ByteSlice};
use internal_iterator::InternalIterator;
use regex::bytes::Regex;

use crate::{
    by_tmp_ref_iterator::ByTmpRefIterator,
    lines_by_tmp_ref::{
        trim_line_terminator, ContentLinesByTmpRef, ReadLinesByTmpRef,
    },
};

fn range_add(outer: Range<usize>, inner: Range<usize>) -> Range<usize> {
    let start = outer.start + inner.start;
    let end = outer.start + inner.end;
    // Ignore outer.end, just assume that inner fits within outer?
    start..end
}

/// Calculate a range within a window denoted by `outer`, but given as
/// a `inner` window in the same backing as `outer` is. (I.e. the
/// result is using small numbers.)
fn range_within(
    inner: Range<usize>,
    frame: Range<usize>,
) -> Option<Range<usize>> {
    let start = inner.start.max(frame.start);
    let end = inner.end.min(frame.end);
    let intersection = start..end;
    if intersection.is_empty() {
        None
    } else {
        Some((start - frame.start)..(end - frame.start))
    }
}

#[test]
fn t_range_within() {
    let t = range_within;
    assert_eq!(t(110..120, 100..200), Some(10..20));
    assert_eq!(t(90..120, 100..200), Some(0..20));
    assert_eq!(t(110..220, 100..200), Some(10..100));
    assert_eq!(t(100..200, 100..200), Some(0..100));
    assert_eq!(t(90..220, 100..200), Some(0..100));
    assert_eq!(t(220..230, 100..200), None);
    assert_eq!(t(200..230, 100..200), None);
    assert_eq!(t(90..100, 100..200), None);
    // XX now also test with start and end jumbled?
}

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

const NON_ZERO_ONE: NonZeroU64 = unsafe { NonZeroU64::new_unchecked(1) };

impl Add for Position128 {
    type Output = Position128;

    // XX a + b != b + a  !
    /// `rhs` is assumed to be a local position inside a window after
    /// `self` in an outer context; returns the position in that outer
    /// context.
    fn add(self, rhs: Self) -> Self::Output {
        if rhs.line == NON_ZERO_ONE {
            let Self { line, column } = self;
            Self {
                line,
                column: column + rhs.column,
            }
        } else {
            Self {
                line: self.line.saturating_add(rhs.line.get()),
                column: rhs.column,
            }
        }
    }
}

impl Position128 {
    /// Not called `ZERO` because it starts at line 1
    const TOP_LEFT: Position128 = Position128 {
        line: unsafe {
            // 1 is NonZero
            NonZeroU64::new_unchecked(1)
        },
        column: 0,
    };

    /// `run_up` is the contents before the
    pub fn from_run_up(run_up: &[u8], line_terminator: u8) -> Position128 {
        let mut last_terminator_pos: usize = 0;
        let mut line: u64 = 1;
        for (i, b) in run_up.iter().enumerate() {
            if *b == line_terminator {
                line = line.saturating_add(1);
                last_terminator_pos = i;
            }
        }
        let column_usize = run_up.len() - last_terminator_pos;
        // XX how do better? saturating_into?
        let column: u64 = column_usize as u64;
        Position128 {
            line: line.try_into().expect(
                "guaranteed since started at 1 and only doing saturating add",
            ),
            column,
        }
    }

    pub fn inc_line(&mut self) {
        self.line = self.line.saturating_add(1);
        self.column = 0;
    }
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

/// Same as `file_lines_grep` but loads (or mmap's, todo) the file,
/// then shares its contents in all match results via `Arc`
pub fn file_lines_grep_via_contents<'regex>(
    path: &Path,
    regex: &'regex Regex,
    invert: bool,
    line_terminator: u8,
) -> Result<LinesGrepContents<'regex>, std::io::Error> {
    let lines = file_contents(path, line_terminator)?;
    Ok(LinesGrepContents {
        regex,
        invert,
        lines,
    })
}

/// Iterator returning line matches for the given regex
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
        let LinesGrep {
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

/// Iterator returning line matches for the given regex
///
/// Returns the start position of the match (with column set to 0 if
/// `invert` is true) and the matched line. Note that the position is
/// byte based!
///
/// Note: only reports the first match for a line!
///
pub struct LinesGrepContents<'regex> {
    regex: &'regex Regex,
    invert: bool,
    lines: Contents,
}

impl<'regex> InternalIterator for LinesGrepContents<'regex> {
    type Item = ContentsWithMatchRange;

    fn try_for_each<R, F>(self, mut f: F) -> ControlFlow<R>
    where
        F: FnMut(Self::Item) -> ControlFlow<R>,
    {
        let LinesGrepContents {
            regex,
            invert,
            lines,
        } = self;

        for contents in lines.lines() {
            let trimmed = contents.as_slice();
            let opt_m = regex.find(trimmed);
            let is_match = opt_m.is_some();
            if is_match.bitxor(invert) {
                let range = if let Some(m) = opt_m {
                    m.start()..m.end()
                } else {
                    0..trimmed.len()
                };
                f(ContentsWithMatchRange { contents, range })?;
            }
        }
        ControlFlow::Continue(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ContentsWithMatchRange {
    pub contents: Contents,
    /// Range within `contents.as_slice()`. Note that the range is
    /// byte based!
    pub range: Range<usize>,
}

#[test]
fn t_size_contents_with_match_range() {
    assert_eq!(size_of::<ContentsWithMatchRange>(), 9 * size_of::<usize>());
}

pub fn split3_line_range(
    line: &[u8],
    range: Range<usize>,
) -> (&BStr, &BStr, &BStr) {
    let start = range.start;
    let end = range.end;
    let veryend = line.len();
    (
        line[0..start].as_bstr(),
        line[start..end].as_bstr(),
        line[end..veryend].as_bstr(),
    )
}

impl ContentsWithMatchRange {
    /// Just the matching area
    pub fn match_as_slice(&self) -> &[u8] {
        let Self { contents, range } = self;
        &contents.as_slice()[range.clone()]
    }

    // todo: collect all these sorts of text-and-line-break
    // manipulation stuff (incl. xmlhub)

    /// The whole line(s) in which the match lies, and the area of
    /// each line that is part of the match. If the `context_*`
    /// arguments are non-zero, that many more lines are included with
    /// None for the range.
    ///
    /// (Panics for context numbers too close to MAX.)
    pub fn lines_around_match(
        &self,
        context_above: usize,
        context_below: usize,
    ) -> Vec<(&[u8], Option<Range<usize>>)> {
        let ContentsWithMatchRange { contents, range } = self;
        // To find line endings, break out of the framing, OK?
        let backing = contents.backing().as_ref();
        let range_in_backing =
            range_add(contents.range_in_backing(), range.clone());
        let line_terminator = contents.line_terminator();
        #[allow(unused)]
        let (contents, range) = ((), ());

        let pre = &backing[0..range_in_backing.start];
        // Need to register the lines within the match range, too,
        // thus don't skip it!
        let onwards = &backing[range_in_backing.start..];
        let mut line_starts: Vec<usize> = pre
            .iter()
            .enumerate()
            .rev()
            .filter(|(_, b)| **b == line_terminator)
            .map(|(i, _)| i + 1)
            .take(context_above + 1)
            .collect();
        if line_starts.len() <= context_above {
            line_starts.push(0);
        }
        line_starts.reverse();
        let num_line_starts_before = line_starts.len();

        let num_line_breaks_within_match = &backing[range_in_backing.clone()]
            .iter()
            .filter(|b| **b == line_terminator)
            .count();

        line_starts.extend(
            onwards
                .iter()
                .enumerate()
                .filter(|(_, b)| **b == line_terminator)
                .map(|(i, _)| range_in_backing.start + i + 1)
                .take(num_line_breaks_within_match + context_below + 1),
        );
        if line_starts.len() - num_line_starts_before <= context_below {
            // + 1 for a "virtual" line terminator to subtract later
            line_starts.push(backing.len() + 1);
        }
        line_starts
            .iter()
            .copied()
            .zip(line_starts.iter().copied().skip(1))
            .map(|(line_start, line_end)| {
                let range_line = line_start..line_end - 1;
                let line = &backing[range_line.clone()];
                let opt_range =
                    range_within(range_in_backing.clone(), range_line);
                (line, opt_range)
            })
            .collect()
    }

    /// Calculate the start position by counting the line terminators
    /// in the contents before the match
    pub fn start_position(&self, line_terminator: u8) -> Position128 {
        self.contents.start_position()
            + Position128::from_run_up(
                &self.contents[0..self.range.start],
                line_terminator,
            )
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
    line_terminator: u8,
) -> Result<ContentsGrep<'regex>, std::io::Error> {
    let contents = file_contents(path, line_terminator)?;
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Contents {
    backing: Arc<[u8]>,
    range: Range<usize>,
    line_terminator: u8,
    /// The line/column position of the start of `range` in
    /// `contents`, if already known (otherwise lines will be counted
    /// in the `start_position` method)
    range_start_position: Option<Position128>,
}

#[test]
fn t_size_contents() {
    // bummer, line_terminator blows it up by a word
    assert_eq!(size_of::<Contents>(), 7 * size_of::<usize>());
}

// No need, Deref works!
// impl Index<Range<usize>> for Contents {
// }

impl Deref for Contents {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl Contents {
    pub fn new_full(backing: Arc<[u8]>, line_terminator: u8) -> Self {
        let range = 0..backing.len();
        Self {
            backing,
            range,
            line_terminator,
            range_start_position: Some(Position128::TOP_LEFT),
        }
    }

    pub fn backing(&self) -> &Arc<[u8]> {
        &self.backing
    }

    pub fn range_in_backing(&self) -> Range<usize> {
        self.range.clone()
    }

    pub fn line_terminator(&self) -> u8 {
        self.line_terminator
    }

    pub fn as_slice(&self) -> &[u8] {
        let Self {
            backing,
            range,
            line_terminator: _,
            range_start_position: _,
        } = self;
        &backing[range.clone()]
    }

    /// Returns the lines without the line endings, referencing via
    /// Arc clone into self's content.
    pub fn lines(&self) -> impl Iterator<Item = Contents> + '_ {
        let Self {
            backing,
            range,
            line_terminator,
            range_start_position: _,
        } = self;
        let line_terminator = *line_terminator;
        let mut range_start_position = self.start_position();
        let mut range_start = range.start;
        self.as_slice()
            .split(move |b| *b == line_terminator)
            .map(move |line| {
                let this = Self {
                    backing: backing.clone(),
                    range: range_start..(range_start + line.len()),
                    line_terminator,
                    range_start_position: Some(range_start_position.clone()),
                };
                range_start =
                    range_start.saturating_add(line.len().saturating_add(1));
                range_start_position.inc_line();
                this
            })
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

    pub fn start_position(&self) -> Position128 {
        let Self {
            backing,
            range,
            range_start_position,
            line_terminator,
        } = self;
        range_start_position.unwrap_or_else(|| {
            Position128::from_run_up(&backing[0..range.start], *line_terminator)
        })
    }
}

pub fn file_contents(
    path: &Path,
    line_terminator: u8,
) -> Result<Contents, std::io::Error> {
    Ok(Contents::new_full(
        std::fs::read(path)?.into(),
        line_terminator,
    ))
}

#[cfg(test)]
mod tests {
    use bstr::{ByteSlice, B};

    use super::*;

    #[test]
    fn t_lines_around_match() {
        let full_contents = Contents::new_full(
            b"Hello\nThere.\nLine 3.\nLine 4".to_owned().into(),
            b'\n',
        );

        {
            // Line based matching: match in a line sub-Contents

            let lines: Vec<_> = full_contents.lines().collect();
            assert_eq!(lines.len(), 4);

            let line3 = lines[2].clone();
            assert_eq!(line3.as_slice(), b"Line 3.");

            let m = ContentsWithMatchRange {
                contents: line3,
                range: 2..4,
            };
            assert_eq!(m.match_as_slice(), b"ne");

            let l = m.lines_around_match(0, 0);
            assert_eq!(l.len(), 1);
            assert_eq!(l[0].0.as_bstr(), B("Line 3.").as_bstr());
            assert_eq!(l[0].1, Some(2..4));

            let l = m.lines_around_match(1, 55);
            assert_eq!(l.len(), 3);
            assert_eq!(l[0].0, b"There.");
            assert_eq!(l[0].1, None);
            assert_eq!(l[1].0, b"Line 3.");
            assert_eq!(l[1].1, Some(2..4));
            assert_eq!(l[2].0, b"Line 4");
            assert_eq!(l[2].1, None);
        }

        {
            // File matching with a multi-line match
            let m = ContentsWithMatchRange {
                contents: full_contents,
                range: 7..16,
            };
            assert_eq!(m.match_as_slice().as_bstr(), B("here.\nLin").as_bstr());

            let l = m.lines_around_match(0, 0);
            // assert_eq!(l.len(), 2);
            assert_eq!(l[0].0.as_bstr(), B("There.").as_bstr());
            assert_eq!(l[0].1, Some(1..6));
            assert_eq!(l[1].0.as_bstr(), B("Line 3.").as_bstr());
            assert_eq!(l[1].1, Some(0..3));
        }
    }
}
