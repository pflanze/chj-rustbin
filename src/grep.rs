use std::{
    fs::File,
    io::BufReader,
    num::NonZeroU64,
    ops::{BitXor, ControlFlow, Range},
    path::Path,
    sync::Arc,
};

use bstr::{BStr, ByteSlice};
use internal_iterator::InternalIterator;
use regex::bytes::Regex;

use crate::{
    by_tmp_ref_iterator::ByTmpRefIterator,
    contents::Contents,
    lines_by_tmp_ref::{
        trim_line_terminator, ContentLinesByTmpRef, ReadLinesByTmpRef,
    },
    position::{Position128, Position64},
    range::{range_add, range_within},
};

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
                let match_range = if let Some(m) = opt_m {
                    m.start()..m.end()
                } else {
                    0..trimmed.len()
                };
                f(ContentsWithMatchRange {
                    contents,
                    match_range,
                })?;
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
    pub match_range: Range<usize>,
}

#[test]
fn t_size_contents_with_match_range() {
    assert_eq!(
        size_of::<ContentsWithMatchRange>(),
        (3 + 2) * size_of::<usize>()
    );
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
        let Self {
            contents,
            match_range,
        } = self;
        &contents.as_slice()[match_range.clone()]
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
        let ContentsWithMatchRange {
            contents,
            match_range,
        } = self;
        // To find line endings, break out of the framing, OK?
        let backing = contents.backing();
        let range_in_backing =
            range_add(contents.range_in_backing(), match_range.clone());
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
    pub fn start_position(&self) -> Position64 {
        let Self {
            contents,
            match_range,
        } = self;
        contents.position_at(match_range.start)
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

impl Contents {
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
            let match_range = m.start()..m.end();
            f(ContentsWithMatchRange {
                contents: contents.clone(),
                match_range,
            })?;
        }
        ControlFlow::Continue(())
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
                match_range: 2..4,
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
                match_range: 7..16,
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
