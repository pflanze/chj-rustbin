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
    let lines: Contents = file_contents(path, line_terminator)?;
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

/// Returns the range of the function intro match within `pre`, and
/// the position of the first newline after it, if any, if any.
/// `is_function` is given a slice into `pre` from the start of a
/// suspected function definition, if it returns false, the search is
/// continued.
pub fn find_function_intro(
    pre: &BStr,
    is_function: impl Fn(&BStr) -> bool,
) -> Option<(Range<usize>, Option<usize>)> {
    let mut last_open_curly = None;
    let mut last_newline = None;
    let mut newline_after_curly = None;
    // Excluding newlines
    let mut last_char_was_whitespace = true;

    macro_rules! end_condition {
        { $i:expr } => {
            (!last_char_was_whitespace) && is_function(pre[$i+1..].as_bstr())
        }
    }

    for i in (0..pre.len()).rev() {
        let b = pre[i];
        match b {
            b'{' => {
                last_open_curly = Some(i);
                newline_after_curly = last_newline;
            }
            b'\n' => {
                if end_condition!(i) {
                    return Some((
                        i + 1
                            ..last_open_curly
                                .map(|pos| pos + 1)
                                .unwrap_or(pre.len()),
                        newline_after_curly,
                    ));
                }
                last_newline = Some(i);
            }
            b' ' | b'\t' => last_char_was_whitespace = true,
            _ => last_char_was_whitespace = false,
        }
    }
    if end_condition!(0) {
        return Some((
            0..last_open_curly.map(|pos| pos + 1).unwrap_or(pre.len()),
            newline_after_curly,
        ));
    }
    None
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ContextLineKind {
    FunctionIntro,
    Gap,
    Context,
    Match(Range<usize>),
}

impl ContentsWithMatchRange {
    pub fn from_contents(contents: Contents) -> Self {
        let match_range = contents.range_in_backing();
        Self {
            contents,
            match_range,
        }
    }

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
    /// None for the range. If `is_function` is given (which is give a
    /// slice into the text from the start of the line suspected to be
    /// a function definition, and return true if it's OK), the
    /// nearest part of the file that starts with non-whitespace on a
    /// new line and extends to the nearest '{' is also included, with
    /// a ".."  line added if there is a gap.
    ///
    /// (Panics for context numbers too close to `usize::MAX`!)
    pub fn lines_around_match<F: Fn(&BStr) -> bool>(
        &self,
        is_function: Option<F>,
        context_above: usize,
        context_below: usize,
    ) -> Vec<(&BStr, ContextLineKind)> {
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

        // Need to register the lines within the match range, too,
        // thus don't skip it!
        let onwards = &backing[range_in_backing.start..];

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

        let function_intro_range = if let Some(is_function) = is_function {
            find_function_intro(pre.as_bstr(), is_function)
        } else {
            None
        };
        let mut lines: Vec<(&BStr, ContextLineKind)> = function_intro_range
            .map(|(function_intro_range, opt_newline_pos)| {
                if let Some(newline_pos) = opt_newline_pos {
                    let already_shown_start = line_starts[0];
                    if newline_pos + 1 >= already_shown_start {
                        if function_intro_range.start < already_shown_start {
                            // intro is a gap-less extension of already-shown part
                            pre[function_intro_range.start
                                ..already_shown_start.saturating_sub(1)]
                                .as_bstr()
                                .lines()
                                .map(|line| {
                                    (
                                        line.as_bstr(),
                                        ContextLineKind::FunctionIntro,
                                    )
                                })
                                .collect()
                        } else {
                            // intro is within already-shown part
                            vec![]
                        }
                    } else {
                        // Have a gap
                        let mut lines: Vec<(&BStr, ContextLineKind)> = pre
                            [function_intro_range.start..newline_pos]
                            .as_bstr()
                            .lines()
                            .map(|line| {
                                (line.as_bstr(), ContextLineKind::FunctionIntro)
                            })
                            .collect();
                        lines.push(("..".as_ref(), ContextLineKind::Gap));
                        lines
                    }
                } else {
                    vec![]
                }
            })
            .unwrap_or_else(Vec::new);

        lines.extend(
            // Convert line starts to line slices
            line_starts
                .iter()
                .copied()
                .zip(line_starts.iter().copied().skip(1))
                .map(|(line_start, line_end)| {
                    let range_line = line_start..line_end - 1;
                    let line = &backing[range_line.clone()];
                    let kind = match range_within(
                        range_in_backing.clone(),
                        range_line,
                    ) {
                        Some(range) => ContextLineKind::Match(range),
                        None => ContextLineKind::Context,
                    };
                    (line.as_bstr(), kind)
                }),
        );

        lines
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
    pub fn lines_grep<'regex, 's>(
        self,
        regex: &'regex Regex,
        invert: bool,
    ) -> LinesGrepContents<'regex> {
        LinesGrepContents {
            regex,
            invert,
            lines: self,
        }
    }

    pub fn file_grep<'regex, 's>(
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

    use crate::text::bstr_parseutil::starts_with_word;

    use super::*;

    #[test]
    fn t_starts_with_word() {
        let sww =
            |s: &str, word: &str| starts_with_word(s.as_ref(), word.as_ref());
        assert!(sww("", ""));
        assert!(sww("foo ", "foo"));
        assert!(!sww("foob ", "foo"));
        assert!(!sww("foo ", "foob"));
        assert!(!sww(" foo ", "foo"));
        assert!(!sww("xfoo ", "foo"));
    }

    #[test]
    fn t_find_function_intro() {
        let t = |s: &str| {
            find_function_intro(s.as_ref(), |s| {
                !starts_with_word(s, "where".as_ref())
            })
        };
        assert_eq!(t(""), None); // OK?
        assert_eq!(t(" foo"), None);
        assert_eq!(t("foo"), Some((0..3, None)));
        assert_eq!(t("\nfoo"), Some((1..4, None)));
        assert_eq!(t("\nfoo {\n  hello\n  world"), Some((1..6, Some(6))));
        assert_eq!(t("\nfoo \n  hello { \n  world"), Some((1..15, Some(16))));
        assert_eq!(t("\nfoo \n  hello \n { world"), Some((1..17, None)));
        assert_eq!(t("\nwhere \n  hello \n { world"), None);
        assert_eq!(
            t("\nwhereabouts \n  hello \n { world"),
            Some((1..25, None))
        );
        assert_eq!(
            t("\nimpl \nwhere| \n  hello \n { world"),
            Some((1..26, None))
        );
    }

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

            let l = m.lines_around_match::<fn(&BStr) -> bool>(None, 0, 0);
            assert_eq!(l.len(), 1);
            assert_eq!(l[0].0.as_bstr(), B("Line 3.").as_bstr());
            assert_eq!(l[0].1, ContextLineKind::Match(2..4));

            let l = m.lines_around_match::<fn(&BStr) -> bool>(None, 1, 55);
            assert_eq!(l.len(), 3);
            assert_eq!(l[0].0, B("There."));
            assert_eq!(l[0].1, ContextLineKind::Context);
            assert_eq!(l[1].0, B("Line 3."));
            assert_eq!(l[1].1, ContextLineKind::Match(2..4));
            assert_eq!(l[2].0, B("Line 4"));
            assert_eq!(l[2].1, ContextLineKind::Context);
        }

        {
            // File matching with a multi-line match
            let m = ContentsWithMatchRange {
                contents: full_contents,
                match_range: 7..16,
            };
            assert_eq!(m.match_as_slice().as_bstr(), B("here.\nLin").as_bstr());

            let l = m.lines_around_match::<fn(&BStr) -> bool>(None, 0, 0);
            // assert_eq!(l.len(), 2);
            assert_eq!(l[0].0.as_bstr(), B("There.").as_bstr());
            assert_eq!(l[0].1, ContextLineKind::Match(1..6));
            assert_eq!(l[1].0.as_bstr(), B("Line 3.").as_bstr());
            assert_eq!(l[1].1, ContextLineKind::Match(0..3));
        }
    }
}
