use std::ops::Range;

pub fn range_add(outer: Range<usize>, inner: Range<usize>) -> Range<usize> {
    let start = outer.start + inner.start;
    let end = outer.start + inner.end;
    // Ignore outer.end, just assume that inner fits within outer?
    start..end
}

/// Calculate a range within a window denoted by `outer`, but given as
/// a `inner` window in the same backing as `outer` is. (I.e. the
/// result is using small numbers.)
pub fn range_within(
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
