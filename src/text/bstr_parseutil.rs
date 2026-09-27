use bstr::BStr;

pub fn is_ascii_word(c: u8) -> bool {
    c.is_ascii_alphanumeric()
        || match c {
            b'_' => true,
            _ => false,
        }
}

pub fn starts_with_word(s: &BStr, word: &BStr) -> bool {
    s.starts_with(word) && {
        s[word.len()..]
            .first()
            .map(|c| !is_ascii_word(*c))
            .unwrap_or(true)
    }
}

/// Works with a `from_position` to avoid having to juggle with
/// position additions. Returns the position in `contents`, not the
/// offset from `from_position`.
pub fn find_byte_position(
    contents: &[u8],
    from_position: usize,
    b: u8,
) -> Option<usize> {
    if from_position > contents.len() {
        return None;
    }
    for i in from_position..contents.len() {
        if contents[i] == b {
            return Some(i);
        }
    }
    None
}
