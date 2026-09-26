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
