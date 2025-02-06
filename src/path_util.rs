
/// Get the file extension from a path given as bytes (since there is
/// no path from CStr to PathBuf, right?). If the extension can't be
/// decoded as UTF-8, returns None. Invalid encoding in other parts of
/// the path does not matter, though (as long as the `path_separator`
/// and `b'.'` are still encoded as such).
pub fn extension_from_ascii_or_utf8_bytes(path: &[u8], path_separator: u8) -> Option<&str> {
    let dot_pos = path.iter().rposition(|b| *b == b'.')?;
    if let Some(separator_pos) = path.iter().rposition(|b| *b == path_separator) {
        if !(separator_pos + 1 < dot_pos) {
            return None
        }
    } else {
        if dot_pos < 1 {
            return None
        }
    }
    let slice = &path[dot_pos + 1..];
    if slice.is_empty() {
        None
    } else {
        std::str::from_utf8(slice).ok()
    }
}

#[test]
fn t_extension_from_ascii_or_utf8_bytes() {
    fn t(s: &str) -> Option<&str> { extension_from_ascii_or_utf8_bytes(s.as_bytes(), b'/') }
    assert_eq!(t("foo.bar"), Some("bar"));
    assert_eq!(t("foobar"), None);
    assert_eq!(t("foo/bar.xz"), Some("xz"));
    assert_eq!(t("foo.bar/xz"), None);
    assert_eq!(t("foo.bar\\xz"), Some("bar\\xz")); // Well..
    assert_eq!(t("foo/bar."), None);
    assert_eq!(t(".bah"), None);
    assert_eq!(t("a.b"), Some("b"));
    assert_eq!(t("ah/.b"), None);
    assert_eq!(t("ah/.b.a"), Some("a"));
    assert_eq!(t("ah/a.b"), Some("b"));
    let mut bs = [0x61, 0x2f, 0x61, 0x2e, 0x62];
    let e = extension_from_ascii_or_utf8_bytes;
    assert_eq!(e(&bs, b'/'), Some("b"));
    bs[1] = 0x90;
    assert_eq!(e(&bs, b'/'), Some("b"));
    bs[1] = 0xf0;
    assert_eq!(e(&bs, b'/'), Some("b"));
    bs[4] = 0x42;
    assert_eq!(e(&bs, b'/'), Some("B"));
    bs[4] = 0xf0;
    assert_eq!(e(&bs, b'/'), None);
}

