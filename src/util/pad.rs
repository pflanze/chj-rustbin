//! Utils for producing column-formatted terminal output

pub fn pad_string_leftadjust(s: &str, len: usize, out: &mut String) {
    out.push_str(s);
    let slen = s.len();
    if slen < len {
        for _ in 0..(len - slen) {
            out.push(' ');
        }
    }
}

pub fn pad_string_rightadjust(string: &str, len: usize, out: &mut String) {
    let slen = string.len();
    if slen < len {
        for _ in 0..(len - slen) {
            out.push(' ');
        }
    }
    out.push_str(string);
}

/// Adjust on the first `dot` character in `string`; if there is no
/// dot, adjust the right end on the dot position.
pub fn pad_string_dotadjust(
    string: &str,
    left_of_dot: usize,
    right_of_dot: Option<usize>,
    dot: char,
    out: &mut String,
) {
    let mut dotpositions =
        string
            .char_indices()
            .enumerate()
            .filter_map(
                |(chari, (bytei, c))| {
                    if c == dot {
                        Some((chari, bytei))
                    } else {
                        None
                    }
                },
            );
    if let Some((_chari, bytei)) = dotpositions.next() {
        let left = &string[0..bytei];
        pad_string_rightadjust(left, left_of_dot, out);
        out.push(dot);
        let right = &string[bytei + 1..];
        if let Some(right_of_dot) = right_of_dot {
            pad_string_leftadjust(right, right_of_dot, out);
        } else {
            out.push_str(right);
        }
    } else {
        pad_string_rightadjust(string, left_of_dot, out);
        if let Some(right_of_dot) = right_of_dot {
            for _ in 0..(1 + right_of_dot) {
                out.push(' ');
            }
        }
    }
}

/// Create a string from lines like "foo: 123", "long_key: 0" with the
/// lines prefixed with spaces so that all the first colons in the
/// lines line up.  Returns None if not at least one of the lines
/// contains a colon.
pub fn line_up_on_colon<'t>(
    prefix: &str,
    unadjusted_lines: impl Iterator<Item = &'t str> + Clone,
) -> Option<String> {
    let max_key_len = unadjusted_lines
        .clone()
        .filter_map(|line| {
            let (ci, _c) = line
                .chars()
                .enumerate()
                .filter(|(_ci, c)| *c == ':')
                .next()?;
            Some(ci)
        })
        .max()?;

    let mut outstr = String::new();
    for line in unadjusted_lines {
        outstr.push_str(prefix);
        pad_string_dotadjust(line, max_key_len, None, ':', &mut outstr);
        outstr.push('\n');
    }
    Some(outstr)
}
