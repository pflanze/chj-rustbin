use anyhow::{anyhow, bail, Result};

pub fn is_all(s: &str, pred: impl Fn(char) -> bool) -> bool {
    s.chars().all(pred)
}

pub fn is_all_white(s: &str) -> bool {
    is_all(s, |c| c.is_ascii_whitespace())
}

// pub fn is_all_alphanum(s: &str) -> bool {
//     is_all(s, |c| c.is_alphanumeric())
// }

pub fn key_val(s: &str) -> Option<(&str, &str)> {
    let i = s.find(':')?;
    Some((&s[0..i], &s[i + 1..]))
}

pub fn first_rest(s: &str) -> Option<(char, &str)> {
    let c = s.chars().next()?;
    Some((c, &s[c.len_utf8()..]))
}

pub fn drop_white(s: &str) -> &str {
    // s.chars().skip_while(|c| c.is_ascii_whitespace())  but now ?  lol
    let mut p = s;
    while let Some((c, r)) = first_rest(p) {
        if !c.is_ascii_whitespace() {
            return p;
        }
        p = r;
    }
    p
}

/// drop whitespace from the end
pub fn drop_white_end(s: &str) -> &str {
    for (i, b) in s.bytes().rev().enumerate() {
        if !(b as char).is_ascii_whitespace() {
            return &s[0..s.len() - i];
        }
    }
    return "";
}

pub fn after_white(s: &str) -> Option<&str> {
    let s2 = drop_white(s);
    if std::ptr::eq(s, s2) {
        None
    } else {
        Some(s2)
    }
}

pub fn cleanwhite(s: &str) -> &str {
    // I forgot the std function, there was one, right? Just:
    drop_white(drop_white_end(s))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_drop_white() {
        fn t(inp: &str, outp: &str) {
            assert_eq!(drop_white(inp), outp)
        }
        t("foo", "foo");
        t("  foo", "foo");
        t("foo  ", "foo  ");
        t(" foo  ", "foo  ");
        t(" f oo  ", "f oo  ");
        t("  ", "");
    }

    #[test]
    fn t_drop_white_end() {
        fn t(inp: &str, outp: &str) {
            assert_eq!(drop_white_end(inp), outp)
        }
        t("foo", "foo");
        t("  foo", "  foo");
        t("foo  ", "foo");
        t(" foo  ", " foo");
        t(" f oo  ", " f oo");
        t("  ", "");
    }

    #[test]
    fn t_cleanwhite() {
        fn t(inp: &str, outp: &str) {
            assert_eq!(cleanwhite(inp), outp)
        }
        t("foo", "foo");
        t("  foo", "foo");
        t("foo  ", "foo");
        t(" foo  ", "foo");
        t(" f oo  ", "f oo");
        t("  ", "");
    }
}

pub fn take_while(s: &str, pred: impl Fn(char) -> bool) -> (&str, &str) {
    let mut it = s.chars().enumerate();
    while let Some((i, c)) = it.next() {
        if !pred(c) {
            return (&s[0..i], &s[i..]);
        }
    }
    (s, "")
}

pub fn parse_hex_digit(c: char) -> Result<u32> {
    let n = c as u32;
    if (n >= '0' as u32) && (n <= '9' as u32) {
        Ok(n - ('0' as u32))
    } else if (n >= 'a' as u32) && (n <= 'f' as u32) {
        Ok(n - ('a' as u32) + 10)
    } else if (n >= 'A' as u32) && (n <= 'F' as u32) {
        Ok(n - ('A' as u32) + 10)
    } else {
        bail!("invalid hex digit {c:?}")
    }
}

pub fn next_hex_digit<I>(cs: &mut I) -> Result<u32>
where
    I: Iterator<Item = char>,
{
    parse_hex_digit(cs.next().ok_or_else(|| anyhow!("hex string too short"))?)
}

pub fn parse_hex<const N: usize>(s: &str) -> Result<[u8; N]> {
    let mut r = [0; N];
    let mut cs = s.chars();
    for i in 0..N {
        let a = next_hex_digit(&mut cs)?;
        let b = next_hex_digit(&mut cs)?;
        r[i] = (a * 16 + b) as u8;
    }
    Ok(r)
}

pub fn char_is_white(c: char) -> bool {
    c.is_ascii_whitespace()
}

pub fn drop_n(s: &str, n: usize, f: impl Fn(char) -> bool) -> Result<&str> {
    let mut p = s;
    let mut i = 0;
    while i < n {
        if let Some((c, r)) = first_rest(p) {
            if !f(c) {
                bail!(
                    "drop_n: non-matching character after {i} \
                       instead of {n} characters"
                )
            }
            p = r;
        } else {
            bail!("drop_n: end of string after {i} instead of {n} characters")
        }
        i += 1;
    }
    Ok(&s[n..])
}

pub fn parse_byte_multiplier(s: &str) -> Result<u64> {
    if s == "B" {
        Ok(1)
    } else if s == "KiB" {
        Ok(1024)
    } else if s == "MiB" {
        Ok(1024 * 1024)
    } else if s == "GiB" {
        Ok(1024 * 1024 * 1024)
    } else if s == "TiB" {
        Ok(1024 * 1024 * 1024 * 1024)
    } else if s == "PiB" {
        Ok(1024 * 1024 * 1024 * 1024 * 1024)
    } else {
        bail!("unknown multiplier {s:?}")
    }
}
