use std::{path::PathBuf, convert::TryInto, time::Instant, collections::{HashSet, HashMap}, sync::Arc};

use anyhow::{Result, Context, anyhow, bail};
use clap::Parser;


#[derive(clap::Parser, Debug)]
/// Parse a folder with todo files with "OPEN.." markers.
struct Opts {
    // #[clap(long)]

    /// The base directory holding the todo files.
    directory: Option<PathBuf>
}

struct Priority {
    
}

struct OpenInfo {
    path: Arc<PathBuf>,
    mtime: Instant,
    priority: Priority,
    dependencies: Vec<Arc<PathBuf>>,
}

fn parse_inside(s: &str) -> HashMap<&str, &str> {

    fn parse_pair(s: &str) -> (&str, &str) {
        todo!()
    }
    
    // inside.split(',').map(parse_pair)
    
    todo!()
}


/// Look for the first occurrence of `needle` in `haystack`, return
/// the rest after it.
fn str_matches<'t>(haystack: &'t str, needle: &str) -> Option<&'t str> {
    let pos = haystack.find(needle)?;
    Some(&haystack[pos + needle.len()..])
}

fn str_starts_with<'t>(haystack: &'t str, beginning: &str) -> Option<&'t str> {
    if haystack.starts_with(beginning) {
        Some(&haystack[beginning.len()..])
    } else {
        None
    }
}

fn str_take_until<'t>(haystack: &'t str, needle: &str) -> Option<&'t str> {
    println!("str_take_until({haystack:?}, {needle:?})");
    let pos = haystack.find(needle)?;
    Some(&haystack[0..pos])
}

fn str_take_while(haystack: &str, mut pred: impl FnMut(char) -> bool) -> &str {
    let mut last_pos = 0;
    for (cur_pos, c) in haystack.char_indices() {
        last_pos = cur_pos;
        if !pred(c) {
            return &haystack[0..last_pos];
        }
    }
    haystack
}

#[cfg(test)]
#[test]
fn t_str_take_while() {
    assert_eq!(str_take_while(" abc def", |c| c.is_alphanumeric()), "");
    assert_eq!(str_take_while("abc def", |c| c.is_alphanumeric()), "abc");
    assert_eq!(str_take_while("abcdef", |c| c.is_alphanumeric()), "abcdef");
}




fn parse_path(path: PathBuf) -> Result<Option<OpenInfo>> {
    println!("parse_path({path:?})");
    let file_name_os = path.file_name().expect(
        "filename always present in path since we iterate over the dir and filter files");
    let file_name = file_name_os.to_str().ok_or_else(
        || anyhow!("the file name in path {path:?} is not valid UTF-8"))?;
    
    if let Some(rest) = str_matches(file_name, "OPEN") {
        if let Some(rest) = str_starts_with(rest, "{") {
            println!("OPEN with {{, rest={rest:?}");
            if let Some(inside) = str_take_until(rest, "}") {
                println!("inside = {inside:?}");

                    
            } else {
                bail!("missing closing '}}' after 'OPEN{{'")
            }
        } else {
            dbg!("have not");
        }
        dbg!((file_name, rest));
        Ok(None)
    } else {
        println!("no match for {file_name:?}");
        Ok(None)
    }
}


fn main() -> Result<()> {
    let opts: Opts = Opts::from_args();

    let directory = opts.directory.unwrap_or_else(|| ".".try_into().unwrap());
    // let directory = opts.directory.unwrap_or_default();

    for item in std::fs::read_dir(&directory)? {
        let item = item?;
        let ft = item.file_type()?;
        if ft.is_file() {
            let path = item.path();
            parse_path(path)?;
        }
        // XX: if it's a symlink, check if it has different OPEN info?

    }
    
    
    Ok(())
}
