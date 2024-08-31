#[path = "../moreordering.rs"]
mod moreordering;
#[allow(unused_imports)] // polyfill only used with older compiler
use moreordering::MoreOrdering;

#[path = "../readwithcontext.rs"]
mod readwithcontext;
use readwithcontext::{easy_read_line, open_file, ReadWithContext};

use anyhow::{Result, bail, Context, Error, anyhow};
use clap::Parser;
use thiserror::Error;
use std::cmp::Ordering;
use std::fs::File;
use std::i64::MIN;
use std::io::{BufReader, stdout, Write, BufWriter};
use std::os::unix::prelude::{MetadataExt, FromRawFd};
use std::path::PathBuf;
use std::collections::{VecDeque, HashSet};
use kstring::KString;

#[derive(clap::Parser, Debug)]
/// Print the lines that occur in all input files. By default, files
/// don't need to be sorted (but see `--sorted`); an in-memory set is
/// built, the order of the output lines follows the last file, and if
/// there are repetitions in the last file, those are repeated, too.

#[clap(name = "intersection from chj-rustbin")]
struct Opt {
    /// Show the set, not the filtered last file (i.e. there will be
    /// no repetitions in the output, and it is lexically sorted
    /// instead of like the last file). This also allows to include
    /// the file given last in the re-ordering optimization (the
    /// smallest files being processed first).
    #[clap(long)]
    set: bool,

    /// Assume that the input files are lexically sorted (uses a
    /// streaming implementation).
    #[clap(long)]
    sorted: bool,

    /// Implies --sorted. Assume numeric sorting instead of
    /// lexical. Currently integers only.
    #[clap(long)]
    numeric: bool,

    /// Whether to print dropped lines to file descriptor from 10
    /// onwards (the dropped lines from the first file go to file
    /// descriptor 10, the second file to fd 11, etc.). Only works in
    /// sorted mode (--sorted or --numeric).
    #[clap(long)]
    fddrop: bool,

    #[clap(long)]
    structsizes: bool,

    /// The paths to files to get the intersection of.
    #[clap(parse(from_os_str))]
    file_paths: Vec<PathBuf>,
}

fn println(out: &mut impl Write, line: &String) -> Result<()> {
    out.write_all(line.as_bytes())?;
    out.write_all(b"\n")?;
    Ok(())
}

#[derive(Debug, Clone, Copy)]
enum SortOrder {
    Lexical,
    Numeric,
}

impl SortOrder {
    fn perhaps_parse_number(self, line: &str) -> Result<i64> {
        match self {
            SortOrder::Lexical => Ok(MIN),
            SortOrder::Numeric => line.parse().with_context(
                || anyhow!("not an i64 number: {:?}", line))
        }
    }
    fn compare(self, l1: &Line, l2: &Line) -> Ordering {
        let res = match self {
            SortOrder::Lexical => l1.string.cmp(&l2.string),
            SortOrder::Numeric => l1.i64.cmp(&l2.i64),
        };
        // eprintln!("compare({:?}, {:?}) = {:?}", l1.string, l2.string, res);
        res
    }
}

#[derive(Debug)]
struct Line {
    string: String,
    i64: i64, // only if opt.numeric; MIN by default in either case
}

impl Line {
    fn new() -> Line {
        Line {
            string: String::new(),
            i64: MIN,
        }
    }
    fn read_and_parse_line(&mut self, inp: &mut BufReader<File>, sortorder: SortOrder)
                           -> Result<bool> {
        let line = &mut self.string;
        let have_line = easy_read_line(inp, line)?;
        if have_line {
            self.i64 = sortorder.perhaps_parse_number(line)?;
        }
        Ok(have_line)
    }
}

#[derive(Debug)]
struct Input {
    path: PathBuf,
    input: BufReader<File>,
    /// Filehandle to write non-intersecting entries to if --fddrop is given.
    output: Option<BufWriter<File>>,
    /// Using two line buffers so as to read a line in advance without
    /// losing the previous one -- for?
    line1: Line,
    line2: Line,
    current_line_is_1: bool,
    current_line_is_in_set: bool,
    linenum: u64,
}

impl Input {
    fn current_line(&self) -> &Line {
        if self.current_line_is_1 { &self.line1 } else { &self.line2 }
    }
    fn is_ordered(&self, sortorder: SortOrder)
                  -> Result<bool> {
        // eprintln!("is_ordered({:?})...", self.path);
        Ok(
            if self.current_line_is_1 {
                sortorder.compare(&self.line1, &self.line2)
            } else {
                sortorder.compare(&self.line2, &self.line1)
            }.is_ge())
    }
    // returns false on EOF
    fn next(&mut self, sortorder: SortOrder) -> Result<bool> {
        (|| {
            // eprintln!("next({:?})...", self.path);
            // Write line to output if fddrop:
            if let Some(output) = &mut self.output {
                if !self.current_line_is_in_set {
                    // copy-paste of current_line algo, once more
                    // (make a macro?):
                    let current_line =
                        if self.current_line_is_1 {
                            &mut self.line1
                        } else {
                            &mut self.line2
                        };
                    // eprintln!("next({:?}): drop {:?}", self.path,
                    //           &current_line.string);
                    println(output, &current_line.string)?;
                }
                self.current_line_is_in_set = false;
            }
            // Switch around line buffers and get the next line
            let current_line_is_1 = !self.current_line_is_1;
            self.current_line_is_1 = current_line_is_1;
            let current_line =
                if current_line_is_1 {
                    &mut self.line1
                } else {
                    &mut self.line2
                };
            self.linenum += 1;
            if current_line.read_and_parse_line(&mut self.input, sortorder)? {
                // eprintln!("next({:?}): new: {:?}", self.path, &current_line.string);
                if ! self.is_ordered(sortorder)? {
                    bail!("file is not ordered")
                }
                Ok(true)
            } else {
                // Mis-use this flag as iterator exhaustion marker, to
                // prevent subsequent calls from println'ing the empty
                // line:
                self.current_line_is_in_set = true;
                Ok(false)
            }
        })().with_context(
            || anyhow!("file {:?} line {}", self.path, self.linenum))
    }
}

#[derive(Debug)]
struct Inputs {
    inputs: Vec<Input>,
}

impl Inputs {
    fn len(&self) -> usize {
        self.inputs.len()
    }

    /// The index into Inputs that has the line furthest along in
    /// sorted processing (if multiple, the one with the largest index
    /// (since `max_by` returns the last one per its docs)).
    fn largest_input_index(
        &self,
        sortorder: SortOrder
    ) -> usize {
        let res = self.inputs.iter().enumerate()
            .max_by(|a, b| sortorder.compare(a.1.current_line(),
                                             b.1.current_line()))
            .unwrap().0;
        // eprintln!("largest_input_index() = {} {:?}", res,
        //           self.inputs[res].current_line().string);
        res
    }

    fn input(&self, index: usize) -> &Input {
        &self.inputs[index]
    }

    fn input_mut(&mut self, index: usize) -> &mut Input {
        &mut self.inputs[index]
    }
}

#[derive(Error, Debug)]
enum Signal {
    #[error("signal: finished")]
    Finished,
    #[error("signal: error {0:?}")]
    Error(Error)
}

enum Mode {
    SetThenLinear,
    Set,
    Sorted(SortOrder),
    StructSizes,
}

impl Mode {
    fn name(&self) -> &'static str {
        match self {
            Mode::SetThenLinear => "default",
            Mode::Set => "--set",
            // Mode::Sorted(SortOrder::Lexical) => "--sorted",
            // Mode::Sorted(SortOrder::Numeric) => "--numeric",
            Mode::Sorted(_) => "--sorted / --numeric",
            Mode::StructSizes => "--structsizes",
        }
    }
    fn min_paths_len(&self) -> usize {
        match self {
            Mode::SetThenLinear => 2,
            Mode::Set => 1,
            Mode::Sorted(_) => 2,
            Mode::StructSizes => 0,
        }
    }
}

fn p_print(nam: &str, siz: usize) {
    println!("{}\t{}", siz, nam)
}
macro_rules! p {
    ( $t:ty ) => {
        let typename =
            if false {
                std::any::type_name::<$t>()
            } else {
                stringify!($t)
            };
        p_print(typename, std::mem::size_of::<$t>())
    }
}
fn print_sizes() {
    p!{Opt};
    p!{Line};
    p!{Input};
    p!{Inputs};
    p!{SortOrder};
    p!{Signal};
    p!{Mode};
}

fn output_fd_for_input_index(i: usize) -> i32 {
    10 + i as i32
}

fn main() -> Result<()> {
    let (mode, mut paths, fddrop) = {
        let opt : Opt = Opt::from_args();
        let paths: VecDeque<PathBuf> = opt.file_paths.into();

        let mode =
            if opt.numeric {
                Mode::Sorted(SortOrder::Numeric)
            } else if opt.sorted {
                Mode::Sorted(SortOrder::Lexical)
            } else if opt.set {
                Mode::Set
            } else if opt.structsizes {
                Mode::StructSizes
            } else {
                Mode::SetThenLinear
            };

        match mode {
            Mode::Sorted(_) => 
                if opt.set {
                    bail!("only one of --set or --sorted (or --numeric) is valid");
                }
            _ => ()
        }

        (mode, paths, opt.fddrop)
    };

    if paths.len() < mode.min_paths_len() {
        bail!("need at least {} input file(s) in {} mode", mode.min_paths_len(),
              mode.name());
    }

    match mode {
        Mode::Sorted(sortorder) => {
            match
                paths.into_iter().enumerate().map(|(i, path)| {
                    let mut input = open_file(&path).map_err(Signal::Error)?;
                    let mut line = Line::new();
                    if line.read_and_parse_line(&mut input, sortorder)
                        .with_context(|| anyhow!("file {:?} line 1", path))
                        .map_err(Signal::Error)?
                    {
                        let output =
                            if fddrop {
                                Some(
                                    BufWriter::new(unsafe {
                                        File::from_raw_fd(output_fd_for_input_index(i))
                                    }))
                            } else {
                                None
                            };
                        Ok(Input {
                            path,
                            input,
                            output,
                            line1: Line::new(),
                            line2: line,
                            current_line_is_1: false,
                            current_line_is_in_set: false,
                            linenum: 1,
                        })
                    } else {
                        Err(Signal::Finished)
                    }
                }).collect::<Result<Vec<Input>, Signal>>()
            {
                Ok(inputs) => {
                    // eprintln!("inputs = {:?}", inputs.iter().map(
                    //     |input| &input.current_line().string).collect::<Vec<_>>());
                    let mut inputs = Inputs { inputs };
                    let mut out = BufWriter::new(stdout());

                    'full: loop {
                        // eprintln!("--- loop... ------------");
                        // Get the largest value--this is what we aim for
                        // when retrieving values from the other
                        // inputs.
                        let largest_input_i = inputs.largest_input_index(sortorder);
                        // Are the others the same, or can we update them
                        // to the same value?
                        let mut all_same = true;
                        let mut largest = inputs.input(largest_input_i).current_line();
                        for i in 0..inputs.len() {
                            // eprintln!("for input (largest_input_i = {}, i = {})...",
                            //           largest_input_i, i);
                            if i != largest_input_i {
                                'this_input: loop {
                                    let i_line = inputs.input(i).current_line();
                                    match sortorder.compare(i_line, largest) {
                                        Ordering::Equal => {
                                            break 'this_input;
                                        }
                                        Ordering::Greater => {
                                            all_same = false;
                                            break 'this_input;
                                        }
                                        Ordering::Less => {
                                            if ! inputs.input_mut(i).next(sortorder)? {
                                                break 'full;
                                            }
                                            largest = inputs.input(largest_input_i)
                                                .current_line();
                                        }
                                    }
                                }
                            }
                        }
                        if all_same {
                            // eprintln!("all_same: {:?}", &largest.string);
                            println(&mut out, &largest.string)?;

                            // Mark them all as in set
                            for input in &mut inputs.inputs {
                                input.current_line_is_in_set = true;
                            }
                            // One of them has to advance; empirically it
                            // has to be the (originally!) largest one. XX
                            // Why?
                            inputs.input_mut(largest_input_i).next(sortorder)?;
                        }
                    }

                    // eprintln!("---finish----");
                    out.flush().with_context(
                        || anyhow!("flushing stdout"))?;
                    for (i, input) in inputs.inputs.iter_mut().enumerate() {
                        if input.output.is_some() {
                            // Re-use next() to copy over the
                            // remainder of the file. (Wasteful? No,
                            // verifications should be done anyway.)
                            
                            while input.next(sortorder)? {}
                            input.output.as_mut().expect(
                                "always have a buffer because we already checked in the above if let")
                                .flush().with_context(
                                    || anyhow!("flushing file descriptor {:?}",
                                               output_fd_for_input_index(i)))?;
                        }
                    }
                }
                Err(Signal::Finished) => {
                    // 1+ of the files are empty, we're done
                }
                Err(Signal::Error(e)) => {
                    Err(e)?;
                }
            }

        }
        Mode::Set | Mode::SetThenLinear => {
            let mut set: HashSet<KString> = HashSet::new();
            let mut tmpline = String::new();

            let last_path =
                match mode {
                    Mode::Set => None,
                    Mode::SetThenLinear => Some(paths.pop_back().unwrap()),
                    _ => panic!()
                };

            let mut paths_meta: VecDeque<(PathBuf, u64)> =
                paths.into_iter().map(
                    |path| {
                        let s = path.metadata().with_context(
                            || anyhow!("stat on file {:?}", path))?.size();
                        Ok((path, s))}
                )
                .collect::<Result<_>>()?;
            paths_meta.make_contiguous().sort_by_key(|x| x.1);

            let first_path = paths_meta.pop_front().unwrap().0;
            {
                let path = first_path;
                let mut inp = ReadWithContext::open_path(&path)?;
                while inp.easy_read_line(&mut tmpline)? {
                    set.insert(KString::from(&tmpline));
                }
            }

            for (path, _) in paths_meta {
                if set.is_empty() {
                    break
                }
                let mut inp = ReadWithContext::open_path(&path)?;
                let mut newset = HashSet::new();
                while inp.easy_read_line(&mut tmpline)? {
                    let line = KString::from(&tmpline);
                    if set.contains(&line) {
                        newset.insert(line);
                    }
                }
                set = newset;
            }

            let mut out = BufWriter::new(stdout());
            match mode {
                Mode::Set => {
                    let mut v: Vec<KString> = set.into_iter().collect();
                    v.sort();
                    for line in v {
                        tmpline.clear();
                        tmpline.push_str(&line);
                        println(&mut out, &mut tmpline)?;
                    }
                }
                Mode::SetThenLinear => {
                    let path = last_path.unwrap();
                    let mut inp = ReadWithContext::open_path(&path)?;
                    while inp.easy_read_line(&mut tmpline)? {
                        if set.contains(&KString::from(&tmpline)) {
                            println(&mut out, &mut tmpline)?;
                        }
                    }
                }
                _ => panic!()
            }
            out.flush()?;
        }
        Mode::StructSizes => print_sizes()
    }
    
    Ok(())
}
