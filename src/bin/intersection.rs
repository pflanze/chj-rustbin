use anyhow::{Result, bail, Context, Error, anyhow};
use structopt::StructOpt;
use thiserror::Error;
use std::cmp::Ordering;
use std::fs::File;
use std::i64::MIN;
use std::io::{BufReader, BufRead, stdout, Write, BufWriter};
use std::os::unix::prelude::MetadataExt;
use std::path::{PathBuf, Path};
use std::collections::{VecDeque, HashSet};
use kstring::KString;

#[derive(StructOpt, Debug)]
/// Print the lines that occur in all input files. By default, files
/// don't need to be sorted (but see `--sorted`); an in-memory set is
/// built, the order of the output lines follows the last file, and if
/// there are repetitions in the last file, those are repeated, too.

#[structopt(name = "intersection from chj-rustbin")]
struct Opt {
    /// Show the set, not the filtered last file (i.e. there will be
    /// no repetitions in the output, and it is lexically sorted
    /// instead of like the last file). This also allows to include
    /// the file given last in the re-ordering optimization (the
    /// smallest files being processed first).
    #[structopt(long)]
    set: bool,

    /// Assume that the input files are lexically sorted (uses a
    /// streaming implementation).
    #[structopt(long)]
    sorted: bool,

    /// Implies --sorted. Assume numeric sorting instead of
    /// lexical. Currently integers only.
    #[structopt(long)]
    numeric: bool,

    #[structopt(long)]
    structsizes: bool,

    /// The paths to files to get the intersection of.
    #[structopt(parse(from_os_str))]
    file_paths: Vec<PathBuf>,
}

fn println(out: &mut impl Write, line: &String) -> Result<()> {
    out.write_all(line.as_bytes())?;
    out.write_all(b"\n")?;
    Ok(())
}

fn trim(line: &mut String) {
    if line.ends_with("\n") {
        line.pop().unwrap();
    }
}

fn open_file(path: &Path) -> Result<BufReader<File>> {
    Ok(BufReader::new(File::open(path).with_context(
        || format!("opening file {:?}", path))?))
}

// "Clean" read_line function: returns true if it did read a line,
// false on EOF. Does overwrite `line`, not append to it. Removes
// trailing '\n' if present.
fn easy_read_line(inp: &mut BufReader<File>, line: &mut String) -> Result<bool> {
    line.clear();
    if inp.read_line(line)? != 0 {
        trim(line);
        Ok(true)
    } else {
        Ok(false)
    }
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
        match self {
            SortOrder::Lexical => l1.string.cmp(&l2.string),
            SortOrder::Numeric => l1.i64.cmp(&l2.i64),
        }
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
    line1: Line,
    line2: Line,
    current_line_is_1: bool,
    linenum: u64,
}

impl Input {
    fn current_line(&self) -> &Line {
        if self.current_line_is_1 { &self.line1 } else { &self.line2 }
    }
    fn is_ordered(&self, sortorder: SortOrder)
                  -> Result<bool> {
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
                if ! self.is_ordered(sortorder)? {
                    bail!("file is not ordered")
                }
                Ok(true)
            } else {
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

    fn largest_input_index(
        &self,
        sortorder: SortOrder
    ) -> usize {
        self.inputs.iter().enumerate()
            .max_by(|a, b| sortorder.compare(a.1.current_line(),
                                             b.1.current_line()))
            .unwrap().0
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
    println!("{siz}\t{nam}")
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

fn main() -> Result<()> {
    let (mode, mut paths) = {
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

        (mode, paths)
    };

    if paths.len() < mode.min_paths_len() {
        bail!("need at least {} input file(s) in {} mode", mode.min_paths_len(),
              mode.name());
    }

    match mode {
        Mode::Sorted(sortorder) => {
            match
                paths.into_iter().map(|path| {
                    let mut input = open_file(&path).map_err(Signal::Error)?;
                    let mut line = Line::new();
                    if line.read_and_parse_line(&mut input, sortorder)
                        .with_context(|| anyhow!("file {:?} line 1", path))
                        .map_err(Signal::Error)? {
                        Ok(Input {
                            path,
                            input,
                            line1: Line::new(),
                            line2: line,
                            current_line_is_1: false,
                            linenum: 1,
                        })
                    } else {
                        Err(Signal::Finished)
                    }
                }).collect::<Result<Vec<Input>, Signal>>()
            {
                Ok(inputs) => {
                    let mut inputs = Inputs { inputs };
                    let mut out = BufWriter::new(stdout());

                    'full: loop {
                        // Get the largest value--this is what we aim for
                        // when retrieving values from the other
                        // inputs.
                        let largest_input_i = inputs.largest_input_index(sortorder);
                        // Are the others the same, or can we update them
                        // to the same value?
                        let mut all_same = true;
                        let mut largest = inputs.input(largest_input_i).current_line();
                        for i in 0..inputs.len() {
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
                                            drop(largest);
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
                            println(&mut out, &largest.string)?;
                            // One of them has to advance; empirically it
                            // has to be the (originally!) largest one. XX
                            // Why?
                            inputs.input_mut(largest_input_i).next(sortorder)?;
                        }
                    }

                    out.flush()?;
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
                            || format!("stat on file {:?}", path))?.size();
                        Ok((path, s))}
                )
                .collect::<Result<_>>()?;
            paths_meta.make_contiguous().sort_by_key(|x| x.1);

            let first_path = paths_meta.pop_front().unwrap().0;
            {
                let path = first_path;
                let mut inp = open_file(&path)?;
                while easy_read_line(&mut inp, &mut tmpline)? {
                    set.insert(KString::from(&tmpline));
                }
            }

            for (path, _) in paths_meta {
                if set.is_empty() {
                    break
                }
                let mut inp = open_file(&path)?;
                let mut newset = HashSet::new();
                while easy_read_line(&mut inp, &mut tmpline)? {
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
                    let mut inp = open_file(&path)?;
                    while easy_read_line(&mut inp, &mut tmpline)? {
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
