use anyhow::{Result, bail, Context, Error};
use structopt::StructOpt;
use thiserror::Error;
use std::cmp::Ordering;
use std::fs::File;
use std::io::{BufReader, BufRead, stdout, Write, BufWriter};
use std::os::unix::prelude::MetadataExt;
use std::path::{PathBuf, Path};
use std::collections::{VecDeque, HashSet};
use kstring::KString;

#[derive(StructOpt, Debug)]
/// Print the lines that occur in all input files. Currently, reads
/// the first file into an index in memory. By default, the order of
/// the output lines follows the last file, and if there are
/// repetitions in the last file, those are repeated, too.

/// (Todo: sorted intersection mode.)
#[structopt(name = "intersection from chj-rustbin")]
struct Opt {
    /// Show the set, not the filtered last file (i.e. there will be
    /// no repetitions in the output, and it is lexically sorted)
    #[structopt(long)]
    set: bool,

    /// Assume that the input files are lexically sorted (uses a
    /// streaming implementation).
    #[structopt(long)]
    sorted: bool,

    /// The paths to files to get the intersection of
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


#[derive(Debug)]
struct Input {
    path: PathBuf,
    input: BufReader<File>,
    line1: String,
    line2: String,
    current_line_is_1: bool,
    linenum: u64,
}

impl Input {
    fn current_line(&self) -> &String {
        if self.current_line_is_1 { &self.line1 } else { &self.line2 }
    }
    // fn current_line_mut(&mut self) -> &mut String {
    //     if self.current_line_is_1 { &mut self.line1 } else { &mut self.line2 }
    // }
    fn is_ordered(&self) -> bool {
        if self.current_line_is_1 {
            &self.line1 >= &self.line2
        } else {
            &self.line2 >= &self.line1
        }
    }
    // returns false on EOF
    fn next(&mut self) -> Result<bool> {
        println!("next: {:?} {} {:?}", self.path, self.linenum, self.current_line_is_1);
        self.current_line_is_1 = !self.current_line_is_1;
        // let current_line = self.current_line_mut();
        // XXX fix this^
        let current_line = if self.current_line_is_1 { &mut self.line1 } else { &mut self.line2 };
        if easy_read_line(&mut self.input, current_line)? {
            self.linenum += 1;
            if ! self.is_ordered() {
                bail!("File is not ordered: {:?} line {}",
                      self.path,
                      self.linenum)
            }
            Ok(true)
        } else {
            Ok(false)
        }
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
        &self
    ) -> usize {
        // self.inputs.iter().max_by_key(|x| x.current_line())
        // XXX^ get to work?
        self.inputs.iter().enumerate()
            .max_by(|a, b| {
                a.1.current_line().cmp(b.1.current_line())
            }).unwrap().0
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
    #[error("Signal: Finished")]
    Finished,
    #[error("Signal: Error({0:?})")]
    Error(Error)
}

fn main() -> Result<()> {
    let opt : Opt = Opt::from_args();
    let mut paths: VecDeque<PathBuf> = opt.file_paths.into();
    if opt.set {
        if paths.len() < 1 {
            bail!("Need at least 1 input file in --set mode");
        }
    } else {
        if paths.len() < 2 {
            bail!("Need at least 2 input files except in --set mode");
        }
    }

    if opt.set && opt.sorted {
        bail!("Only one of --set or --sorted is valid");
    }

    if opt.sorted {
        match
            paths.into_iter().map(|path| {
                let mut input = open_file(&path).map_err(Signal::Error)?;
                let mut current_line = String::new();
                if easy_read_line(&mut input, &mut current_line).map_err(
                    Signal::Error)? {
                    Ok(Input {
                        path,
                        input,
                        line1: String::new(),
                        line2: current_line,
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
                    // dbg!(&inputs);
                    // Get the largest value--this is what we aim for
                    // when retrieving values from the other
                    // inputs.
                    let largest_input_i = inputs.largest_input_index();
                    // Are the others the same, or can we update them
                    // to the same value?
                    let mut all_same = true;
                    let mut largest = inputs.input(largest_input_i).current_line();
                    dbg!(largest);
                    for i in 0..inputs.len() {
                        if i != largest_input_i {
                            'this_input: loop {
                                let i_line = inputs.input(i).current_line();
                                let res = i_line.cmp(largest);
                                dbg!((i, i_line, res));
                                match res {
                                    Ordering::Equal => {
                                        break 'this_input;
                                    }
                                    Ordering::Greater => {
                                        all_same = false;
                                        break 'this_input;
                                    }
                                    Ordering::Less => {
                                        drop(largest);
                                        if ! inputs.input_mut(i).next()? {
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
                        println(&mut out, largest)?;
                        // One of them has to advance; just pick one:
                        inputs.input_mut(largest_input_i).next()?;
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
        
    } else {
        let mut set: HashSet<KString> = HashSet::new();
        let mut tmpline = String::new();

        let last_path = if opt.set { None } else { Some(paths.pop_back().unwrap()) };

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
        if opt.set {
            let mut v: Vec<KString> = set.into_iter().collect();
            v.sort();
            for line in v {
                tmpline.clear();
                tmpline.push_str(&line);
                println(&mut out, &mut tmpline)?;
            }
        } else {
            let path = last_path.unwrap();
            let mut inp = open_file(&path)?;
            while easy_read_line(&mut inp, &mut tmpline)? {
                if set.contains(&KString::from(&tmpline)) {
                    println(&mut out, &mut tmpline)?;
                }
            }
        }
        out.flush()?;
    }
    
    Ok(())
}
