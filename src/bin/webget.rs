use std::{
    borrow::Cow,
    io::{stdout, Write},
    path::PathBuf,
};

use anyhow::{Context, Result};
use cj_path_util::temp_file::unbuffered_temp_file_for;
use clap::Parser;

use external_webget::{
    Request, ResponseToFile, ResponseToVec, WebGet, WebGetCommand,
};

#[derive(clap::Parser, Debug)]
#[clap(next_line_help = true)]
/// Tool to test the webget library
struct Opts {
    /// Which command to use, must be one of the supported ones.
    #[clap(long)]
    command: Option<WebGetCommand>,

    /// Additional headers to send
    #[clap(long)]
    header: Vec<String>,

    /// Whether to follow redirects
    #[clap(long)]
    follow: bool,

    /// Output file
    #[clap(long)]
    file: Option<PathBuf>,

    /// URL to retrieve
    url: String,
}

fn main() -> Result<()> {
    let opts: Opts = Opts::parse();
    let webget = if let Some(command) = opts.command {
        WebGet::new(command)?
    } else {
        WebGet::auto_choose()?
    };
    let headers: Cow<[Cow<str>]> =
        opts.header.into_iter().map(|s| s.into()).collect();
    let request = Request {
        url: (&*opts.url).into(),
        follow: opts.follow,
        headers,
    };
    if let Some(path) = &opts.file {
        let temp_file = unbuffered_temp_file_for(path.clone(), None)?;
        let (temp_path, file) = temp_file.into_constituents();
        let () = webget.request(&request, ResponseToFile(file))?;
        temp_path.persist()?;
    } else {
        let response = webget.request(&request, ResponseToVec)?;
        let mut out = stdout().lock();
        out.write_all(&response).context("writing to stdout")?;
    }
    Ok(())
}
