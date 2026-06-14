use std::{
    borrow::Cow,
    io::{stdout, Write},
};

use anyhow::{Context, Result};
use clap::Parser;

use external_webget::{Request, WebGet, WebGetCommand};

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
    let response = webget.request(&request)?;
    let mut out = stdout().lock();
    out.write_all(&response).context("writing to stdout")?;
    Ok(())
}
