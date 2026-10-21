//! A wrapper around the `curl`, `wget`, or `GET` utilities to
//! retrieve web pages. To save on code size (i.e. to avoid linking
//! tokio, reqwest, a system ssl library (probably openssl) which
//! reqwest does statically hence never security-updated).

pub mod util;

use std::{
    borrow::Cow, fs::File, os::unix::process::ExitStatusExt, path::PathBuf,
    process::Command, str::FromStr,
};

use anyhow::{anyhow, bail, Context, Result};
use bstr::BString;
use which::which;

use crate::util::format_string_list;

#[derive(Debug)]
pub struct Request<'t> {
    pub url: Cow<'t, str>,
    pub follow: bool,
    pub headers: Cow<'t, [Cow<'t, str>]>,
}

pub trait ResponseStorage {
    type Output;
    fn into_output_file(self) -> Option<File>;
    fn make_output(v: Vec<u8>) -> Self::Output;
}

pub struct ResponseToVec;

impl ResponseStorage for ResponseToVec {
    type Output = BString;

    fn into_output_file(self) -> Option<File> {
        None
    }
    fn make_output(v: Vec<u8>) -> Self::Output {
        v.into()
    }
}

/// Write the response to a file
///
/// Note: if you have a `cj_path_util::TempFile`, use its
/// `into_constituents` method to get the `File`.
///
// NOTE: it's not possible to (mutably) borrow File:
// `command.stdout(..)` in the code requires moving the `File`.
pub struct ResponseToFile(pub File);

impl ResponseStorage for ResponseToFile {
    type Output = ();

    fn into_output_file(self) -> Option<File> {
        Some(self.0)
    }
    fn make_output(_v: Vec<u8>) -> Self::Output {
        // _v should be empty in this case. Simply drop it.
        ()
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum WebGetCommand {
    Curl,
    Wget,
    GET,
}

impl FromStr for WebGetCommand {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use WebGetCommand::*;
        match s {
            "curl" => Ok(Curl),
            "wget" => Ok(Wget),
            "GET" => Ok(GET),
            _ => bail!(
                "unsupported command, needs to be one of curl | wget | GET"
            ),
        }
    }
}

impl WebGetCommand {
    pub const LIST: &'static [WebGetCommand] = {
        use WebGetCommand::*;
        &[Curl, Wget, GET]
    };

    pub fn command_name(self) -> &'static str {
        match self {
            WebGetCommand::Curl => "curl",
            WebGetCommand::Wget => "wget",
            WebGetCommand::GET => "GET",
        }
    }

    pub fn request_options(self) -> &'static [&'static str] {
        match self {
            WebGetCommand::Curl => &["--silent", "--show-error"],
            WebGetCommand::Wget => &["--no-verbose", "-O", "-"],
            WebGetCommand::GET => &[],
        }
    }

    /// The options to use if following redirects is desired, or not
    /// (without adding these options, the tools differ in their
    /// behaviour!) XX: consistency of the clients is bad here, only
    /// `follow == true` works consistently!
    pub fn redirect_options(self, follow: bool) -> &'static [&'static str] {
        match self {
            WebGetCommand::Curl => {
                if follow {
                    &["--location"]
                } else {
                    // XX: evil: silently returns "" if the server
                    // sends a redirect!
                    &[]
                }
            }
            WebGetCommand::Wget => {
                if follow {
                    &[]
                } else {
                    // Leads wget to return an error if the server
                    // sends a redirect. XX consistency not given.
                    &["--max-redirect=0"]
                }
            }
            WebGetCommand::GET => &[], // XX? follows by default, can't be turned off?
        }
    }

    /// Add command line arguments for sending the given "HeaderName:
    /// Value" header.
    pub fn add_header<'s>(self, header: &'s str, arguments: &mut Vec<&'s str>) {
        match self {
            WebGetCommand::Curl | WebGetCommand::Wget => {
                arguments.push("--header".into());
                arguments.push(header.into());
            }
            WebGetCommand::GET => {
                arguments.push("-H".into());
                arguments.push(header.into());
            }
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct WebGet {
    command: WebGetCommand,
    command_path: PathBuf,
}

impl WebGet {
    /// Use the given command
    pub fn new(command: WebGetCommand) -> Result<Self> {
        let command_name = command.command_name();
        let command_path = which(command_name).with_context(|| {
            anyhow!("could not find command {command_name:?}")
        })?;
        Ok(WebGet {
            command,
            command_path,
        })
    }

    /// Find a supported command to run
    pub fn auto_choose() -> Result<Self> {
        let mut tried = Vec::new();
        for command in WebGetCommand::LIST {
            let command_name = command.command_name();
            match which(command_name) {
                Ok(command_path) => {
                    return Ok(WebGet {
                        command: *command,
                        command_path,
                    })
                }
                Err(_) => (),
            }
            tried.push(command_name);
        }

        bail!(
            "could not find a known command for retrieving web URLs, tried: {}",
            format_string_list(tried)
        )
    }

    /// Carry out the given request. If `output` is of type
    /// `ResponseToVec`, the response is returned as a byte vector,
    /// whereas if it is of type `ResponseToFile` then it is written
    /// to the `File` contained in `ResponseToFile` (note: you could
    /// use `cj_path_util::tempfile_utils::tempfile_unbuffered` to set
    /// up the `File` for this).
    pub fn request<O: ResponseStorage>(
        &self,
        request: &Request,
        output: O,
    ) -> Result<O::Output> {
        // Can't use Cow<str> in args as that doesn't cast to OsStr, bummer.
        let mut args: Vec<&str> = vec![];

        for option in self.command.request_options() {
            args.push(option);
        }

        for option in self.command.redirect_options(request.follow) {
            args.push(option);
        }

        args.push(request.url.as_ref());

        for header in &*request.headers {
            self.command.add_header(header.as_ref(), &mut args);
        }

        let mut command = Command::new(&self.command_path);
        command.args(&args);

        if let Some(output_file) = output.into_output_file() {
            command.stdout(output_file);
        }

        let output = command.output().with_context(|| {
            anyhow!("running {:?} with arguments {:?}", self.command_path, args)
        })?;

        if output.status.success() {
            Ok(O::make_output(output.stdout))
        } else {
            let what = if let Some(code) = output.status.code() {
                format!("exit code {code}")
            } else if let Some(signal) = output.status.signal() {
                format!("signal {signal}")
            } else {
                format!("unknown??")
            };
            let stderr = output.stderr;
            bail!(
                "command {:?} with arguments {:?} exited with {what} (stderr: {:?})",
                self.command_path,
                args,
                String::from_utf8_lossy(&stderr)
            )
        }
    }
}
