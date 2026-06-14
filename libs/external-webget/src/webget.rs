//! A wrapper around the `curl`, `wget`, or `GET` utilities to
//! retrieve web pages. To save on code size (i.e. to avoid linking
//! tokio, reqwest, a system ssl library (probably openssl) which
//! reqwest does statically hence never security-updated).
use std::{
    borrow::Cow, os::unix::process::ExitStatusExt, path::PathBuf, process::Command, str::FromStr,
};

use anyhow::{anyhow, bail, Context, Result};
use which::which;

use crate::util::format_string_list;

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
            _ => bail!("unsupported command, needs to be one of curl | wget | GET"),
        }
    }
}

impl WebGetCommand {
    pub const LIST: &[WebGetCommand] = {
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
        let command_path = which(command_name)
            .with_context(|| anyhow!("could not find command {command_name:?}"))?;
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

    /// Carry out the given request
    pub fn request(&self, request: &Request) -> Result<Vec<u8>> {
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

        let output = command.output().with_context(|| {
            anyhow!("running {:?} with arguments {:?}", self.command_path, args)
        })?;

        if output.status.success() {
            Ok(output.stdout)
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

#[derive(Debug)]
pub struct Request<'t> {
    pub url: Cow<'t, str>,
    pub follow: bool,
    pub headers: Cow<'t, [Cow<'t, str>]>,
}
