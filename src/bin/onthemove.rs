use std::{
    ffi::OsString, fmt::Debug, path::PathBuf, process::Command, thread,
    time::Duration,
};

use anyhow::{anyhow, bail, Context, Result};
use chj_rustbin::util::network_interface::{
    external_interfaces, ExternalInterfaces, SummarizeAs,
};
use clap::Parser;
use log::{debug, info};

fn act_as<T>(act: impl Fn(&T) -> Result<()>, sleep_time: f64) -> Result<()>
where
    T: PartialEq + Debug,
    ExternalInterfaces: SummarizeAs<T>,
{
    let mut last_ifaces: Option<T> = None;
    let mut i: u64 = 0;
    loop {
        let new_ifaces = Some(external_interfaces()?.summarize());
        if last_ifaces != new_ifaces {
            let ifaces = new_ifaces.as_ref().expect("made as a Some above");
            info!("have interfaces: {ifaces:#?}");
            act(ifaces)?;
            last_ifaces = new_ifaces;
        } else {
            debug!("{i}");
        }
        i += 1;
        thread::sleep(Duration::from_secs_f64(sleep_time));
    }
}

#[derive(clap::Parser, Debug)]
/// Observe public network interfaces in an endless loop, and run a
/// command with "start" if no interfaces are present and "stop" if
/// there are. Runs the command immediately on startup and then again
/// every time the status changes. .
#[clap(name = "onthemove from chj-rustbin")]
struct Opt {
    /// Show what it's doing
    #[clap(short, long)]
    verbose: bool,

    /// Show what it's doing in more detail
    #[clap(long)]
    debug: bool,

    /// Show interface details
    #[clap(short, long)]
    details: bool,

    /// Sleep duration between interface checks in seconds (floating
    /// point)
    #[clap(short, long, default_value = "1")]
    sleep_time: f64,

    /// Program to run
    cmd: PathBuf,

    /// Program arguments; "start" or "stop" is appended
    args: Vec<OsString>,
}

fn main() -> Result<()> {
    let Opt {
        verbose,
        debug,
        details,
        sleep_time,
        cmd,
        args,
    } = Opt::from_args();
    if verbose {
        std::env::set_var("RUST_LOG", "info");
    }
    if debug {
        std::env::set_var("RUST_LOG", "trace");
    }
    env_logger::init();

    let act = |start: bool| -> Result<()> {
        let mut command = Command::new(&cmd);
        command
            .args(&args)
            .arg(if start { "start" } else { "stop" });
        let status = command
            .status()
            .with_context(|| anyhow!("executing {cmd:?}"))?;
        if status.success() {
            Ok(())
        } else {
            bail!("executing {command:?}: {status}")
        }
    };

    if details {
        act_as::<Vec<String>>(|vals| act(vals.is_empty()), sleep_time)?;
    } else {
        act_as::<bool>(|have_ifaces| act(!*have_ifaces), sleep_time)?;
    }

    Ok(())
}
