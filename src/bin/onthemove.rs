use std::{
    ffi::OsString, fmt::Debug, iter::Filter, path::PathBuf, process::Command,
    thread, time::Duration, vec::IntoIter,
};

use anyhow::{anyhow, bail, Context, Result};
use clap::Parser;
use log::{debug, info};
use network_interface::{NetworkInterface, NetworkInterfaceConfig};

struct ExternalInterfaces(
    Filter<
        IntoIter<NetworkInterface>,
        for<'a> fn(&'a NetworkInterface) -> bool,
    >,
);

fn external() -> Result<ExternalInterfaces> {
    let ifaces = NetworkInterface::show()?;
    fn f(iface: &NetworkInterface) -> bool {
        !iface.internal
    }
    Ok(ExternalInterfaces(ifaces.into_iter().filter(f)))
}

trait Summarize<Summary> {
    fn summarize(self) -> Summary;
}

impl Summarize<Vec<NetworkInterface>> for ExternalInterfaces {
    fn summarize(self) -> Vec<NetworkInterface> {
        self.0.collect()
    }
}

impl Summarize<bool> for ExternalInterfaces {
    fn summarize(mut self) -> bool {
        self.0.next().is_some()
    }
}

fn act_as<T>(act: impl Fn(&T) -> Result<()>) -> Result<()>
where
    T: PartialEq + Debug,
    ExternalInterfaces: Summarize<T>,
{
    let mut last_ifaces: Option<T> = None;
    let mut i = 0;
    loop {
        let new_ifaces = Some(external()?.summarize());
        if last_ifaces != new_ifaces {
            let ifaces = new_ifaces.as_ref().expect("made as a Some above");
            info!("have interfaces: {ifaces:#?}");
            act(ifaces)?;
            last_ifaces = new_ifaces;
        } else {
            debug!("{i}");
        }
        i += 1;
        thread::sleep(Duration::from_millis(200));
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

    /// Show what it's doing
    #[clap(long)]
    debug: bool,

    /// Show interface details
    #[clap(short, long)]
    details: bool,

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
        act_as::<Vec<NetworkInterface>>(|vals| act(vals.is_empty()))?;
    } else {
        act_as::<bool>(|have_ifaces| act(!*have_ifaces))?;
    }

    Ok(())
}
