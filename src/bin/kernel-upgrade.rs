use std::{fmt::Display, num::NonZeroUsize, path::PathBuf};

use anyhow::{anyhow, bail, Context, Result};
use clap::Parser;

use chj_rustbin::{
    easy_webget::GetToFileError,
    file_make::MakeError,
    kernel_upgrade::{
        get_hashes_and_patch::{GetAndApplyError, GetHashesAndPatch},
        kernel_worker::{Dirs, KernelWorker},
    },
    script_tools::SetX,
    scripting::linux::kernel_version::KernelVersion,
    util::num_hardware_threads::num_hardware_threads,
};
use external_webget::{RequestError, WebGet};

enum ReportKind {
    NoChange,
    NewKernelReady,
}

impl Display for ReportKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let msg = match self {
            ReportKind::NoChange => "no-change",
            ReportKind::NewKernelReady => "new-kernel-ready",
        };
        f.write_str(msg)
    }
}

enum RebootNeeded {
    None,
    Upgrade {
        from: KernelVersion,
        to: KernelVersion,
    },
}

impl Display for RebootNeeded {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RebootNeeded::None => f.write_str("no-reboot-needed"),
            RebootNeeded::Upgrade { from, to } => {
                write!(f, "need-reboot from {from} to {to}")
            }
        }
    }
}

#[derive(clap::Parser, Debug)]
#[clap(next_line_help = true)]
/// Upgrade the kernel on the system
struct Opts {
    /// Run `make oldconfig` to update the `.config` file for each point update;
    /// this is recommended to potentially improve the precision of the config
    /// updates (the default is false, though, as a secure default). This is
    /// implied by `--build`.
    #[clap(short, long)]
    make_oldconfig: bool,

    /// Build the kernel after updating it (default: false)
    #[clap(short, long)]
    build: bool,

    /// Number of cores to use (default: try to determine automatically)
    #[clap(short = 'j', long)]
    num_cores: Option<NonZeroUsize>,

    /// Write a report consisting of a first line that is one of the strings
    /// `no-change`, `new-kernel-ready`, `error`, a second line
    /// `no-reboot-needed` or `need-reboot ` followed by the running and
    /// ready-to-install version, and then in the latter cases some more
    /// information (the kernel version and path, or the error message). This is
    /// in addition to the normal reporting via stdout/stderr (but that might be
    /// redirected to a log file, hence this option), but the exit code is
    /// changed to always be 0 when the program manages to finish writing the
    /// report file.
    #[clap(long)]
    report_to: Option<PathBuf>,

    /// Path to the base directory containing a "linux" subdir with the kernel
    /// sources. "hashes", "patches", "configs" and "stamps" subdirs will be
    /// created.
    base_dir: PathBuf,
}

/// Returns a success report kind and statement
fn run(opts: &Opts) -> Result<(ReportKind, RebootNeeded, String)> {
    let _x = SetX::new(true);

    let dirs = Dirs::new(opts.base_dir.clone());

    if !dirs.linux().exists() {
        // https://cdn.kernel.org/pub/linux/kernel/v7.x/linux-{version}.tar.xz
        // tar xf ...
        bail!("the kernel source directory is missing, please unpack the desired kernel to {:?}",
              dirs.linux())
    }

    dirs.create_dirs_if_not_exist()?;

    let worker = KernelWorker {
        dirs,
        webget: WebGet::auto_choose()?,
    };

    let mut current_kernel = worker.current_kernel()?;

    let mut num_updates = 0;

    loop {
        eprintln!("current kernel = {}", current_kernel.version());

        if opts.make_oldconfig || opts.build {
            current_kernel.make_oldconfig()?;
        } else {
            eprintln!("skipping `make oldconfig` since neither --build nor --make-oldconfig \
                       has been given");
        }

        let patch = current_kernel.the_patch_to_the_next_point_version();
        current_kernel = match patch.get_and_apply() {
            Ok(kernel) => kernel,
            Err(e) => match &e {
                GetAndApplyError::GetChecked(MakeError::Dependency(
                    MakeError::BuildAction(GetToFileError::RequestError(
                        RequestError::NotFound,
                    )),
                )) => {
                    eprintln!("upstream has no patch at {}", patch.url());
                    break;
                }
                _ => Err(e)?,
            },
        };

        num_updates += 1;
    }

    if opts.build {
        current_kernel.build(num_hardware_threads(opts.num_cores))?;
    }

    // XX and builds and installs, and reboots?, optionally.

    println!("kernel-upgrade: done, executed {num_updates} steps of patch version updates.");

    let running_kernel_version = KernelVersion::of_running_kernel()?;
    let reboot_needed = if current_kernel.version() > &running_kernel_version {
        RebootNeeded::Upgrade {
            from: running_kernel_version,
            to: current_kernel.version().clone(),
        }
    } else {
        RebootNeeded::None
    };

    Ok(if num_updates == 0 {
        (ReportKind::NoChange, reboot_needed, "".into())
    } else {
        (
            ReportKind::NewKernelReady,
            reboot_needed,
            format!(
                "{}\n{}",
                current_kernel.version(),
                current_kernel.worker().dirs.linux().display()
            ),
        )
    })
}

fn main() -> Result<()> {
    let opts: Opts = Opts::parse();

    if let Some(report_path) = &opts.report_to {
        let report = match run(&opts) {
            Ok((report_kind, reboot_needed, success_statement)) => {
                format!("{report_kind}\n{reboot_needed}\n{success_statement}")
            }
            Err(e) => format!("error\n{e:#?}"),
        };
        std::fs::write(report_path, report)
            .with_context(|| anyhow!("writing report to {report_path:?}"))
    } else {
        run(&opts)?;
        Ok(())
    }
}
