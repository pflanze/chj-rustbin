use std::{fmt::Display, num::NonZeroUsize, path::PathBuf};

use anyhow::{anyhow, Context, Result};
use clap::Parser;

use chj_rustbin::{
    easy_webget::GetToFileError,
    file_make::MakeError,
    kernel_upgrade::{
        get_hashes_and_patch::{GetAndApplyError, GetHashesAndPatch},
        get_tar_and_unpack::GetTarAndUnpack,
        kernel_worker::{KernelWorker, KernelWorkerError, KernelWorkerOpts},
        mode::{Action, Mode},
    },
    script_tools::ScriptContext,
    scripting::linux::kernel_version::KernelVersion,
    util::{
        network_interface::{external_interfaces, SummarizeAs},
        num_hardware_threads::num_hardware_threads,
    },
};
use external_webget::RequestError;
use libc::uid_t;
use nix::unistd::{getuid, Uid};

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

#[derive(Debug)]
enum RebootNeeded {
    No,
    Upgrade {
        from: KernelVersion,
        to: KernelVersion,
        linux_path: PathBuf,
    },
}

impl RebootNeeded {
    fn is_yes(&self) -> bool {
        match self {
            RebootNeeded::No => false,
            RebootNeeded::Upgrade {
                from: _,
                to: _,
                linux_path: _,
            } => true,
        }
    }
}

impl Display for RebootNeeded {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RebootNeeded::No => f.write_str("no-reboot-needed"),
            RebootNeeded::Upgrade {
                from,
                to,
                linux_path,
            } => {
                write!(
                    f,
                    "need-reboot from {from} to {to} at: {}",
                    linux_path.display()
                )
            }
        }
    }
}

#[derive(clap::Parser, Debug)]
#[clap(next_line_help = true)]
/// Upgrade the Linux kernel on the local system
///
/// This fetches / updates a local copy of the kernel sources, builds
/// them, reports the need for an upgrade, and installs them. It
/// verifies the cryptographic signatures / signed hashes on all
/// downloaded files.
///
/// It is meant to be run from two user accounts, in two modes: with
/// `--build` as a normal user, and then when the need to
/// install/reboot arises (which can be reported via `--report-to`),
/// it can be run again as root with the same base directory argument
/// and the `--install` or `--install-and-reboot` argument.
///
/// So far this has been tested on Debian.
struct Opts {
    /// For development only: fake user id.
    #[clap(long)]
    dev_force_uid: Option<uid_t>,

    /// Exit silently if another instance is running
    #[clap(long)]
    skip_if_locked: bool,

    /// Exit without error if no public network interface is active
    #[clap(long)]
    skip_if_offline: bool,

    /// Run `make oldconfig` to update the `.config` file for each point update;
    /// this is recommended to potentially improve the precision of the config
    /// updates (the default is false, though, as a secure default). This is
    /// implied by `--build`.
    #[clap(short, long)]
    make_oldconfig: bool,

    /// Build the kernel after updating it (default: false)
    #[clap(short, long)]
    build: bool,

    /// Install an already-built kernel. This must be run as root, and
    /// conflicts with `--make-oldconfig` and `--build`. It does
    /// verify that the kernel was actually built, and if it isn't,
    /// returns an error, as the kernel is supposed to get built as
    /// non-root user, unless `--allow-root-builds` is given which
    /// disables this check.
    #[clap(long)]
    install: bool,

    /// Does `--install`, then if successful reboots the machine
    #[clap(long)]
    install_and_reboot: bool,

    /// Allow the root user to retrieve kernel sources and build
    /// them. Not recommended.
    #[clap(long)]
    allow_root_builds: bool,

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

    /// Path to the base directory optionally containing a "linux"
    /// subdir with the kernel sources. "hashes", "patches", "configs"
    /// and "stamps" subdirs and a ".kernel-upgrade.flock" file will
    /// be created.
    ///
    /// Currently, only ever use the same base directory for the same
    /// `version.patchlevel` version of the linux kernel;
    /// `kernel-upgrade` will (currently) not update to a new
    /// patchlevel version automatically!  And the downloaded files
    /// can currently conflict when using the same base directory for
    /// different patchlevel versions. (Todo: improve)
    base_dir: PathBuf,

    /// If `BASE_DIR` does not contain a `linux` subdir, please
    /// specify the version of linux that should be downloaded (it is
    /// signature-checked and unpacked). Once the linux directory
    /// exists, this argument is ignored. Also, this is just the
    /// version of the snapshot that is downloaded, `kernel-upgrade`
    /// will afterwards automatically update the sublevel (of
    /// `version.patchlevel.sublevel`) to the newest available.
    initial_linux_version: Option<KernelVersion>,
}

struct ProcessedOpts {
    skip_if_locked: bool,
    skip_if_offline: bool,
    mode: Mode,
    num_cores: Option<NonZeroUsize>,
    // (Should this only be for <= Mode::Build / "non-root"?)
    report_to: Option<PathBuf>,
    base_dir: PathBuf,
    // In some situations alway None / unused
    initial_linux_version: Option<KernelVersion>,
}

impl TryFrom<Opts> for ProcessedOpts {
    type Error = anyhow::Error;

    fn try_from(value: Opts) -> std::result::Result<Self, Self::Error> {
        let Opts {
            dev_force_uid,
            skip_if_locked,
            skip_if_offline,
            make_oldconfig,
            build,
            install,
            install_and_reboot,
            allow_root_builds,
            num_cores,
            report_to,
            base_dir,
            initial_linux_version,
        } = value;

        let wanted_action = if install_and_reboot {
            Action::Install { reboot: true }
        } else if install {
            Action::Install { reboot: false }
        } else if build {
            Action::Build
        } else if make_oldconfig {
            Action::MakeOldconfig
        } else {
            Action::Retrieve
        };

        let mode = Mode {
            wanted_action,
            allow_root_builds,
        };

        let uid = if let Some(uid) = dev_force_uid {
            Uid::from_raw(uid)
        } else {
            getuid()
        };
        mode.check_uid(uid)?;

        let initial_linux_version = if mode.allow_install_only() {
            if initial_linux_version.is_some() {
                eprintln!(
                    "note: initial linux version was provided but is \
                     unused since running install only"
                );
            }
            None
        } else {
            initial_linux_version
        };

        Ok(Self {
            skip_if_locked,
            skip_if_offline,
            mode,
            num_cores,
            report_to,
            base_dir,
            initial_linux_version,
        })
    }
}

/// Returns a success report kind and statement
fn run(
    opts: &ProcessedOpts,
) -> Result<(ReportKind, RebootNeeded), KernelWorkerError> {
    let ctx = ScriptContext {
        x: Some(1),
        cwd: None,
    };

    let worker = KernelWorker::new(
        opts.base_dir.clone(),
        KernelWorkerOpts {
            skip_if_locked: opts.skip_if_locked,
            refuse_lock_file_creation: !opts.mode.allow_lock_file_creation(),
        },
        ctx,
        opts.mode,
    )?;

    let linux_path = worker.linux_path();
    let mut current_kernel = if linux_path.exists() {
        worker.current_kernel()?
    } else {
        let Some(version) = &opts.initial_linux_version else {
            Err(anyhow!(
                "directory {linux_path:?} does not exist, please provide \
                 the `INITIAL_LINUX_VERSION` program argument"
            ))?
        };
        let kernel = worker.kernel_with_version(version.clone());
        kernel.get_unpacked().context("getting unpacked kernel")?;
        kernel
    };

    let mut num_updates = 0;

    if opts.mode.do_retrieve() {
        loop {
            eprintln!("current kernel = {}", current_kernel.version());

            if opts.mode.do_make_oldconfig() {
                current_kernel
                    .make_oldconfig()
                    .context("running make oldconfig")?;
            } else {
                eprintln!(
                    "skipping `make oldconfig` since running in {} mode",
                    opts.mode
                );
            }

            let patch = current_kernel.the_patch_to_the_next_point_version();
            current_kernel = match patch.get_and_apply() {
                Ok(kernel) => kernel,
                Err(e) => match &e {
                    MakeError::Dependency(GetAndApplyError::GetChecked(
                        MakeError::Dependency(MakeError::BuildAction(
                            GetToFileError::RequestError(
                                RequestError::NotFound,
                            ),
                        )),
                    )) => {
                        eprintln!("upstream has no patch at {}", patch.url());
                        break;
                    }
                    _ => Err(e).context("getting and applying kernel patch")?,
                },
            };

            num_updates += 1;
        }

        println!("kernel-upgrade: executed {num_updates} steps of patch version updates.");
    }

    let n_hw_threads = num_hardware_threads(opts.num_cores);

    // With --allow-root-builds, this is duplicated with .install()
    // further down as that has a dependency on build anyway, but is
    // cosmetic only.
    if opts.mode.do_build() {
        current_kernel
            .build(n_hw_threads)
            .context("building the kernel")?;
        println!(
            "kernel-upgrade: kernel {} is built.",
            current_kernel.version()
        );
    }

    let change = if num_updates == 0 {
        ReportKind::NoChange
    } else {
        ReportKind::NewKernelReady
    };

    let running_kernel_version = KernelVersion::of_running_kernel()?;
    let reboot_needed = if current_kernel.version() > &running_kernel_version {
        RebootNeeded::Upgrade {
            from: running_kernel_version,
            to: current_kernel.version().clone(),
            linux_path: current_kernel.worker().linux_path(),
        }
    } else {
        RebootNeeded::No
    };

    if reboot_needed.is_yes() {
        // Actually *do* need to install and then potentially reboot
        if opts.mode.do_install() {
            current_kernel
                .install(n_hw_threads)
                .context("installing the kernel")?;
            println!(
                "kernel-upgrade: kernel {} is installed.",
                current_kernel.version()
            );
        }
    }

    Ok((change, reboot_needed))
}

fn main() -> Result<()> {
    let opts: Opts = Opts::parse();
    // Also checks current uid (a bit hacky):
    let processed_opts = ProcessedOpts::try_from(opts)?;

    if processed_opts.skip_if_offline {
        let is_online: bool = external_interfaces()?.summarize();
        if !is_online {
            eprintln!("exiting since there are no active network interfaces");
            return Ok(());
        }
    }

    let reboot_needed = if let Some(report_path) = &processed_opts.report_to {
        let (report, reboot_needed) = match run(&processed_opts) {
            Ok((report_kind, reboot_needed)) => {
                (format!("{report_kind}\n{reboot_needed}\n"), reboot_needed)
            }
            Err(KernelWorkerError::Locked) => return Ok(()),
            Err(e) => (format!("error\n{e:#}\n"), RebootNeeded::No),
        };
        std::fs::write(report_path, report)
            .with_context(|| anyhow!("writing report to {report_path:?}"))?;
        reboot_needed
    } else {
        match run(&processed_opts) {
            Ok((_report_kinde, reboot_needed)) => reboot_needed,
            Err(KernelWorkerError::Locked) => return Ok(()),
            Err(e) => Err(e)?,
        }
    };

    if reboot_needed.is_yes() && processed_opts.mode.do_reboot() {
        ScriptContext::default().cmd("reboot", ())?;
    }
    Ok(())
}
