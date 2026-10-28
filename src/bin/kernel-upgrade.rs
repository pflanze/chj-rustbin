use std::{
    collections::HashMap,
    ffi::{OsStr, OsString},
    fmt::Display,
    io::{stderr, Write},
    num::NonZeroUsize,
    path::{Path, PathBuf},
    process::{Child, Command, Stdio},
    sync::Arc,
};

use anyhow::{anyhow, bail, Context, Result};
use bstr::{BStr, BString, ByteSlice};
use chj_rustbin::{
    def_subdirs,
    easy_webget::{EasyWebget, GetToFileError},
    file_make::{file_make, MakeError},
    gpg::Gpg,
    script_tools::{cmd, touch, CmdCwd, SetX},
    scripting::{kernel_version::KernelVersion, sha256sum::sha256sum},
    util::{
        anyhow::AnyhowError, link_rename::link_rename,
        num_hardware_threads::num_hardware_threads,
    },
};
use cj_path_util::{path_util::AppendToPath, unix::polyfill::add_extension};
use clap::Parser;

use external_webget::{RequestError, WebGet};

const KERNEL_KEY_GPG_MESSAGE: &str =
    "using RSA key B8868C80BA62A1FFFAF5FDA9632D3A06589DA6B1";

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
    /// `no-change`, `new-kernel-ready`, `error`, and then in the latter cases
    /// some more information (the kernel version and path, or the error
    /// message). This is in addition to the normal reporting via stdout/stderr
    /// (but that might be redirected to a log file, hence this option), but the
    /// exit code is changed to always be 0 when the program manages to finish
    /// writing the report file.
    #[clap(long)]
    report_to: Option<PathBuf>,

    /// Path to the base directory containing a "linux" subdir with the kernel
    /// sources. "hashes", "patches", "configs" and "stamps" subdirs will be
    /// created.
    base_dir: PathBuf,
}

/// Map a relative path to the SHA-256 hash hex string
struct PathFileHashes(HashMap<OsString, BString>);

def_subdirs! {Dirs => linux, hashes, patches, configs, stamps}

#[derive(Debug)]
struct KernelWorker {
    webget: WebGet,
    dirs: Dirs,
}

impl KernelWorker {
    fn hashes_asc_url(&self) -> &'static str {
        "https://cdn.kernel.org/pub/linux/kernel/v7.x/incr/sha256sums.asc"
    }

    fn get_hashes_asc(
        &self,
        newer_than_paths: &[&Path],
    ) -> Result<Arc<Path>, MakeError<GetToFileError>> {
        file_make(
            self.dirs.hashes().append("sha256sums.asc"),
            newer_than_paths,
            |target, _| self.webget.get_to_file(&self.hashes_asc_url(), target),
        )
    }

    /// Verifies the signature of the hashes list file via gpg
    fn get_hashes_file(
        &self,
        newer_than_paths: &[&Path],
    ) -> Result<Arc<Path>, MakeError<AnyhowError>> {
        file_make(
            self.dirs.hashes().append("sha256sums"),
            [self
                .get_hashes_asc(newer_than_paths)
                .map_err(anyhow::Error::from)
                .map_err(AnyhowError)?],
            |target, dependencies| -> Result<(), AnyhowError> {
                let [source] = dependencies;
                let err =
                    Gpg::default().verify_unpack(source, target).with_context(
                        || anyhow!("verifying signature on {source:?}"),
                    )?;
                if err.contains_str(KERNEL_KEY_GPG_MESSAGE) {
                    eprintln!("good signature for {target:?}");
                    Ok(())
                } else {
                    stderr()
                        .write_all(&err)
                        .with_context(|| anyhow!("writing to stderr"))?;
                    Err(anyhow!(
                        "missing {KERNEL_KEY_GPG_MESSAGE:?} in GPG output"
                    ))?
                }
            },
        )
    }

    /// Downloads, verifies and parses the sha256sum file.
    ///
    /// Give all the patch file paths you want to use the
    /// `PathFileHashes` on, so that the sha256sum is re-retrieved if
    /// any of them is newer! (Thankfully the hashes are only needed
    /// after getting a new patch file, so, file based caching is all
    /// that's needed to avoid overly many re-retrievals. Also, could
    /// just retry once missing a file in the hash set.)
    fn get_hashes(&self, newer_than_paths: &[&Path]) -> Result<PathFileHashes> {
        let hashes_path = self.get_hashes_file(newer_than_paths)?;

        eprintln!("parse {hashes_path:?}");
        let contents: BString = std::fs::read(&hashes_path)
            .with_context(|| anyhow!("reading {hashes_path:?}"))?
            .into();

        let mut lines: Vec<_> =
            contents.split(|c| *c == b'\n' || *c == b'\r').collect();
        if lines.last() == Some(&b"".as_slice()) {
            lines.pop();
        }

        let table = lines.iter()
            .enumerate()
            .map(|(i, line)|-> Result<(_, _)> {
                let line = line.as_bstr();
                let items : Vec<&BStr> = line.split_str(b"  ").map(|b| b.as_bstr()).collect();
                match items.as_slice() {
                    [hash, path] => {
                        // Uff. Only on Unix, and only from slices, *then* to_owned.
                        use std::os::unix::ffi::OsStrExt;
                        let path = OsStr::from_bytes(path).to_owned();
                        Ok((path, (*hash).to_owned()))
                    },
                    _ =>  bail!(
                        "file {hashes_path:?} line {} does not contain 2 items split by '  ': \
                         {items:?}",
                        i + 1
                    )
                }
            }).collect::<Result<HashMap<OsString, BString>>>()?;
        Ok(PathFileHashes(table))
    }

    // -----------------------------------------------------------------------------

    fn current_kernel(&self) -> Result<Kernel<'_>> {
        Ok(Kernel {
            worker: self,
            version: KernelVersion::from_makefile_in_directory(
                &self.dirs.linux(),
            )?,
        })
    }
}

#[derive(Debug)]
struct Kernel<'worker> {
    worker: &'worker KernelWorker,
    version: KernelVersion,
}

impl<'worker> Kernel<'worker> {
    fn config_backup_path(&self) -> PathBuf {
        self.worker
            .dirs
            .configs()
            .append(format!("config-{}", self.version.to_string()))
    }

    /// Returns the path to the backed up config file
    fn make_oldconfig(&self) -> Result<Arc<Path>, MakeError<AnyhowError>> {
        let linux_path = self.worker.dirs.linux();
        file_make(
            self.config_backup_path(),
            [
                // Make sure there is an initial file, or `make oldconfig` will
                // generate a default config which is not going to be useful!
                self.worker.dirs.linux().append(".config"),
                // (No need to depend on the mtime of anything else like the
                // Makefile, since the target is versioned and won't exist from
                // before updating to this kernel version.)
            ],
            |target, dependencies| {
                let [dependency] = dependencies;
                {
                    // (Should this region be redirected into a file? All of the
                    // program's output, right, i.e. set stdout and stderr
                    // temporarily?)

                    let _cwd = CmdCwd::new(linux_path.clone());
                    cmd("make", ["oldconfig"])?;
                }
                std::fs::copy(dependency, target).with_context(|| {
                    anyhow!("copying {dependency:?} to {target:?}")
                })?;
                Ok(())
            },
        )
    }

    fn stamp_path(&self) -> PathBuf {
        self.worker.dirs.stamps().append(self.version.to_string())
    }

    /// (Returns the path to the stamp file)
    fn build(
        &self,
        num_cores: NonZeroUsize,
    ) -> Result<Arc<Path>, MakeError<AnyhowError>> {
        let linux_path = self.worker.dirs.linux();
        file_make(
            self.stamp_path(),
            [self.make_oldconfig()?],
            |target, _dependencies| {
                {
                    // (Should this region be redirected into a file? All of the
                    // program's output, right, i.e. set stdout and stderr
                    // temporarily?)

                    let j = &format!("-j{num_cores}");
                    let _cwd = CmdCwd::new(linux_path.clone());
                    cmd("make", [j])?;
                    cmd("make", [j, "modules"])?;
                }
                touch(target)?;
                Ok(())
            },
        )
    }

    fn the_patch_to_the_next_point_version(&self) -> KernelPatch<'worker> {
        KernelPatch {
            worker: self.worker,
            from_version: self.version.clone(),
        }
    }
}

#[derive(Debug)]
struct KernelPatch<'worker> {
    worker: &'worker KernelWorker,
    from_version: KernelVersion,
}

impl<'worker> KernelPatch<'worker> {
    fn file_name(&self) -> String {
        self.from_version.patch_filename_to_next()
    }

    fn file_path(&self) -> PathBuf {
        self.worker.dirs.patches().append(self.file_name())
    }

    fn url(&self) -> String {
        format!(
            "https://cdn.kernel.org/pub/linux/kernel/v7.x/incr/{}",
            self.file_name()
        )
    }

    /// Fetch or local file if present
    fn get_unchecked(&self) -> Result<Arc<Path>, MakeError<GetToFileError>> {
        file_make(
            add_extension(self.file_path(), "-unchecked")
                .expect("file_path is non-empty"),
            (),
            |target, _| self.worker.webget.get_to_file(&self.url(), target),
        )
    }
}

#[derive(Debug, thiserror::Error)]
pub enum GetCheckedError {
    #[error("{0}")]
    GetUnchecked(#[from] MakeError<GetToFileError>),
    #[error("{0}")]
    CheckError(#[from] anyhow::Error),
}

impl<'worker> KernelPatch<'worker> {
    fn get_checked(&self) -> Result<Arc<Path>, MakeError<GetCheckedError>> {
        file_make(
            self.file_path(),
            [self.get_unchecked().map_err(GetCheckedError::from)?],
            |target, dependencies| -> Result<(), GetCheckedError> {
                let [source] = dependencies;

                let target_filename = target.file_name().ok_or_else(|| {
                    anyhow!("can't get file_name for {target:?}")
                })?;

                // First see if we have an existing file that might suit us
                let old_hashes = self.worker.get_hashes(&[])?;
                let new_hashes;

                let sum_expected =
                    if let Some(sum) = old_hashes.0.get(target_filename) {
                        sum
                    } else {
                        // Have to actually get a sums file newer than the patch
                        new_hashes = self.worker.get_hashes(&[source])?;
                        new_hashes.0.get(target_filename).ok_or_else(|| {
                            anyhow!("missing hash for file {target:?}")
                        })?
                    };

                let sum = sha256sum(source)?;

                if sum_expected == sum.as_bytes() {
                    eprintln!(
                        "hashes for {target:?} match: {sum_expected} == {sum}"
                    );

                    // Do not use rename--it would lead to re-fetching of
                    // the dependency! `link_rename` is fine as
                    // `webget.get_to_file` uses a tempfile-rename
                    // approach, hence never mutates the hard-linked file.
                    link_rename(source, target).with_context(|| {
                        anyhow!("linking {source:?} to {target:?}")
                    })?;
                    Ok(())
                } else {
                    Err(anyhow!(
                        "hashes do not match for {target:?}: \
                         expected {sum_expected}, got {sum}"
                    ))?
                }
            },
        )
    }
}

#[derive(Debug, thiserror::Error)]
pub enum GetAndApplyError {
    #[error("{0}")]
    GetCheckedError(#[from] MakeError<GetCheckedError>),
    #[error("{0}")]
    Anyhow(#[from] anyhow::Error),
}

impl<'worker> KernelPatch<'worker> {
    /// Returns the new Kernel (in old place)
    // GetToFileError
    fn get_and_apply(&self) -> Result<Kernel<'worker>, GetAndApplyError> {
        let Self {
            worker,
            from_version,
        } = self;
        let path = self.get_checked()?;

        let mut xzcat: Child = Command::new("xzcat")
            .arg(&*path)
            .stdout(Stdio::piped())
            .spawn()
            .context("starting 'xzcat'")?;

        let inp = xzcat.stdout.take().expect("requested above");

        let mut patch: Child = Command::new("patch")
            .arg("-p1")
            .current_dir(worker.dirs.linux())
            .stdin(inp)
            .spawn()
            .context("starting 'patch'")?;

        let mut errors = Vec::new();
        let xzcat_status = xzcat.wait().context("waiting for xzcat")?;
        if !xzcat_status.success() {
            errors.push(format!("xzcat failed: {xzcat_status}"));
        }
        let patch_status = patch.wait().context("waiting for patch")?;
        if !patch_status.success() {
            errors.push(format!("patch failed: {patch_status}"));
        }
        if !errors.is_empty() {
            Err(anyhow!(
                "applying {:?}: {}",
                self.file_path(),
                errors.join(", ")
            ))?;
        }

        Ok(Kernel {
            worker: *worker,
            version: from_version.sublevel_add(1),
        })
    }
}

/// Returns a success report kind and statement
fn run(opts: &Opts) -> Result<(ReportKind, String)> {
    let _x = SetX::new();

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
        eprintln!("current kernel = {}", current_kernel.version);

        if opts.make_oldconfig || opts.build {
            current_kernel.make_oldconfig()?;
        }

        let patch = current_kernel.the_patch_to_the_next_point_version();
        current_kernel = match patch.get_and_apply() {
            Ok(kernel) => kernel,
            Err(e) => match &e {
                GetAndApplyError::GetCheckedError(MakeError::BuildAction(
                    GetCheckedError::GetUnchecked(MakeError::BuildAction(
                        GetToFileError::RequestError(RequestError::NotFound),
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

    Ok(if num_updates == 0 {
        (ReportKind::NoChange, "".into())
    } else {
        (
            ReportKind::NewKernelReady,
            format!(
                "{}\n{}",
                current_kernel.version,
                current_kernel.worker.dirs.linux().display()
            ),
        )
    })
}

fn main() -> Result<()> {
    let opts: Opts = Opts::parse();

    if let Some(report_path) = &opts.report_to {
        let report = match run(&opts) {
            Ok((report_kind, success_statement)) => {
                format!("{report_kind}\n{success_statement}")
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
