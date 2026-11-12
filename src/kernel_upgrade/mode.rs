//! Model the various execution needs and allowances for the
//! kernel-upgrade
//!
//! `Mode` represents which `Action` a user wants to do, and what
//! context (root or non-root) it runs in. It can answer whether a
//! particular action should be carried out, and together with an
//! `Action` value from a make function, it can (via `Mode::fm`)
//! return the appropriatete execution mode (if a build should happen
//! or only the existance and up-to-date-ness of a target be checked).
//!

use std::fmt::Display;

use anyhow::{bail, Result};
use nix::unistd::Uid;

use crate::file_make::{FileMake, FileMakeMode};

/// Each successive action implies all of the former except the
/// `Install` only implies the former if `allow_root_builds` is true
#[derive(Debug, Clone, Copy)]
pub enum Action {
    Retrieve,
    MakeOldconfig,
    Build,
    Install { reboot: bool },
}

#[derive(Debug, Clone, Copy)]
pub struct Mode {
    pub wanted_action: Action,
    pub allow_root_builds: bool,
}

impl Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Action::*;
        let msg = match self.wanted_action {
            Retrieve => "retrieval",
            MakeOldconfig => "make-oldconfig",
            Build => "build",
            Install { reboot } => {
                let not = if self.allow_root_builds { "" } else { "not " };
                let then_reboot = if reboot { "then reboot" } else { "" };
                return write!(
                    f,
                    "install ({not}allowing to build){then_reboot}"
                );
            }
        };
        f.write_str(msg)
    }
}

impl Mode {
    pub fn allow_install_only(&self) -> bool {
        use Action::*;
        match self.wanted_action {
            Retrieve | MakeOldconfig | Build => false,
            Install { reboot: _ } => !self.allow_root_builds,
        }
    }

    // Turns out to do the same as fn do_retrieve
    pub fn allow_lock_file_creation(&self) -> bool {
        use Action::*;
        match self.wanted_action {
            // If these are chosen then if we're running as root then
            // --allow-root-builds must have been given, too, and
            // creating the lock file should be allowed
            Retrieve | MakeOldconfig | Build => true,
            Install { reboot: _ } => self.allow_root_builds,
        }
    }

    /// Whether we are asked (directly or indirectly) to retrieve files
    pub fn do_retrieve(&self) -> bool {
        use Action::*;
        match self.wanted_action {
            Retrieve | MakeOldconfig | Build => true,
            Install { reboot: _ } => self.allow_root_builds,
        }
    }

    /// Whether we are asked (directly or indirectly) to use `make oldconfig`
    pub fn do_make_oldconfig(&self) -> bool {
        use Action::*;
        match self.wanted_action {
            Retrieve => false,
            MakeOldconfig | Build => true,
            Install { reboot: _ } => self.allow_root_builds,
        }
    }

    /// Whether we are asked (directly or indirectly) to build
    pub fn do_build(&self) -> bool {
        use Action::*;
        match self.wanted_action {
            Retrieve | MakeOldconfig => false,
            Build => true,
            Install { reboot: _ } => self.allow_root_builds,
        }
    }

    /// Whether we are asked to install
    pub fn do_install(&self) -> bool {
        use Action::*;
        match self.wanted_action {
            Retrieve | MakeOldconfig | Build => false,
            Install { reboot: _ } => true,
        }
    }

    /// Whether we are asked to reboot
    pub fn do_reboot(&self) -> bool {
        use Action::*;
        match self.wanted_action {
            Retrieve | MakeOldconfig | Build => false,
            Install { reboot } => reboot,
        }
    }

    /// A `FileMake` in the suitable mode (build vs. check-only) for
    /// the given action for this operation mode
    pub fn fm(&self, for_action: Action) -> FileMake {
        let build = || FileMake {
            mode: FileMakeMode::Build,
        };
        let check = || FileMake {
            mode: FileMakeMode::ReportMissing,
        };
        if self.allow_install_only() {
            match for_action {
                Action::Retrieve | Action::MakeOldconfig | Action::Build => {
                    check()
                }
                Action::Install { reboot: _ } => build(),
            }
        } else {
            build()
        }
    }

    pub fn check_uid(&self, uid: Uid) -> Result<()> {
        use Action::*;
        match self.wanted_action {
            Retrieve | MakeOldconfig | Build => {
                if uid.is_root() && self.allow_root_builds {
                    bail!(
                        "root is not allowed to retrieve files or build \
                         unless --allow-root-builds is given"
                    )
                } else {
                    Ok(())
                }
            }
            Install { reboot: _ } => {
                if uid.is_root() {
                    Ok(())
                } else {
                    bail!("--install requires root permissions")
                }
            }
        }
    }
}
