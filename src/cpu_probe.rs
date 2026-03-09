//! Simple CPU time measurement probes, printing
//! human-readable/somewhat-TSV format to stderr

use std::{
    os::unix::prelude::OsStrExt, sync::atomic::AtomicBool, time::SystemTime,
};

use anyhow::{bail, Result};

pub static ACTIVE: AtomicBool = AtomicBool::new(false);

pub fn set_active(val: bool) {
    ACTIVE.store(val, std::sync::atomic::Ordering::SeqCst)
}

pub fn is_active() -> bool {
    ACTIVE.load(std::sync::atomic::Ordering::Relaxed)
}

/// Initialize activation status from the `CPU_PROBE` environment
/// variable. Returns an error for invalid values.
pub fn init() -> Result<()> {
    let active = match std::env::var_os("CPU_PROBE") {
        Some(s) => match s.as_bytes() {
            b"0" | b"false" | b"f" | b"inactive" => false,
            b"1" | b"true" | b"t" | b"active" => true,
            _ => bail!(
                "invalid value for environment variable `CPU_PROBE`, please give 0 or 1: {:?}",
                s.to_string_lossy()
            ),
        },
        None => false,
    };
    set_active(active);
    Ok(())
}

#[macro_export]
macro_rules! probe {
    { $name:expr } => {
        let __cpu_probe_name;
        let __cpu_probe = if $crate::cpu_probe::is_active() {
            __cpu_probe_name = $name;
            $crate::cpu_probe::CpuProbe::Active($crate::cpu_probe::CpuProbeInner {
                name: &__cpu_probe_name,
                file: file!(),
                line: line!(),
                col: column!(),
                start: std::time::SystemTime::now()
            })
        } else {
            $crate::cpu_probe::CpuProbe::Inactive
        };
    }
}

pub struct CpuProbeInner<'t> {
    pub name: &'t str,
    pub file: &'static str,
    pub line: u32,
    pub col: u32,
    pub start: SystemTime,
}

pub enum CpuProbe<'t> {
    Active(CpuProbeInner<'t>),
    Inactive,
}

impl<'t> Drop for CpuProbeInner<'t> {
    fn drop(&mut self) {
        let Self {
            name,
            file,
            line,
            col,
            start,
        } = self;
        let end = SystemTime::now();
        match end.duration_since(*start) {
            Ok(duration) => {
                eprintln!(
                    "probe s\t{}\t{name:?}\t{file}:{line}:{col}",
                    duration.as_secs_f64()
                );
            }
            Err(e) => {
                eprintln!("error in CpuPrope::drop: duration from {start:?} to {end:?}: {e:#}");
            }
        }
    }
}
