//! Simple CPU time measurement probes, printing
//! human-readable/somewhat-TSV format to stderr

use std::{
    cell::UnsafeCell,
    io::{stderr, StderrLock, Write},
    os::unix::prelude::OsStrExt,
    ptr::null,
    sync::atomic::AtomicBool,
    time::SystemTime,
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

thread_local! {
    // Safety: do not touch! Only access from the code in this module!
    pub static THREAD_LOCAL_CONTEXT: UnsafeCell<*const CpuProbeInner> = UnsafeCell::new(null());
}

#[macro_export]
macro_rules! probe {
    { $name:expr } => {
        let __cpu_probe_name;
        // Safety: do not touch __cpu_probe! Only access from the code in this module!
        let __cpu_probe;
        if $crate::cpu_probe::is_active() {
            __cpu_probe_name = $name;
            let parent = $crate::cpu_probe::THREAD_LOCAL_CONTEXT.with(|p| unsafe {
                // Safe because it is initialized to null via the `thread_local!` macro
                *p.get()
            });
            let name = unsafe {
                // Safe because we're only keeping the wrapper as long
                // as this variable is in scope.
                std::mem::transmute::<&str, &'static str>(__cpu_probe_name.as_ref())
            };
            __cpu_probe = $crate::cpu_probe::CpuProbe::Active($crate::cpu_probe::CpuProbeInner {
                parent,
                name,
                file: file!(),
                line: line!(),
                col: column!(),
                start: std::time::SystemTime::now()
            });
            let inner = match &__cpu_probe {
                $crate::cpu_probe::CpuProbe::Active(inner) => inner,
                $crate::cpu_probe::CpuProbe::Inactive => unreachable!(),
            };
            $crate::cpu_probe::THREAD_LOCAL_CONTEXT.with(|p| unsafe {
                *p.get() = inner
            });
        } else {
            __cpu_probe = $crate::cpu_probe::CpuProbe::Inactive;
        }
    }
}

pub struct CpuProbeInner {
    // name is not really static, but we use unsafe
    pub name: &'static str,
    pub file: &'static str,
    pub line: u32,
    pub col: u32,
    pub start: SystemTime,
    pub parent: *const CpuProbeInner,
}

pub enum CpuProbe {
    Active(CpuProbeInner),
    Inactive,
}

fn print_parents(out: &mut StderrLock, parent: *const CpuProbeInner) {
    if !parent.is_null() {
        let p: &CpuProbeInner = unsafe {
            // Safe because the probe objects in the parent stack
            // frames are not moved--XX correct for closures, can't be
            // moved while executed? BUT, relying on not moving the
            // parts that have to be `pub` for macro reasons!
            &*parent
        };
        print_parents(out, p.parent);
        _ = write!(out, "{:?}/", p.name);
    }
}

impl Drop for CpuProbeInner {
    fn drop(&mut self) {
        let Self {
            name,
            file,
            line,
            col,
            start,
            parent,
        } = self;
        let end = SystemTime::now();
        match end.duration_since(*start) {
            Ok(duration) => {
                // Remove us from the context
                THREAD_LOCAL_CONTEXT.with(|p| unsafe {
                    // Safe because the parent is guaranteed to still
                    // be around (see other comment on
                    // `THREAD_LOCAL_CONTEXT`)
                    *p.get() = *parent
                });
                let mut lock = stderr().lock();
                _ = write!(&mut lock, "probe s\t{}\t", duration.as_secs_f64());
                print_parents(&mut lock, *parent);
                _ = writeln!(&mut lock, "{name:?}\t{file}:{line}:{col}",);
            }
            Err(e) => {
                let mut lock = stderr().lock();
                _ = writeln!(
                    &mut lock,
                    "error in CpuPrope::drop: duration from {start:?} to {end:?}: {e:#}"
                );
            }
        }
    }
}
