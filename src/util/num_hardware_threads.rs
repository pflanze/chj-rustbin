use std::num::NonZeroUsize;

/// Number of CPUs ehr cores ehr hyperthreads available to the program
/// (i.e. virtual ones)
pub fn num_hardware_threads(
    opt_override: Option<NonZeroUsize>,
) -> NonZeroUsize {
    if let Some(num) = opt_override {
        num
    } else {
        num_cpus::get().try_into().expect("should always be at least 1 since otherwise why would we be running?")
    }
}
