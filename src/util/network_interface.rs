use std::{iter::Filter, vec::IntoIter};

use anyhow::Result;
use network_interface::{NetworkInterface, NetworkInterfaceConfig};

pub struct ExternalInterfaces(
    Filter<
        IntoIter<NetworkInterface>,
        for<'a> fn(&'a NetworkInterface) -> bool,
    >,
);

/// Network interfaces that are not internal, do have an address
/// (i.e. are up?), and whose name does not start with "tap"
pub fn external_interfaces() -> Result<ExternalInterfaces> {
    let ifaces = NetworkInterface::show()?;
    fn f(iface: &NetworkInterface) -> bool {
        (!iface.internal)
            && !iface.addr.is_empty()
            && !iface.name.starts_with("tap")
    }
    Ok(ExternalInterfaces(ifaces.into_iter().filter(f)))
}

pub trait SummarizeAs<Summary> {
    fn summarize(self) -> Summary;
}

impl SummarizeAs<Vec<NetworkInterface>> for ExternalInterfaces {
    fn summarize(self) -> Vec<NetworkInterface> {
        self.0.collect()
    }
}

impl SummarizeAs<Vec<String>> for ExternalInterfaces {
    /// Give a vec of interface names
    fn summarize(self) -> Vec<String> {
        let mut vec: Vec<_> = self.0.map(|iface| iface.name).collect();
        vec.sort();
        vec
    }
}

impl SummarizeAs<bool> for ExternalInterfaces {
    /// Return whether any such interface exists
    fn summarize(mut self) -> bool {
        self.0.next().is_some()
    }
}
