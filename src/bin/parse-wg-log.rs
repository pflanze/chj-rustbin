use chj_rustbin::numbers::numbers_within;
use chrono::{Timelike, NaiveDate};
use genawaiter::rc::Gen;
use structopt::StructOpt;
use tai64::Tai64N;
use std::{path::PathBuf, fmt::Display, fs::File, io::BufWriter};
use std::io::Write;
use anyhow::{Result, bail, anyhow, Context};
use bit_set::BitSet;

use chj_rustbin::{readwithcontext::ReadWithContext,
                  parseutil::{cleanwhite, parse_byte_multiplier, is_all_white,
                              key_val, after_white},
                  tai::{parse_timestamp, Tai64Format},
                  sequences::group,
                  fp::on};


#[derive(StructOpt, Debug)]
/// Parse a log file consisting of repeated output of `wg` (wireguard
/// command line tool), with tai64n timestamps prepended to each line
/// (DJB daemontools log format).

#[structopt(name = "parse-wg-log from chj-rustbin")]
struct Opt {
    /// Show parsed data directly
    #[structopt(long)]
    show_direct: bool,

    /// Calculate derived values and save as TSV files, one for each
    /// interface. The option specifies the base path, to which
    /// `$interfacename.tsv` is appended.
    #[structopt(long)]
    tsv: Option<String>,

    /// The paths to dirs with files to parse
    #[structopt(parse(from_os_str))]
    dir_paths: Vec<PathBuf>,
}

#[derive(Debug)]
struct Transfer {
    /// bytes total since interface was activated
    received: usize,
    /// bytes total since interface was activated
    sent: usize
}
impl Transfer {
    fn total(&self) -> usize {
        self.received + self.sent
    }
    fn sub(&self, old: &Transfer) -> Result<Transfer> {
        let received = self.received.checked_sub(old.received)
            .ok_or_else(|| anyhow!("old has higher received transfers than new"))?;
        let sent = self.sent.checked_sub(old.sent)
            .ok_or_else(|| anyhow!("old has higher sent transfers than new"))?;
        Ok(Transfer { received, sent })
    }
}

fn parse_transfer(s: &str) -> Result<Transfer> {
    // "19.52 GiB received, 134.39 GiB sent"
    let mut received_f = None;
    let mut sent_f = None;
    for part in s.split(',') {
        let part = cleanwhite(part);
        let v: Vec<_> = part.split(' ').collect();
        if v.len() != 3 {
            bail!("expect 3 parts for a number, for part {part:?}")
        }
        let num: f64 = v[0].parse()?;
        let multiplier = parse_byte_multiplier(v[1])?;
        let label = v[2];
        if label == "received" {
            received_f = Some(num * (multiplier as f64));
        } else if label == "sent" {
            sent_f = Some(num * (multiplier as f64));
        } else {
            bail!("unknown label {label:?}")
        }
    }
    // so verbose code, todo better?
    let received = received_f.ok_or_else(|| anyhow!("missing 'received'"))?
        as usize; // well
    let sent = sent_f.ok_or_else(|| anyhow!("missing 'sent'"))?
        as usize;
    Ok(Transfer { received, sent })
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, PartialOrd, Ord)]
struct WireguardInterface(pub u16);

impl WireguardInterface {
    fn from_str(s: &str) -> Result<Self> {
        if s.starts_with("wg") {
            Ok(Self(s[2..].parse()?))
        } else {
            bail!("interface name does not start with \"wg\": {s:?}")
        }
    }
    // fn to_string(&self) -> String {
    //     format!("wg{}", self.0)
    // }
}

impl Display for WireguardInterface {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("wg{}", self.0))
    }
}

struct UnfinishedPeer {
    interface: WireguardInterface,
}

#[derive(Debug, PartialEq, Clone, Copy)]
struct DateHourUtc {
    hour: u8,
    date: NaiveDate
}

#[derive(Debug)]
struct Datapoint {
    interface: WireguardInterface,
    timestamp: Tai64N,
    date_and_hour: DateHourUtc, // cache, derived from timestamp
    transfer: Transfer,
}

const NUM_INTERFACES: usize = 2;
// Well. a vec with that many *inlined* elements would be best.

#[derive(Debug)]
struct Timepoint([Option<Datapoint>; NUM_INTERFACES]);
impl Timepoint {
    fn _new() -> Self {
        Self(std::array::from_fn(|_i| None))
    }
    fn _insert(&mut self, dp: Datapoint) -> Result<()> {
        let r = self.0.get_mut(dp.interface.0 as usize).ok_or_else(
            || anyhow!("interface outside supported range hard-coded \
                        in NUM_INTERFACES: {}",
                       dp.interface))?;
        *r = Some(dp);
        Ok(())
    }
    pub fn get(&self, i: usize) -> Option<&Datapoint> {
        // if let Some(opt_dp) = self.0.get(i) {
        //     opt_dp.as_ref()
        // } else {
        //     None
        // }
        self.0.get(i).and_then(|v| v.as_ref())
    }
    pub fn from_iter(points: impl Iterator<Item = Datapoint>) -> Result<Self> {
        let mut ps = Self::_new();
        let mut ok = false;
        for p in points {
            ps._insert(p)?;
            ok = true;
        }
        if ok {
            Ok(ps)
        } else {
            bail!("trying to construct Datapoints with empty input iterator")
        }
    }
    /// The first timestamp from the left
    pub fn timestamp(&self) -> &Tai64N {
        for dp in &self.0 {
            if let Some(dp) = dp {
                return &dp.timestamp;
            }
        }
        panic!("always having at least one entry")
    }
    pub fn timestamp_seconds(&self) -> u64 {
        // broken up just because rust-analyzer has some issue with .0.0
        let a = self.timestamp().0;
        a.0
    }
    pub fn date_and_hour(&self) -> DateHourUtc {
        for dp in &self.0 {
            if let Some(dp) = dp {
                return dp.date_and_hour;
            }
        }
        panic!("always having at least one entry")
    }
}

struct Group(pub Vec<Timepoint>);
impl Group {
    fn first_timepoint(&self) -> &Timepoint {
        self.0.first().expect("Group always has at least 1 Timepoint")
    }
    fn last_timepoint(&self) -> &Timepoint {
        self.0.last().expect("Group always has at least 1 Timepoint")
    }
    fn first_datapoint(&self, i: usize) -> Option<&Datapoint> {
        for tp in &self.0 {
            if let Some(dp) = tp.get(i) {
                return Some(dp)
            }
        }
        None
    }
    fn last_datapoint(&self, i: usize) -> Option<&Datapoint> {
        for tp in self.0.iter().rev() {
            if let Some(dp) = tp.get(i) {
                return Some(dp)
            }
        }
        None
    }
    pub fn transfer_diffs<'a>(
        &'a self,
        previous: Option<&'a Self>
    ) -> impl Iterator<Item = (WireguardInterface, Transfer)> + 'a {
        Gen::new(|co| async move {
            for i in 0..NUM_INTERFACES {
                // Get the last Datapoint for `i` from `previous` if
                // that's from the preceding hour and a Datapoint for i
                // is present, or the first from self if present. If
                // present, also get the last Datapoint from self if
                // present, and calculate and yield the transfer diff.

                if let Some(dp1) =
                    previous.and_then(
                        |group| {
                            let l = group.last_timepoint();
                            let f = self.first_timepoint();
                            if let Some(timediff) =
                                f.timestamp_seconds().checked_sub(
                                    l.timestamp_seconds())
                            {
                                if timediff < 3600 {
                                    // adjacent hours
                                    group.last_datapoint(i)
                                } else {
                                    None
                                }
                            } else {
                                eprintln!("WARNING: unexpected non-increasing time \
                                           in subsequent groups: {} to {}",
                                          l.timestamp().to_rfc2822_local(),
                                          f.timestamp().to_rfc2822_local());
                                None
                            }
                        })
                    .or_else(
                        || self.first_datapoint(i))
                {
                    if let Some(dp2) =
                        self.last_datapoint(i)
                    {
                        match dp2.transfer.sub(&dp1.transfer) {
                            Ok(d) => co.yield_(
                                (WireguardInterface(i as u16), d)).await,
                            Err(e) => eprintln!(
                                "can't calculate diff({:?}, {:?}): {e}",
                                dp1.transfer,
                                dp2.transfer),
                        }
                    }
                }
            }
        }).into_iter()
    }
}


const MAX_ERRORS: usize = 2000000;

fn parse_files(
    files: Vec<PathBuf>
) -> Result<Vec<Datapoint>>
{
    let mut line = String::new();
    let mut current_interface: Option<WireguardInterface> = None;
    let mut current_peer: Option<UnfinishedPeer> = None;
    let mut num_errors = 0;
    let mut out = Vec::new();
    for file in files {
        let mut inp = ReadWithContext::open_path(&file)?;

        while inp.easy_read_line(&mut line)? {
            let res = (|current_interface: &mut Option<WireguardInterface>| -> Result<_> {
                let (timestamp, rest) = inp.context(parse_timestamp(&line))?;
                if is_all_white(rest) {
                    return Ok(());
                }
                if let Some((indentkey, val)) = key_val(rest) {
                    let val = cleanwhite(val);
                    if indentkey == "interface" {
                        if current_interface.is_some() {
                            inp.err_with_context(anyhow!(
                                "missed \"peer\" before another \"interface\""))?
                        }
                        *current_interface = Some(WireguardInterface::from_str(val)?);
                    } else if indentkey == "peer" {
                        if current_peer.is_some() {
                            inp.err_with_context(anyhow!(
                                "got \"peer\" again"))?
                        }   
                        if let Some(interface) = current_interface.take() {
                            current_peer = Some(UnfinishedPeer {
                                interface
                            });
                            *current_interface = None;
                        } else {
                            inp.err_with_context(anyhow!(
                                "missed \"peer\" before another \"interface\""))?
                        }
                    } else if let Some(key) = after_white(indentkey) {
                        if key == "public key" {
                        } else if key == "private key" {
                        } else if key == "listening port" {
                        } else if key == "endpoint" {
                        } else if key == "allowed ips" {
                        } else if key == "latest handshake" {
                        } else if key == "transfer" {
                            let transfer = inp.context(parse_transfer(val))?;
                            if let Some(peer) = current_peer.take() {
                                let dt = timestamp.to_datetime_utc();
                                let datehour = DateHourUtc {
                                    date: dt.date_naive(),
                                    hour: dt.hour() as u8
                                };
                                let dp = Datapoint {
                                    timestamp,
                                    date_and_hour: datehour,
                                    transfer,
                                    interface: peer.interface
                                };
                                out.push(dp);
                            } else {
                                inp.err_with_context(anyhow!(
                                    "missing peer before key {key:?}"))?
                            }
                        } else {
                            inp.err_with_context(anyhow!(
                                "unknown indented key {key:?}"))?
                        }
                    } else {
                        inp.err_with_context(anyhow!(
                            "unknown key {indentkey:?}"))?
                    }
                } else {
                    inp.err_with_context(anyhow!(
                        "line does not match `key: val` pattern"))?
                }
                Ok(())
            })(&mut current_interface);
            match res {
                Ok(_) => {},
                Err(e) =>
                    if num_errors < MAX_ERRORS {
                        num_errors += 1;
                        eprintln!("Warning: {e:?}");
                    } else {
                        return Err(e)
                    }
            }
        }
    }
    Ok(out)
}

fn main() -> Result<()> {
    let opt : Opt = Opt::from_args();
    if !opt.show_direct && !opt.tsv.is_some() {
        eprintln!("WARNING: neither --tsv nor --show-direct given, \
                   going to parse without output");
    }

    let mut file_paths: Vec<PathBuf> = Vec::new();

    for dir_path in &opt.dir_paths {
        let mut items: Vec<PathBuf> =
            std::fs::read_dir(dir_path).with_context(
                || anyhow!("can't open dir {dir_path:?} for reading"))?
            .filter_map(
                |entry_result: Result<std::fs::DirEntry, std::io::Error>|
                                      -> Option<Result<PathBuf,
                                                       std::io::Error>>
                {
                    match entry_result {
                        Ok(entry) => {
                            let ft = entry.file_type()
                                .expect("does this fail on OSes needing stat?");
                            if ft.is_file() {
                                Some(Ok(entry.path()))
                            } else {
                                None
                            }
                        }
                        Err(e) =>
                            Some(Err(e))
                    }
                })
            .collect::<Result<_,_>>()?;
        file_paths.append(&mut items);
    }
    file_paths.sort(); // Not ideal, should sort on filenames only.

    let datapoints = parse_files(file_paths)?;
    if opt.show_direct {
        for datapoint in &datapoints {
            println!("{}: {}: {} {}",
                     datapoint.timestamp.to_rfc2822_local(),
                     datapoint.interface,
                     datapoint.transfer.received,
                     datapoint.transfer.sent);
        }
    }
    if let Some(tsv_basepath) = opt.tsv {
        // Go through the values by time, if time difference is <5
        // seconds they belong together. But how do I know all the
        // interfaces? A first scan through them. -- Well, rather
        // split them up anyway and produce a separate TSV for each
        // interface.

        // rust-analyzer can't handle this (rustc can):
        // |datapoint: &Datapoint| -> u64 { datapoint.timestamp.0.0 }
        // so:
        fn timestamp_second(datapoint: &Datapoint) -> u64 {
            datapoint.timestamp.0.0
        }

        let mut outputs = (0..NUM_INTERFACES).map(
            |interfacenumber| -> Result<BufWriter<File>> {
                let iface = WireguardInterface(interfacenumber as u16);
                let path = format!("{tsv_basepath}{iface}.tsv");
                Ok(BufWriter::new(File::create(&path)?))
            })
            .collect::<Result<Vec<_>, _>>()?;

        let timepoints = group(
            datapoints.into_iter(),
            on(timestamp_second, numbers_within(8)),
            |points| Timepoint::from_iter(points.as_mut().unwrap().drain(..))
                .expect("groups are guaranteed to be non-empty, \
                         and we just panic for now if interfaces \
                         are > NUM_INTERFACES"));

        let groups = group(
            timepoints,
            on(|tp: &Timepoint| tp.date_and_hour(),
               |a, b| a == b),
            |pointss| Group(pointss.take().unwrap()));

        for output in &mut outputs {
            writeln!(output,
                     "{}\t({})\t({})\t{}\t{}",
                     "f.timestamp.to_rfc2822_local()",
                     "f.transfer.received",
                     "f.transfer.sent",
                     "transferdiff.received",
                     "transferdiff.sent")?;
        }

        let mut last_group: Option<Group> = None;
        let mut seen: BitSet = Default::default();
        for group in groups {
            let time = group.first_timepoint().timestamp().to_rfc2822_local();

            seen.clear();
            let mut total_all_ifaces = 0; // B
            for (iface, transferdiff) in group.transfer_diffs(last_group.as_ref()) {
                total_all_ifaces += transferdiff.total();
                let i = iface.0 as usize;
                let f = group.first_datapoint(i)
                    .expect("exists because we have a transferdiff");
                let outp = &mut outputs[i];
                write!(outp,
                       "{}\t({})\t({})\t{}\t{}",
                       time,
                       f.transfer.received,
                       f.transfer.sent,
                       transferdiff.received,
                       transferdiff.sent)?;
                seen.insert(i);
            }
            last_group = Some(group);

            for i in &seen {
                let outp = &mut outputs[i];
                writeln!(outp,
                         "",
                         // XX totals and stuff   total_all_ifaces
                )?;
            }
        }
    }

    Ok(())
}
