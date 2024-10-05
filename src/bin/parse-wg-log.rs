use chj_rustbin::util::div::{hashmap_add, hashmap_get_mut_vivify};
use chrono::{Timelike, NaiveDate, Datelike};
use clap::Parser;
use genawaiter::rc::Gen;
use tai64::Tai64N;
use std::collections::HashMap;
use std::ops::Add;
use std::{path::PathBuf, fmt::Display, fs::File, io::BufWriter};
use std::io::Write;
use anyhow::{Result, bail, anyhow, Context};

use chj_rustbin::gen_try_result;
use chj_rustbin::numbers::{numbers_within, max_f64, nandropping_add};
use chj_rustbin::sequences::try_group;
use chj_rustbin::{io::readwithcontext::ReadWithContext,
                  parseutil::{cleanwhite, parse_byte_multiplier, is_all_white,
                              key_val, after_white},
                  time::tai::{parse_timestamp, Tai64Format},
                  fp::on};


#[derive(clap::Parser, Debug)]
/// Parse a log file consisting of repeated output of `wg` (wireguard
/// command line tool), with tai64n timestamps prepended to each line
/// (DJB daemontools log format).

#[clap(name = "parse-wg-log from chj-rustbin")]
struct Opt {
    /// Show parsed data directly
    #[clap(long)]
    show_direct: bool,

    /// Calculate derived values and save as TSV files, one for each
    /// interface. The option specifies the base path, to which
    /// `$interfacename.tsv` is appended for the hourly tables, and
    /// `$interfacename-summary.tsv` is appended for the monthly
    /// summary tables.
    #[clap(long)]
    tsv: Option<String>,

    /// The paths to dirs with files to parse
    #[clap(parse(from_os_str))]
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
struct YearMonth {
    year: i32,
    month: u8,
}
impl YearMonth {
    fn from_naivedate(nd: NaiveDate) -> YearMonth {
        YearMonth {
            year: nd.year(),
            month: nd.month() as u8,
        }
    }
}
impl Display for YearMonth {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}/{:02}", self.year, self.month))
    }
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
) -> impl Iterator<Item = Result<Datapoint>>
{
    Gen::new(|co| async move {
        let mut line = String::new();
        let mut current_interface: Option<WireguardInterface> = None;
        let mut current_peer: Option<UnfinishedPeer> = None;
        let mut num_errors = 0;
        for file in files {
            let mut inp = gen_try_result!(ReadWithContext::open_path(&file), co);

            while gen_try_result!(inp.easy_read_line(&mut line), co) {
                let res = (|current_interface: &mut Option<WireguardInterface>|
                                                           -> Result<Option<Datapoint>> {
                    let (timestamp, rest) = inp.context(parse_timestamp(&line))?;
                    if is_all_white(rest) {
                        return Ok(None);
                    }
                    if let Some((indentkey, val)) = key_val(rest) {
                        let val = cleanwhite(val);
                        if indentkey == "interface" {
                            if current_interface.is_some() {
                                inp.err_with_context(anyhow!(
                                    "missed \"peer\" before another \
                                     \"interface\""))?
                            }
                            *current_interface =
                                Some(WireguardInterface::from_str(val)?);
                            Ok(None)
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
                                    "missed \"peer\" before another \
                                     \"interface\""))?
                            }
                            Ok(None)
                        } else if let Some(key) = after_white(indentkey) {
                            if key == "public key" {
                                Ok(None)
                            } else if key == "private key" {
                                Ok(None)
                            } else if key == "listening port" {
                                Ok(None)
                            } else if key == "endpoint" {
                                Ok(None)
                            } else if key == "allowed ips" {
                                Ok(None)
                            } else if key == "latest handshake" {
                                Ok(None)
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
                                    Ok(Some(dp))
                                } else {
                                    inp.err_with_context(anyhow!(
                                        "missing peer before key {key:?}"))
                                }
                            } else {
                                inp.err_with_context(anyhow!(
                                    "unknown indented key {key:?}"))
                            }
                        } else {
                            inp.err_with_context(anyhow!(
                                "unknown key {indentkey:?}"))
                        }
                    } else {
                        inp.err_with_context(anyhow!(
                            "line does not match `key: val` pattern"))
                    }
                })(&mut current_interface);
                match res {
                    Ok(None) => {},
                    Ok(Some(v)) => co.yield_(Ok(v)).await,
                    Err(e) =>
                        if num_errors < MAX_ERRORS {
                            num_errors += 1;
                            eprintln!("Warning: {e:?}");
                        } else {
                            //return Err(e)
                            //  Is there a way to give an endless error?
                            co.yield_(Err(e)).await
                        }
                }
            }
        }
        ()
    }).into_iter()
}

#[derive(Copy, Clone, Debug)]
struct BilledCost {
    billed_cost: f64,
    your_cost: f64
}
impl Add for BilledCost {
    type Output = BilledCost;

    fn add(self, rhs: Self) -> Self::Output {
        // evil to drop NaN?
        let billed_cost = nandropping_add(self.billed_cost, rhs.billed_cost);
        let your_cost = nandropping_add(self.your_cost, rhs.your_cost);
        BilledCost { billed_cost, your_cost }
    }
}


struct RowShared {
    time: Tai64N,
    total_all_ifaces_hour: usize,
    num_servers_running: u32,
}
struct RowUser {
    received_cum: usize,
    sent_cum: usize,
    received_hour: usize,
    sent_hour: usize,
}

struct Row<'a> {
    shared: &'a RowShared,
    user: &'a RowUser,
}
impl<'a> Row<'a> {
    fn write_header(outp: &mut impl Write) -> Result<(), std::io::Error> {
        writeln!(outp,
                 "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
                 "time window start",
                 "time excel",
                 "received B",
                 "sent B",
                 "received B/hour",
                 "sent B/hour",
                 "total B/hour",
                 "all interfaces B/hour",
                 "fraction of all traffic",
                 "num servers running",
                 "free traffic B/hour",
                 "billed traffic B",
                 "billed cost EUR",
                 "your cost EUR"
        )
    }
    fn write(&self, outp: &mut impl Write) -> Result<BilledCost, std::io::Error> {
        let total = self.user.received_hour + self.user.sent_hour;
        let part =
            total as f64
            / (self.shared.total_all_ifaces_hour as f64);
        let included_traffic = self.shared.num_servers_running as f64 * 1.42e9;
        let billed_traffic =
            max_f64(0.,
                    self.shared.total_all_ifaces_hour as f64
                    - included_traffic);
        let billed_cost = billed_traffic * (0.02000000 / 1e9);
        let your_cost = part * billed_cost;

        writeln!(outp,
                 "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
                 self.shared.time.to_rfc2822_local(),
                 // XX Hard coding +01:00 for central europe, since
                 // daylight savings time is the fake one, thus this
                 // is closest without introducing discontinuities
                 // (because this is easier than switching for DST,
                 // introducing wrong time points while at it, and
                 // discontinuities which might matter e.g. for
                 // plots):
                 self.shared.time.to_exceldays(1.),
                 self.user.received_cum,
                 self.user.sent_cum,
                 self.user.received_hour,
                 self.user.sent_hour,
                 total,
                 self.shared.total_all_ifaces_hour,
                 part, // * 100. ? use Excel formatting for that
                 self.shared.num_servers_running,
                 included_traffic,
                 billed_traffic,
                 billed_cost,
                 your_cost
        )?;

        // Hack: return calculated values for summary
        Ok(BilledCost { billed_cost, your_cost })
    }
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

    let datapoints = parse_files(file_paths);
    if opt.show_direct {
        for datapoint in datapoints {
            let datapoint = datapoint?;
            println!("{}: {}: {} {}",
                     datapoint.timestamp.to_rfc2822_local(),
                     datapoint.interface,
                     datapoint.transfer.received,
                     datapoint.transfer.sent);
        }
        return Ok(())
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

        let timepoints = try_group(
            datapoints,
            on(timestamp_second, numbers_within(8)),
            |points| Timepoint::from_iter(points.as_mut().unwrap().drain(..))
                .expect("groups are guaranteed to be non-empty, \
                         and we just panic for now if interfaces \
                         are > NUM_INTERFACES"));

        let groups = try_group(
            timepoints,
            on(|tp: &Timepoint| tp.date_and_hour(),
               |a, b| a == b),
            |pointss| Group(pointss.take().unwrap()));

        for output in &mut outputs {
            Row::write_header(output)?;
        }

        let mut by_user_month: HashMap<u16, HashMap<YearMonth, BilledCost>>
            = Default::default();

        let mut last_group: Option<Group> = None;
        let mut rows: HashMap<u16, RowUser> = Default::default();
        for group in groups {
            let group = group?;

            rows.clear();
            let mut total_all_ifaces_hour = 0; // B
            for (iface, transferdiff) in group.transfer_diffs(last_group.as_ref()) {
                total_all_ifaces_hour += transferdiff.total();
                let i = iface.0 as usize;
                let f = group.first_datapoint(i)
                    .expect("exists because we have a transferdiff");
                let row = RowUser {
                    received_cum: f.transfer.received,
                    sent_cum: f.transfer.sent,
                    received_hour: transferdiff.received,
                    sent_hour: transferdiff.sent,
                };
                rows.insert(iface.0, row);
            }

            let num_servers_running = 3; // configure XX
            let shared = RowShared {
                time: group.first_timepoint().timestamp().clone(),
                total_all_ifaces_hour,
                num_servers_running
            };
            let ym = YearMonth::from_naivedate(
                shared.time.to_datetime_utc().date_naive());
            for (i, user) in &mut rows {
                let outp = &mut outputs[*i as usize];
                let row = Row {
                    shared: &shared,
                    user
                };
                let calculated = row.write(outp)?;
                hashmap_add(
                    hashmap_get_mut_vivify(
                        &mut by_user_month,
                        i,
                        || HashMap::new()),
                    ym,
                    calculated);
            }

            last_group = Some(group);
        }

        for (i, by_month) in &by_user_month {
            let mut summary: Vec<_> = by_month.iter().collect();
            summary.sort_by(|a, b| (*a).0.cmp(b.0));
            let iface = WireguardInterface(*i);
            let mut outp = BufWriter::new(File::create(format!(
                "{tsv_basepath}{iface}-summary.tsv"))?);
            writeln!(&mut outp, "year/month\tbilled cost EUR\tyour cost EUR")?;
            for (month, cost) in summary {
                writeln!(&mut outp, "{month}\t{:.2}\t{:.2}",
                         cost.billed_cost, cost.your_cost)?;
            }
        }

        return Ok(())
    }
    Ok(())
}
