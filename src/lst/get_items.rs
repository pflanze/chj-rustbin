use std::{
    ffi::OsStr,
    fmt::Display,
    fs::Metadata,
    marker::PhantomData,
    os::unix::{ffi::OsStrExt, fs::MetadataExt},
    path::Path,
    sync::Mutex,
    time::SystemTime,
};

use anstyle::{AnsiColor, Color, Style};
use anyhow::{anyhow, bail, Context, Result};
use rayon::iter::ParallelIterator;

use crate::{
    bag::Bag,
    efficient_regex::EfficientRegex,
    io::{unix_gr::Gid, unix_pw::Uid},
    io_utils::read_buf::ReadBufStreamError,
    leaked_region::GlobalLeakedRegions,
    path_file_kind::{FileKind, ToFileKind},
    probe,
    time::age_at::AgeAt,
};

fn chomp(v: &mut Vec<u8>, record_separator: u8) {
    if let Some(last) = v.last() {
        if *last == record_separator {
            v.pop();
        }
    }
}

const MIN_OUT_SLICE_LEN: usize = 500000;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnixFileType {
    File = 8,
    Dir = 4,
    Link = 10,
    Socket = 12,
    CharDevice = 2,
    BlockDevice = 6,
    Pipe = 1,
    // Linux uses this value when getting `metadata` for
    // /proc/$pid/task/$tid/fd/$n symlink
    None = 0,
}

impl UnixFileType {
    pub fn is_dir(self) -> bool {
        self == UnixFileType::Dir
    }
    pub fn is_file(self) -> bool {
        self == UnixFileType::File
    }
    pub fn is_link(self) -> bool {
        self == UnixFileType::Link
    }
    /// Char as used by the `ls` command with `-l`
    pub fn type_char(self) -> char {
        match self {
            UnixFileType::File => '-',
            UnixFileType::Dir => 'd',
            UnixFileType::Link => 'l',
            UnixFileType::Socket => 's',
            UnixFileType::CharDevice => 'c',
            UnixFileType::BlockDevice => 'b',
            UnixFileType::Pipe => 'p',
            UnixFileType::None => 'N',
        }
    }
    pub fn has_device_info(self) -> bool {
        match self {
            UnixFileType::File
            | UnixFileType::Dir
            | UnixFileType::Link
            | UnixFileType::Socket
            | UnixFileType::Pipe
            | UnixFileType::None => false,
            UnixFileType::CharDevice | UnixFileType::BlockDevice => true,
        }
    }
}

impl TryFrom<u8> for UnixFileType {
    type Error = anyhow::Error;
    fn try_from(m: u8) -> Result<Self> {
        match m {
            8 => Ok(UnixFileType::File),
            4 => Ok(UnixFileType::Dir),
            10 => Ok(UnixFileType::Link),
            12 => Ok(UnixFileType::Socket),
            2 => Ok(UnixFileType::CharDevice),
            6 => Ok(UnixFileType::BlockDevice),
            1 => Ok(UnixFileType::Pipe),
            0 => Ok(UnixFileType::None),
            _ => bail!("invalid file type number {m}"),
        }
    }
}

pub trait RwxPosition {
    const S_CHAR_SET: char;
    const S_CHAR_UNSET: char;
}

pub struct RwxUser;
impl RwxPosition for RwxUser {
    const S_CHAR_SET: char = 's';
    const S_CHAR_UNSET: char = 'S';
}

pub struct RwxGroup;
impl RwxPosition for RwxGroup {
    const S_CHAR_SET: char = 's';
    const S_CHAR_UNSET: char = 'S';
}

pub struct RwxOther;
impl RwxPosition for RwxOther {
    const S_CHAR_SET: char = 't';
    const S_CHAR_UNSET: char = 'T';
}

/// Contains the r, w, x flags for one of user/group/other (P
/// determines which of those), as well as the setuid/setgid/sticky
/// flag (again depending on P)
#[derive(Debug, PartialEq, Eq)]
pub struct Rwx<P: RwxPosition>(u8, PhantomData<fn() -> P>);

impl<P: RwxPosition> Copy for Rwx<P> {}

impl<P: RwxPosition> Clone for Rwx<P> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<P: RwxPosition> Rwx<P> {
    pub fn s_or_t(self) -> bool {
        ((self.0 >> 3) & 1) > 0
    }
    pub fn r(self) -> bool {
        ((self.0 >> 2) & 1) > 0
    }
    pub fn w(self) -> bool {
        ((self.0 >> 1) & 1) > 0
    }
    pub fn x(self) -> bool {
        ((self.0 >> 0) & 1) > 0
    }
}

// unused?
impl<P: RwxPosition> TryFrom<u8> for Rwx<P> {
    type Error = anyhow::Error;
    fn try_from(m: u8) -> Result<Self> {
        if m < 16 {
            Ok(Self(m, PhantomData))
        } else {
            bail!("Rwx number must be 4 bits, i.e. 0..15")
        }
    }
}

impl<P: RwxPosition> Display for Rwx<P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write;
        f.write_char(if self.r() { 'r' } else { '-' })?;
        f.write_char(if self.w() { 'w' } else { '-' })?;
        let x_char = if self.x() {
            if self.s_or_t() {
                P::S_CHAR_SET
            } else {
                'x'
            }
        } else {
            if self.s_or_t() {
                P::S_CHAR_UNSET
            } else {
                '-'
            }
        };
        f.write_char(x_char)?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Mode(u32);

impl Mode {
    pub fn u(self) -> Rwx<RwxUser> {
        let flags = ((self.0 & 0o7000) >> 9) as u8;
        let flag = ((flags >> 2) & 1) << 3;
        Rwx(((self.0 & 0o0700) >> 6) as u8 | flag, PhantomData)
    }
    pub fn g(self) -> Rwx<RwxGroup> {
        let flags = ((self.0 & 0o7000) >> 9) as u8;
        let flag = ((flags >> 1) & 1) << 3;
        Rwx(((self.0 & 0o0070) >> 3) as u8 | flag, PhantomData)
    }
    pub fn o(self) -> Rwx<RwxOther> {
        let flags = ((self.0 & 0o7000) >> 9) as u8;
        let flag = ((flags >> 0) & 1) << 3;
        Rwx((self.0 & 0o0007) as u8 | flag, PhantomData)
    }
    pub fn s_bits(self) -> u32 {
        (self.0 & 0o7000) >> 9
    }
    pub fn setuid(self) -> bool {
        (self.0 & 0o4000) > 0
    }
    pub fn setgid(self) -> bool {
        (self.0 & 0o2000) > 0
    }
    pub fn sticky(self) -> bool {
        (self.0 & 0o1000) > 0
    }
    pub fn any_x(self) -> bool {
        self.u().x() || self.g().x() || self.o().x()
    }
    pub fn all_w(self) -> bool {
        self.u().w() && self.g().w() && self.o().w()
    }
    pub fn filetype(self) -> UnixFileType {
        (((self.0 & 0o170000) >> 12) as u8)
            .try_into()
            .expect("OS gives valid values")
    }
}

impl From<u32> for Mode {
    fn from(m: u32) -> Self {
        Self(m)
    }
}

impl Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let t = self.filetype();
        let d = t.type_char();
        write!(f, "{d}{}{}{}", self.u(), self.g(), self.o())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RDev(u64);

impl From<u64> for RDev {
    fn from(v: u64) -> Self {
        Self(v)
    }
}

impl RDev {
    /// No idea if it uses anything more than 8 bits? But the original
    /// combined value *is* 64-bit.
    pub fn major(self) -> u64 {
        self.0 >> 8
    }
    pub fn minor(self) -> u8 {
        (self.0 & ((1 << 8) - 1)) as u8
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct EssentialMetadata {
    pub mtime: SystemTime,
    pub size: u64,
    pub mode: Mode,
    pub uid: Uid,
    pub gid: Gid,
    pub nlink: u64,
    pub device: Option<RDev>,
    pub file_kind: Option<FileKind>,
}

impl EssentialMetadata {
    pub fn from_symlink_metadata(
        s: &Metadata,
        file_kind: Option<FileKind>,
    ) -> Result<Self> {
        let mode: Mode = s.mode().into();
        let device = if mode.filetype().has_device_info() {
            Some(s.rdev().into())
        } else {
            None
        };
        Ok(EssentialMetadata {
            mtime: s.modified()?,
            size: s.size(),
            mode,
            uid: s.uid().into(),
            gid: s.gid().into(),
            nlink: s.nlink(),
            device,
            file_kind,
        })
    }

    pub fn style(&self) -> Option<Style> {
        let mode = self.mode;
        match mode.filetype() {
            UnixFileType::None => None,
            UnixFileType::File => {
                if mode.u().s_or_t() {
                    Some(
                        Style::new()
                            .fg_color(Some(Color::Ansi(AnsiColor::White)))
                            .bg_color(Some(Color::Ansi(AnsiColor::Red))),
                    )
                } else if mode.g().s_or_t() {
                    Some(
                        Style::new()
                            .fg_color(Some(Color::Ansi(AnsiColor::Black)))
                            .bg_color(Some(Color::Ansi(AnsiColor::Yellow))),
                    )
                } else if mode.any_x() {
                    Some(
                        Style::new().bold().fg_color(Some(Color::Ansi(
                            AnsiColor::BrightGreen,
                        ))),
                    )
                } else {
                    self.file_kind.map(|k| match k {
                        FileKind::EmacsBackupFile => Style::new().fg_color(
                            Some(Color::Ansi(AnsiColor::BrightBlack)),
                        ),
                        FileKind::VisualMedia => Style::new().bold().fg_color(
                            Some(Color::Ansi(AnsiColor::BrightMagenta)),
                        ),
                        FileKind::Audio => Style::new()
                            .fg_color(Some(Color::Ansi(AnsiColor::Cyan))),
                        FileKind::Archive => Style::new()
                            .bold()
                            .fg_color(Some(Color::Ansi(AnsiColor::Red))),
                    })
                }
            }
            UnixFileType::Dir => Some(if mode.o().w() {
                Style::new()
                    .fg_color(Some(Color::Ansi(AnsiColor::Black)))
                    .bg_color(Some(Color::Ansi(AnsiColor::Green)))
            } else if mode.sticky() {
                Style::new()
                    .fg_color(Some(Color::Ansi(AnsiColor::White)))
                    .bg_color(Some(Color::Ansi(AnsiColor::Blue)))
            } else {
                Style::new()
                    .bold()
                    .fg_color(Some(Color::Ansi(AnsiColor::Blue)))
            }),
            UnixFileType::Link => Some(
                Style::new()
                    .bold()
                    .fg_color(Some(Color::Ansi(AnsiColor::Cyan))),
            ),
            UnixFileType::Socket => Some(
                Style::new()
                    .bold()
                    .fg_color(Some(Color::Ansi(AnsiColor::BrightMagenta))),
            ),
            UnixFileType::CharDevice | UnixFileType::BlockDevice => Some(
                Style::new()
                    .bold()
                    .fg_color(Some(Color::Ansi(AnsiColor::Yellow)))
                    .bg_color(Some(Color::Ansi(AnsiColor::Black))),
            ),
            UnixFileType::Pipe => Some(
                Style::new()
                    .fg_color(Some(Color::Ansi(AnsiColor::Yellow)))
                    .bg_color(Some(Color::Ansi(AnsiColor::Black))),
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Item<'t> {
    pub path: &'t Path,
    pub metadata: EssentialMetadata,
    /// Metadata for the path if there was no error getting it
    pub link_target: Option<(Box<Path>, Option<Box<EssentialMetadata>>)>,
}

#[test]
fn t_sizes() {
    assert_eq!(size_of::<UnixFileType>(), 1);
    // Sec and nsec both taking up 8 B
    assert_eq!(size_of::<SystemTime>(), 16);
    assert_eq!(size_of::<RDev>(), 8);
    assert_eq!(size_of::<Mode>(), 4);
    assert_eq!(size_of::<Uid>(), 4);
    assert_eq!(size_of::<Gid>(), 4);
    assert_eq!(size_of::<FileKind>(), 1);

    assert_eq!(size_of::<EssentialMetadata>(), 64);
    // 16+64+8+64=152, actually 160 when flat; 104 when boxing
    // link_target metadata
    assert_eq!(size_of::<Item>(), 104);
}

impl<'t> Item<'t> {
    pub fn from_path_and_metadata(
        path: &'t Path,
        read_link: bool,
        stat_link_target: bool,
        metadata: Metadata,
    ) -> Result<Option<Self>> {
        let metadata = EssentialMetadata::from_symlink_metadata(
            &metadata,
            path.to_file_kind(),
        )?;
        let link_target = if read_link && metadata.mode.filetype().is_link() {
            match path.read_link() {
                Ok(t) => {
                    let metadata2 = if stat_link_target {
                        path.metadata().ok().and_then(|m| {
                            EssentialMetadata::from_symlink_metadata(
                                &m,
                                t.to_file_kind(),
                            )
                            .ok()
                        })
                    } else {
                        None
                    };
                    Some((t.into(), metadata2.map(Box::new)))
                }
                Err(_) => None,
            }
        } else {
            None
        };
        Ok(Some(Item {
            path,
            metadata,
            link_target,
        }))
    }

    /// Returns None if the path is not found. `read_link` is true,
    /// retrieve the link target path; if `stat_link_target` is
    /// additionally true, try to get the metadata for the target, too
    pub fn from_path(
        path: &'t Path,
        read_link: bool,
        stat_link_target: bool,
    ) -> Result<Option<Self>> {
        match path.symlink_metadata() {
            Ok(metadata) => Self::from_path_and_metadata(
                path,
                read_link,
                stat_link_target,
                metadata,
            ),
            Err(e) => match e.kind() {
                std::io::ErrorKind::NotFound => Ok(None),
                _ => bail!("getting metadata for {path:?}: {e:#}"),
            },
        }
    }

    pub fn mtime(&self) -> SystemTime {
        self.metadata.mtime
    }

    // fn age_secs_at(&self, now: SystemTime) -> Result<u64> {
    //     self.mtime().age_secs_at(now)
    // }

    pub fn age_days_at(&self, now: SystemTime) -> Result<u64> {
        self.mtime().age_days_at(now)
    }
}

pub struct GetItems {
    pub ignore: Option<EfficientRegex>,
    pub long: bool,
    pub use_color: bool,
}

impl GetItems {
    fn ignore_path(&self, path: &Path) -> bool {
        if let Some(ignore) = &self.ignore {
            ignore.is_match_path(path)
        } else {
            false
        }
    }

    /// Leaks the backing memory for Item for now for simplicity
    pub fn get_from_read_buf_stream(
        &self,
        input: impl ParallelIterator<Item = Result<Vec<u8>, ReadBufStreamError>>,
        input_record_separator: u8,
    ) -> (Vec<Item<'static>>, Vec<anyhow::Error>) {
        // To avoid appending individual items multiple times (in
        // multiple reduce layers), collect the original vectors then
        // flatten them in one go at the end. Also, collect errors
        // from all chunks together, too, don't stop early except per
        // chunk.
        let (itemss, errors): (Vec<Vec<Item>>, Vec<anyhow::Error>) = input
            .map(|chunk| -> (Vec<Vec<Item>>, Vec<anyhow::Error>) {
                match (|| -> Result<Vec<Item>> {
                    let mut chunk = chunk?;
                    chomp(&mut chunk, input_record_separator);
                    // XX hack for now, OK for single-shot program
                    let chunk = chunk.leak();
                    let paths = chunk.split(|c| *c == input_record_separator);

                    paths
                        .map(|path| {
                            let path: &OsStr = OsStr::from_bytes(path);
                            let path: &Path = path.as_ref();
                            path
                        })
                        .filter(|path: &&Path| -> bool {
                            !self.ignore_path(path)
                        })
                        .map(|path| -> Result<Option<Item>> {
                            // Only stat linked files if used for
                            // coloring, and only in long format anyway.
                            Item::from_path(path, self.long, self.use_color)
                        })
                        .filter_map(|r| r.transpose())
                        .collect::<Result<Vec<_>>>()
                })() {
                    Ok(v) => (vec![v], vec![]),
                    Err(e) => (vec![], vec![e]),
                }
            })
            .reduce(
                || (Vec::new(), Vec::new()),
                |(mut itemss_a, mut errors_a), (mut itemss_b, mut errors_b)| {
                    itemss_a.append(&mut itemss_b);
                    errors_a.append(&mut errors_b);
                    (itemss_a, errors_a)
                },
            );
        // `into_par_iter` would be a large slow down here!
        (itemss.into_iter().flatten().collect(), errors)
    }

    /// `(Vec<Item<'static>>, Vec<anyhow::Error>)` is what one dir
    /// level yields. Vec of that since subdirs, too.
    fn _find<'p>(
        &self,
        dir: &'p Path,
        include_dir: bool,
        metadata: Metadata,
        global_leaked_regions: &'p GlobalLeakedRegions,
    ) -> (Bag<Item<'p>>, Bag<anyhow::Error>) {
        match (|| -> Result<_> {
            let Self {
                ignore: _,
                long,
                use_color,
            } = self;

            let mut items: Vec<Item<'p>> = Vec::new();
            if include_dir {
                if let Some(item) = Item::from_path_and_metadata(
                    dir, *long, *use_color, metadata,
                )? {
                    items.push(item);
                }
                // else will run into open error anyway--XX hmm
                // actually should accept not found then, there?
            }

            let input = std::fs::read_dir(dir)
                .with_context(|| anyhow!("directory {dir:?}"))?;
            let subdir_items: Mutex<(Bag<Item<'p>>, Bag<anyhow::Error>)> =
                Mutex::new((Bag::new(), Bag::new()));
            let subdir_items_rf = &subdir_items;
            rayon::scope(|scope| -> Result<()> {
                let mut allocator = global_leaked_regions.get_region();
                for entry in input {
                    let entry = entry?;
                    let metadata = entry.metadata()?;
                    let path = entry.path();
                    if self.ignore_path(&path) {
                        continue;
                    }
                    let path = allocator.allocate_path(&path);
                    if metadata.is_dir() {
                        scope.spawn(move |_| {
                            let (items, errors) = self._find(
                                path,
                                true,
                                metadata,
                                global_leaked_regions,
                            );
                            let mut lock =
                                subdir_items_rf.lock().expect("no panics");
                            lock.0.push_bag(items);
                            lock.1.push_bag(errors);
                        });
                    } else {
                        if let Some(item) = Item::from_path_and_metadata(
                            path, *long, *use_color, metadata,
                        )? {
                            items.push(item);
                        }
                    }
                }
                Ok(())
            })
            .with_context(|| anyhow!("reading entries in {dir:?}"))?;
            let mut subdir_items =
                subdir_items.into_inner().expect("no panics");
            subdir_items.0.push_bag(items.into());
            Ok(subdir_items)
        })() {
            Ok(items) => items,
            Err(e) => (Bag::Empty, Bag::Leaf(e)),
        }
    }

    /// Integrated "find", with `read_dir`, path filtering and
    /// `Item::from_path` intertwined for efficiency.
    pub fn find<'p>(
        &self,
        dir: &Path,
        include_top: bool,
        global_leaked_regions: &'p GlobalLeakedRegions,
    ) -> Result<(Vec<Item<'p>>, Vec<anyhow::Error>)> {
        // XX .metadata() ?
        let metadata = dir
            .symlink_metadata()
            .with_context(|| anyhow!("directory {dir:?}"))?;
        let dir = {
            let mut allocator = global_leaked_regions.get_region();
            allocator.allocate_path(dir)
        };
        let (items, errors) =
            self._find(dir, include_top, metadata, global_leaked_regions);
        probe!("flattening");
        let items = items.par_flatten(MIN_OUT_SLICE_LEN);
        let errors = errors.par_flatten(MIN_OUT_SLICE_LEN);
        Ok((items, errors))
    }
}
