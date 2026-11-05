use std::{
    ffi::{OsStr, OsString},
    fmt::{Debug, Display},
    fs::{DirEntry, Metadata},
    marker::PhantomData,
    os::unix::{ffi::OsStrExt, fs::MetadataExt},
    path::Path,
    sync::Mutex,
    time::SystemTime,
};

use anstyle::{AnsiColor, Color, Style};
use anyhow::{anyhow, bail, Context, Result};
use log::debug;
use rayon::{iter::ParallelIterator, prelude::ParallelBridge};

pub use crate::io::unix::unix_file_type::UnixFileType;
use crate::{
    bag::Bag,
    efficient_regex::EfficientRegex,
    io::{unix_gr::Gid, unix_pw::Uid},
    io_utils::read_buf::ReadBufStreamError,
    kitschcell::{KitschCache, KitschCell, KitschValue},
    lst::{
        possibly_segmented_path::PossiblySegmentedPath,
        segmented_path::tmp_path_buffer,
    },
    path_file_kind::{FileKind, ToFileKind},
    shared_regions::SharedRegions,
    time::age_at::AgeAt,
    try_chunks::TryChunksOp,
};

fn chomp(v: &mut Vec<u8>, record_separator: u8) {
    if let Some(last) = v.last() {
        if *last == record_separator {
            v.pop();
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
    #[inline]
    pub fn file_type(self) -> UnixFileType {
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
        let t = self.file_type();
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
        let device = if mode.file_type().has_device_info() {
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
        match mode.file_type() {
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

    #[inline]
    pub fn file_type(&self) -> UnixFileType {
        self.mode.file_type()
    }
}

// Need PartialEq, Eq for tests
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Item<'region, P: PossiblySegmentedPath<'region, INLINE>, INLINE> {
    pub path: P,
    pub metadata: EssentialMetadata,
    /// Metadata for the path if there was no error getting it
    pub link_target: Option<(Box<Path>, Option<Box<EssentialMetadata>>)>,
    // XX wanted to keep this field private to make it impossible to
    // create?
    pub _phantom: PhantomData<fn() -> &'region INLINE>,
}

#[test]
fn t_sizes() {
    use std::mem::size_of;

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
    assert_eq!(size_of::<Item<&Path, ()>>(), 104);
}

impl<'region, P: PossiblySegmentedPath<'region, INLINE>, INLINE>
    Item<'region, P, INLINE>
{
    /// `get_path` is only evaluated when `metadata.is_link()`
    ///
    /// XX: when does this fail with an error? And when does it return None?
    fn from_path_and_metadata<'t, 'p, 'tmp: 'p>(
        path: P,
        get_path: &'p mut impl KitschCache<&'tmp Path>,
        file_kind: Option<FileKind>,
        read_link: bool,
        stat_link_target: bool,
        metadata: &Metadata,
    ) -> Result<Option<Self>> {
        let metadata =
            EssentialMetadata::from_symlink_metadata(&metadata, file_kind)?;
        let link_target = if read_link && metadata.mode.file_type().is_link() {
            let _path = get_path.get();
            match _path.read_link() {
                Ok(t) => {
                    let metadata2 = if stat_link_target {
                        _path.metadata().ok().and_then(|m| {
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
            _phantom: PhantomData,
        }))
    }

    /// Returns None if the path is not found. `read_link` is true,
    /// retrieve the link target path; if `stat_link_target` is
    /// additionally true, try to get the metadata for the target, too
    pub fn from_path(
        path: P,
        _path: &Path,
        read_link: bool,
        stat_link_target: bool,
    ) -> Result<Option<Self>> {
        match _path.symlink_metadata() {
            Ok(metadata) => {
                let mut get_path = KitschValue(_path);
                Self::from_path_and_metadata(
                    path,
                    &mut get_path,
                    _path.to_file_kind(),
                    read_link,
                    stat_link_target,
                    &metadata,
                )
            }
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

pub struct GetItemsIgnore<INLINE> {
    pub ignore_path_regex: Option<EfficientRegex>,
    pub ignore_item_names: Vec<OsString>,
    pub ignore_item_regex: Option<EfficientRegex>,
    pub _phantom: PhantomData<INLINE>,
}

impl<INLINE: Sync> GetItemsIgnore<INLINE> {
    fn ignore_path<'s, 'p, 'tmp: 'p, 'o>(
        &'s self,
        file_name: Option<&'o OsStr>,
        get_path: &'p mut impl KitschCache<&'tmp Path>,
    ) -> bool {
        let Self {
            ignore_path_regex,
            ignore_item_names,
            ignore_item_regex,
            _phantom,
        } = self;

        if let Some(file_name) = file_name {
            for ignore_item_name in ignore_item_names {
                if ignore_item_name == &file_name {
                    return true;
                }
            }

            if let Some(ignore) = ignore_item_regex {
                if ignore.is_match_file_name(&file_name) {
                    return true;
                }
            }
        } else {
            // XX ? should only happen for "" paths anyway, right?
            return true;
        }

        if let Some(ignore) = ignore_path_regex {
            let path = get_path.get();
            if ignore.is_match_path(path) {
                return true;
            }
        }

        false
    }
}

#[derive(Clone, Copy)]
pub struct GetItemsInfo<INLINE: Copy> {
    pub long: bool,
    pub use_color: bool,
    pub _phantom: PhantomData<INLINE>,
}

impl<INLINE: Copy> GetItemsInfo<INLINE> {
    /// Convert a directory entry to an Item (None if not
    /// found, I think? XX Check `Item::from_path_and_metadata`)
    ///
    /// If `entry` is provided, get the metadata via that entry, otherwise via
    /// `opt_sub_path.get()`.
    ///
    /// `entry`, `file_name`, `sub_p`, `opt_sub_path` all represent the same
    /// entry from various viewpoints / efficiencies. `opt_file_name` should not
    /// be given for directories, and is convenient not to be given for the
    /// toplevel directory (because that may be ".").
    ///
    /// Also returns whether the item is a directory.
    fn item_from_entry<'region, 'p, P>(
        self,
        entry: Option<&DirEntry>,
        opt_file_name: Option<&OsStr>,
        sub_p: P,
        opt_sub_path: &mut impl KitschCache<&'p Path>,
    ) -> Result<Option<(Item<'region, P, INLINE>, bool)>>
    where
        P: PossiblySegmentedPath<'region, INLINE> + Send + Copy,
    {
        let Self {
            long,
            use_color,
            _phantom,
        } = self;
        let metadata = if let Some(entry) = entry {
            entry.metadata().with_context(|| {
                anyhow!("getting metadata for {:?}", opt_sub_path.get())
            })?
        } else {
            let dir_path = opt_sub_path.get();
            dir_path
                .metadata()
                .with_context(|| anyhow!("getting metadata for {dir_path:?}"))?
        };

        if let Some(item) = Item::from_path_and_metadata(
            sub_p,
            opt_sub_path,
            opt_file_name.and_then(|file_name| file_name.to_file_kind()),
            long,
            use_color,
            &metadata,
        )
        .with_context(|| {
            anyhow!("constructing Item for {:?}", opt_sub_path.get())
        })? {
            Ok(Some((item, metadata.is_dir())))
        } else {
            Ok(None)
        }
    }
}

pub struct GetItems<INLINE: Copy> {
    pub ignore: GetItemsIgnore<INLINE>,
    pub info: GetItemsInfo<INLINE>,
    pub _phantom: PhantomData<INLINE>,
}

impl<INLINE: Copy + Sync> GetItems<INLINE> {
    /// Get the items from a directory (the first level only)
    ///
    /// Readdir serially (since the OS doesn't support anything else anyway,
    /// except perhaps via `io_uring`?), then stat in parallel.
    pub fn ls<
        'region,
        P: PossiblySegmentedPath<'region, INLINE> + Send + Copy,
    >(
        &self,
        dir: P,
        include_dir: bool,
        shared_regions: &'region SharedRegions,
    ) -> Result<(Bag<Item<'region, P, INLINE>>, Bag<anyhow::Error>)> {
        let Self {
            ignore,
            info,
            _phantom,
        } = self;

        let mut tmp = tmp_path_buffer();
        let dir_path = dir.psp_to_path(None, &mut tmp);
        let dir_handle = std::fs::read_dir(dir_path)
            .with_context(|| anyhow!("reading directory {dir_path:?}"))?;

        let opt_base_item = if include_dir {
            // Do not check `ignore`, right?
            let mut opt_sub_path = KitschValue(dir_path);
            let item = info
                .item_from_entry(None, None, dir, &mut opt_sub_path)
                .with_context(|| {
                    anyhow!("constructing Item for {:?}", dir_path)
                })?
                .ok_or_else(|| {
                    anyhow!(
                        "top-level item was not found for \
                     further info? Is this possible?"
                    )
                })?;
            Some(item)
        } else {
            None
        };

        let (items, errors): (
            Bag<Item<'region, P, INLINE>>,
            Bag<anyhow::Error>,
        ) = dir_handle
            .try_chunks(1000)
            .par_bridge()
            .map(
                |chunk| -> (Bag<Item<'region, P, INLINE>>, Bag<anyhow::Error>) {
                    let mut allocator = shared_regions.get_region();
                    let mut tmp = tmp_path_buffer();

                    let mut errors: Vec<anyhow::Error> = Vec::new();
                    let mut items: Vec<Item<'region, P, INLINE>> = Vec::new();
                    let items_ref = &mut items;

                    'entry: for entry in chunk {
                        let entry = match entry.with_context(|| {
                            anyhow!("reading from directory {dir_path:?}")
                        }) {
                            Ok(v) => v,
                            Err(e) => {
                                errors.push(e);
                                break;
                            }
                        };

                        // XX get from libc instead to avoid allocation
                        let file_name = entry.file_name();
                        let sub_p =
                            dir.psp_add_segment(&file_name, &mut allocator);
                        let mut opt_sub_path = KitschCell::Unevaluated(|| {
                            sub_p.psp_to_path(None, &mut tmp)
                        });

                        if ignore
                            .ignore_path(Some(&file_name), &mut opt_sub_path)
                        {
                            continue 'entry;
                        }

                        match info.item_from_entry(
                            Some(&entry),
                            Some(&file_name),
                            sub_p,
                            &mut opt_sub_path,
                        ) {
                            Ok(Some((item, _))) => {
                                items_ref.push(item);
                            }
                            Ok(None) => (),
                            Err(e) => {
                                errors.push(e);
                            }
                        }
                    }

                    (items.into(), errors.into())
                },
            )
            .collect();

        let items = if let Some((base_item, _)) = opt_base_item {
            items.add(base_item)
        } else {
            items
        };
        Ok((items, errors))
    }

    /// Read chunks of buffers containing paths separated by
    /// `input_record_separator` into a Bag of items in parallel (each chunk
    /// being a work unit)
    ///
    /// Leaks the backing memory for Item for now for simplicity.
    pub fn get_from_read_buf_stream(
        &self,
        input: impl ParallelIterator<Item = Result<Vec<u8>, ReadBufStreamError>>,
        input_record_separator: u8,
    ) -> (
        Bag<Item<'static, &'static Path, INLINE>>,
        Bag<anyhow::Error>,
    ) {
        // To avoid appending individual items multiple times (in
        // multiple reduce layers), collect the original vectors then
        // flatten them in one go at the end. Also, collect errors
        // from all chunks together, too, don't stop early except per
        // chunk.
        let (itemss, errors): (Bag<Item<_, _>>, Bag<anyhow::Error>) = input
            .map(|chunk| -> (Bag<Item<_, _>>, Bag<anyhow::Error>) {
                match (|| -> Result<Vec<Item<_, _>>> {
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
                            let mut get_path = KitschValue(*path);
                            !self
                                .ignore
                                .ignore_path(path.file_name(), &mut get_path)
                        })
                        .map(|path| -> Result<Option<Item<_, _>>> {
                            let GetItemsInfo {
                                long,
                                use_color,
                                _phantom,
                            } = self.info;
                            // Only stat linked files if used for
                            // coloring, and only in long format anyway.
                            Item::from_path(path, path, long, use_color)
                        })
                        .filter_map(|r| r.transpose())
                        .collect::<Result<Vec<_>>>()
                })() {
                    Ok(v) => (Bag::LeafVec(v), Bag::Empty),
                    Err(e) => (Bag::Empty, Bag::Leaf(e)),
                }
            })
            .reduce(
                || (Bag::Empty, Bag::Empty),
                |(itemss_a, errors_a), (itemss_b, errors_b)| {
                    (itemss_a.add_bag(itemss_b), errors_a.add_bag(errors_b))
                },
            );
        (itemss, errors)
    }

    /// `(Vec<Item<'static>>, Vec<anyhow::Error>)` is what one dir
    /// level yields. Vec of that since subdirs, too.
    ///
    /// `include_dir`: whether to include the item at `dir` in the result,
    /// too. (Always true on self-recursive calls.)
    fn _find<
        's,
        'region,
        P: PossiblySegmentedPath<'region, INLINE> + Send + Copy,
    >(
        &'s self,
        dir: P,
        shared_regions: &'region SharedRegions,
    ) -> (Bag<Item<'region, P, INLINE>>, Bag<anyhow::Error>) {
        match (move || -> Result<_> {
            let Self {
                ignore,
                info,
                _phantom,
            } = self;

            let mut tmp = tmp_path_buffer();

            let mut items: Vec<Item<'region, P, INLINE>> = Vec::new();
            let items_ref = &mut items;
            let dir_path: &Path = dir.psp_to_path(None, &mut tmp);
            debug!("dir_path = {dir_path:?}");

            let input = std::fs::read_dir(dir_path)
                .with_context(|| anyhow!("reading directory {dir_path:?}"))?;
            let subdir_items: Mutex<(Bag<Item<_, _>>, Bag<anyhow::Error>)> =
                Mutex::new((Bag::new(), Bag::new()));
            let subdir_items_rf = &subdir_items;
            rayon::scope(move |scope| -> Result<()> {
                let mut allocator = shared_regions.get_region();
                'entry: for entry in input {
                    let entry: std::fs::DirEntry = entry?;
                    // XX get from libc instead to avoid allocation
                    let file_name = entry.file_name();
                    let sub_p = dir.psp_add_segment(&file_name, &mut allocator);

                    // Only need &Path if a path regex was given or
                    // the item is a symlink (in that latter case,
                    // could also change to dir-fd based POSIX
                    // functions).
                    let mut opt_sub_path = KitschCell::Unevaluated(|| {
                        sub_p.psp_to_path(None, &mut tmp)
                    });

                    if ignore.ignore_path(Some(&file_name), &mut opt_sub_path) {
                        continue 'entry;
                    }

                    if let Some((item, is_dir)) = info.item_from_entry(
                        Some(&entry),
                        Some(&file_name),
                        sub_p,
                        &mut opt_sub_path,
                    )? {
                        items_ref.push(item);
                        if is_dir {
                            drop(file_name);
                            scope.spawn(move |_| {
                                let (items, errors) =
                                    self._find(sub_p, shared_regions);
                                let mut lock =
                                    subdir_items_rf.lock().expect("no panics");
                                lock.0.push_bag(items);
                                lock.1.push_bag(errors);
                            });
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
    pub fn find<
        'region,
        P: PossiblySegmentedPath<'region, INLINE> + Send + Copy,
    >(
        &self,
        dir: P,
        include_top: bool,
        shared_regions: &'region SharedRegions,
    ) -> Result<(Bag<Item<'region, P, INLINE>>, Bag<anyhow::Error>)> {
        let mut tmp = tmp_path_buffer();
        let dir_path = dir.psp_to_path(None, &mut tmp);

        // Check the top item first, to avoid doing the rest if this gives
        // errors
        let top_item = if include_top {
            let mut opt_dir_path = KitschValue(dir_path);
            let (item, _is_dir) = self
                .info
                .item_from_entry(None, None, dir, &mut opt_dir_path)
                .with_context(|| {
                    anyhow!("constructing Item for {:?}", dir_path)
                })?
                .ok_or_else(|| {
                    anyhow!(
                        "top-level item was not found for \
                     further info? Is this possible?"
                    )
                })?;
            // ^ else (None case) will run into open error anyway--XX hmm
            // actually should accept not found then, there?  But then, make
            // sure to track that.
            Some(item)
        } else {
            None
        };

        let (mut items, errors) = self._find(dir, shared_regions);
        if let Some(v) = top_item {
            items.push(v);
        }
        Ok((items, errors))
    }
}
