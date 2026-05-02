use anyhow::{bail, Result};
use enumn::N;

#[derive(N, Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum UnixFileType {
    // XX are these Linux-specific, use C constants?
    Pipe = 1,
    CharDevice = 2,
    Dir = 4,
    BlockDevice = 6,
    File = 8,
    Link = 10,
    Socket = 12,
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
