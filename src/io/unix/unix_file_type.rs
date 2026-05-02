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

pub type UnixFileTypeMask = u8;

impl UnixFileType {
    /// As an integer where exactly one bit is set, to use standard
    /// bit operators on.
    #[inline]
    pub fn as_mask(self) -> UnixFileTypeMask {
        let m: UnixFileTypeMask = match self {
            UnixFileType::Pipe => 1,
            UnixFileType::CharDevice => 2,
            UnixFileType::Dir => 4,
            UnixFileType::BlockDevice => 8,
            UnixFileType::File => 16,
            UnixFileType::Link => 32,
            UnixFileType::Socket => 64,
            UnixFileType::None => 128,
        };
        debug_assert_eq!(m.count_ones(), 1);
        m
    }

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

    /// Convert `type_char` back into a UnixFileType; also accepts 'f'
    /// as alias for '-' (normal file).
    pub fn from_type_char(c: char) -> Option<Self> {
        match c {
            '-' | 'f' => Some(UnixFileType::File),
            'd' => Some(UnixFileType::Dir),
            'l' => Some(UnixFileType::Link),
            's' => Some(UnixFileType::Socket),
            'c' => Some(UnixFileType::CharDevice),
            'b' => Some(UnixFileType::BlockDevice),
            'p' => Some(UnixFileType::Pipe),
            'N' => Some(UnixFileType::None),
            _ => None,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_mask() {
        // Those are *not* related to the position in the mask!
        assert_eq!(UnixFileType::Pipe as u8, 1);
        assert_eq!(UnixFileType::Dir as u8, 4);

        assert_eq!(UnixFileType::Pipe.as_mask(), 1);
        assert_eq!(UnixFileType::Dir.as_mask(), 4);
        assert_eq!(UnixFileType::File.as_mask(), 16);
        assert_eq!(UnixFileType::None.as_mask(), 128);

        let mut count = 0;
        for i in 0..=255 {
            if let Some(t) = UnixFileType::n(i) {
                count += 1;
                // just force the debug_assert inside it:
                t.as_mask();
            }
        }
        assert_eq!(count, 8);
    }
}
