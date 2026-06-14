use std::{os::linux::fs::MetadataExt, path::Path, time::SystemTime};

/// The details about a file system item (only directories and normal
/// files; symlinks are followed, and special files won' work with
/// that) that should clearly tell whether the content is still the
/// same, when talking about the same host. The goal is to be usable
/// as a cache key (while staying on the same host).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnixFileContentKey {
    pub device: u64,
    pub inode: u64,
    pub size: u64,
    pub mtime: SystemTime,
    pub ctime: i64,
    pub ctime_nsec: i64,
    // Do not include uid, gid or mode, since those say nothing about
    // the contents, and do not include nlink, as that says nothing
    // about non-directory files, and while for directories it is a
    // content thing, the content change will also be reflected in the
    // other numbers.
}

impl UnixFileContentKey {
    /// Follows symlinks. Does not give an error for filesystem items
    /// other than files and directories, even though for other item
    /// types it does not make sense.
    pub fn from_path_metadata(path: &Path) -> Result<Self, std::io::Error> {
        let m = path.metadata()?;
        let device = m.st_dev();
        let inode = m.st_ino();
        let size = m.st_size();
        let mtime = m.modified()?;
        let ctime = m.st_ctime();
        let ctime_nsec = m.st_ctime_nsec();
        Ok(UnixFileContentKey {
            device,
            inode,
            size,
            mtime,
            ctime,
            ctime_nsec,
        })
    }
}
