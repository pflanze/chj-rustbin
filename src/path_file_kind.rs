use std::{os::unix::prelude::OsStrExt, path::Path};

pub trait ToFileKind {
    fn to_file_kind(&self) -> Option<FileKind>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FileKind {
    /// File name ending in `~`
    EmacsBackupFile,
    /// File extension `png`, `jpg`, `webm`, `mov`, etc. (but not
    /// audio)
    VisualMedia,
    /// `mp3`, `flac`, etc.
    Audio,
    /// `tar`, `gz`, etc.
    Archive,
}

impl ToFileKind for Path {
    fn to_file_kind(&self) -> Option<FileKind> {
        if let Some(file_name) = self.file_name() {
            if let Some(s) = file_name.to_str() {
                if s.chars().last().map(|c| c == '~').unwrap_or(false) {
                    return Some(FileKind::EmacsBackupFile);
                }
            } else {
                let b = file_name.as_bytes();
                if b.last().map(|c| *c == b'~').unwrap_or(false) {
                    return Some(FileKind::EmacsBackupFile);
                }
            }
        }

        if let Some(ext) = self.extension() {
            if let Some(ext) = ext.to_str() {
                let tmp;
                let ext_lc = if ext.chars().all(|c| c.is_lowercase()) {
                    ext
                } else {
                    tmp = ext.to_lowercase();
                    &tmp
                };
                match ext_lc {
                    "png" | "jpg" | "jpeg" | "gif" | "webp" | "webm"
                    | "tiff" | "mov" => return Some(FileKind::VisualMedia),
                    "mp3" | "wav" | "flac" | "aac" | "ogg" => {
                        return Some(FileKind::Audio)
                    }
                    "gz" | "tar" | "bz2" | "lzo" | "xz" | "zstd" | "zst"
                    | "zip" | "rz" => return Some(FileKind::Archive),
                    _ => (),
                }
            }
        }

        None
    }
}
