use enumn::N;

#[derive(N, Eq, PartialEq, Debug)]
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
}
