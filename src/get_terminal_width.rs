//! Hack to get terminal width to allow making older Clap versions
//! auto-adapt to the current width.

pub fn get_terminal_width() -> usize {
    let default = 120;
    if let Some((terminal_size::Width(width), _height)) = terminal_size::terminal_size() {
        usize::from(width).checked_sub(4).unwrap_or(default)
    } else {
        default
    }
}
