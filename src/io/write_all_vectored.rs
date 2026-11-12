use std::io::{IoSlice, Result, Write};

pub fn io_slices_tot_size(sls: &[IoSlice]) -> usize {
    sls.iter().map(|sl| sl.len()).sum()
}

fn continue_after_eintr(res: Result<usize>) -> Result<usize> {
    match res {
        Ok(v) => Ok(v),
        Err(e) => match e.kind() {
            // std::io::ErrorKind::TimedOut
            // std::io::ErrorKind::WriteZero
            // std::io::ErrorKind::InProgress
            // std::io::ErrorKind::WouldBlock
            // std::io::ErrorKind::ResourceBusy
            std::io::ErrorKind::Interrupted => Ok(0),
            _ => Err(e),
        },
    }
}

fn _write_all_vectored<const TESTING: bool, const DEBUG: bool>(
    ioslices: &mut [IoSlice],
    tot_size: usize,
    mut outp: impl Write,
) -> Result<usize> {
    let mut tot_written = 0;
    let mut num_slices_done = 0;
    loop {
        let written = if TESTING {
            use rand::{thread_rng, Rng};
            let mut rng = thread_rng();
            let all_to_write = tot_size.saturating_sub(tot_written);
            if DEBUG {
                dbg!(all_to_write);
            }
            let to_write = rng.gen_range(0..all_to_write + 1);
            if DEBUG {
                dbg!(to_write);
            }
            let mut slv = Vec::new();
            {
                let mut i = num_slices_done;
                let mut written = 0;
                while written < to_write {
                    if DEBUG {
                        dbg!(written);
                    }
                    let need = to_write - written;
                    let sl = &ioslices[i];
                    if need >= sl.len() {
                        slv.push(*sl);
                        written += sl.len();
                        i += 1;
                    } else {
                        slv.push(IoSlice::new(&sl[0..need]));
                        break;
                    }
                }
            }
            let w = continue_after_eintr(outp.write_vectored(&slv))?;
            assert_eq!(w, to_write);
            if DEBUG {
                dbg!(w);
            }
            w
        } else {
            continue_after_eintr(
                outp.write_vectored(&ioslices[num_slices_done..]),
            )?
        };
        tot_written += written;
        if DEBUG {
            dbg!((written, tot_written, tot_size, num_slices_done));
        }
        if tot_written >= tot_size {
            break;
        }
        // Take off `written` bytes from the begin of the still-open window
        let mut to_remove = written;
        while to_remove > 0 {
            if DEBUG {
                dbg!(to_remove);
            }
            let sl = &mut ioslices[num_slices_done];
            let len = sl.len();
            if to_remove < len {
                sl.advance(to_remove);
                break;
            } else {
                num_slices_done += 1;
                to_remove -= len;
            }
        }
    }
    Ok(tot_written)
}

pub fn write_all_vectored(
    ioslices: &mut [IoSlice],
    tot_size: usize,
    outp: impl Write,
) -> std::io::Result<usize> {
    _write_all_vectored::<false, false>(ioslices, tot_size, outp)
}

#[cfg(test)]
mod tests {
    use anyhow::Result;
    use bstr::B;

    use super::*;

    #[test]
    fn t_() -> Result<()> {
        // release mode can do 10000000
        for _ in 0..1000 {
            let w = _write_all_vectored::<true, false>;
            let mut out = [0; 300];
            let mut sls = [
                IoSlice::new(b"Hello"),
                IoSlice::new(b" "),
                IoSlice::new(b"World!"),
                IoSlice::new(b""),
                IoSlice::new(b"?"),
                IoSlice::new(b""),
            ];
            let joined = "Hello World!?";
            let orig_len = io_slices_tot_size(&sls);
            assert_eq!(orig_len, joined.len());
            let written = w(&mut sls, orig_len, out.as_mut_slice())?;
            // assert_eq!(io_slices_tot_size(&sls), 0); -- aha we dont see
            // the num_slices_done, and advance doesn't happen bc in 1 go
            assert_eq!(written, orig_len);
            assert_eq!(&out[0..written], B(joined));
        }
        Ok(())
    }
}
