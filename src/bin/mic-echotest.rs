use anyhow::{Context, Result}; 

use alsa::card::Iter;
use alsa::ValueOr;
use alsa::pcm::{PCM, HwParams, Format, Access, State};

use std::f64::consts::PI;

const SAMPLES_PER_BUF : usize = 1024; // per channel

fn set_start_threshold(pcm: &PCM) -> Result<()> {
    // Make sure we don't start the stream too early
    let hwp = pcm.hw_params_current()?;
    let swp = pcm.sw_params_current()?;
    // This prints 262144, and would lead to a 5-6 second delay:
    println!("buffer size would be: {:?}", hwp.get_buffer_size()?);
    // Ending up just setting something that "works for me"
    // empirically. This only works for certain sizes of
    // SAMPLES_PER_BUF, < 1024 is not working anymore.
    swp.set_start_threshold((SAMPLES_PER_BUF * 2) as i64)?;
    pcm.sw_params(&swp)?;
    Ok(())
}


pub fn main() -> Result<()> {
    for i in Iter::new() {
        let i = i?;
        println!("Getting i={:?}", i);
    }
    let inp = (|| -> Result<_, anyhow::Error> {
        let inp = PCM::new("default", alsa::Direction::Capture, false)?;
        {
            let p = HwParams::any(&inp)?;
            p.set_channels(1)?;
            p.set_rate(48000, ValueOr::Nearest)?;
            p.set_format(Format::s16())?;
            p.set_access(Access::RWInterleaved)?; //?
            inp.hw_params(&p)?;
        }
        Ok(inp)
    })().with_context(|| "preparing audio input")?;

    let outp = (|| -> Result<_, anyhow::Error> {
        let outp = PCM::new("plughw:CARD=Device,DEV=0", alsa::Direction::Playback, false)?;
        {
            let p = HwParams::any(&outp)?;
            p.set_channels(2)?; // 1 doesn't work!
            p.set_rate(48000, ValueOr::Nearest)?;
            p.set_format(Format::s16())?;
            p.set_access(Access::RWInterleaved)?;
            outp.hw_params(&p)?;
        }
        Ok(outp)
    })().with_context(|| "preparing audio output")?;

    // set_start_threshold(&inp)?; //  ?
    set_start_threshold(&outp)?;

    let i = inp.io_i16()?;
    let o = outp.io_i16()?;

    let mut inbuf = [0i16; SAMPLES_PER_BUF];
    let mut outbuf = [0i16; SAMPLES_PER_BUF * 2];
    let mut t : f64 = 0.0;
    let d = 2.0 * PI;

    // Play back for n seconds.
    let n = 600;
    for _ in 0..n*48000/SAMPLES_PER_BUF {

        assert_eq!(i.readi(&mut inbuf)?, SAMPLES_PER_BUF);
        
        for i in 0..SAMPLES_PER_BUF {
            // left
            outbuf[i*2 + 1] = ((
                ((t / 200.000123 + (t / 301000.438).sin() * 400.).sin()
                 * (t / 200.0).sin()
                ) * 1000.0
            ) as i16 + inbuf[i]) / 2;

            // right
            outbuf[i*2] = ((
                ((t / 201.000123 + (t / 300000.).sin() * 400.).sin()
                 * (t / 201.0).sin()
                ) * 1000.0
            ) as i16 + inbuf[i]) / 2;

            t = t + d;
        }

        assert_eq!(o.writei(&outbuf)?, SAMPLES_PER_BUF);
    }

    // In case the buffer was larger than the playing duration, start the stream manually.
    if outp.state() != State::Running { outp.start()? };
    // Wait for the stream to finish playback.
    outp.drain()?;

    Ok(())
}
