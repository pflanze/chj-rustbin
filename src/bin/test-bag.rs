use anyhow::Result;
use arbitrary::{Arbitrary, Unstructured};
use clap::Parser;
use getrandom::getrandom;

use chj_rustbin::{bag::Bag, cpu_probe, probe};

#[derive(clap::Parser, Debug)]
/// Test Bag.rs
#[clap(name = "test-bag from chj-rustbin")]
struct Opt {
    /// Random data len, determines the (maximum) size of the
    /// generated data
    #[clap(short, long)]
    random_len: usize,

    /// Minimum output slice length to spawn a task for
    #[clap(short, long)]
    slice_len: usize,
}

fn main() -> Result<()> {
    cpu_probe::init()?;

    let Opt {
        random_len,
        slice_len,
    } = Opt::from_args();

    let random_data = {
        probe!("random_data");
        let mut random_data = vec![0; random_len];
        getrandom(&mut random_data)?;
        dbg!(random_data.len());
        random_data
    };
    let mut data = {
        probe!("data");
        Unstructured::new(&random_data)
    };
    let bag = {
        probe!("arbitrary");
        Bag::<i32>::arbitrary(&mut data)?
    };

    let show_it = false;
    if show_it {
        eprintln!("{bag:?}");
    }
    eprintln!("bag.len() = {}", bag.len());

    let l1 = {
        let bag = {
            probe!("clone");
            bag.clone()
        };
        probe!("flatten");
        bag.flatten()
    };
    let l2 = {
        probe!("par_flatten");
        bag.par_flatten(slice_len)
    };
    assert_eq!(l1, l2);

    if show_it {
        panic!();
    }

    Ok(())
}
