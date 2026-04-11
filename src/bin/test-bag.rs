use anyhow::Result;
use arbitrary::{Arbitrary, Unstructured};
use chj_rustbin::{bag::Bag, cpu_probe, probe};
use getrandom::getrandom;

fn main() -> Result<()> {
    cpu_probe::init()?;

    let random_data = {
        probe!("random_data");
        let mut random_data = vec![0; 10_000_000];
        getrandom(&mut random_data)?;
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
        bag.par_flatten()
    };
    assert_eq!(l1, l2);

    if show_it {
        panic!();
    }

    Ok(())
}
