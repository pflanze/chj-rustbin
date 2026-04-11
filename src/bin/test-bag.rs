use anyhow::Result;
use arbitrary::{Arbitrary, Unstructured};
use chj_rustbin::bag::Bag;
use getrandom::getrandom;

fn main() -> Result<()> {
    let mut random_data = vec![0; 10_000_000];
    getrandom(&mut random_data)?;
    let mut data = Unstructured::new(&random_data);
    let bag = Bag::<i32>::arbitrary(&mut data)?;

    let show_it = false;
    if show_it {
        eprintln!("{bag:?}");
    }
    eprintln!("bag.len() = {}", bag.len());

    let l1 = bag.clone().flatten();
    let l2 = bag.par_flatten();
    assert_eq!(l1, l2);

    if show_it {
        panic!();
    }

    Ok(())
}
