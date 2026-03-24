use std::path::Path;

use anyhow::Result;
use chj_rustbin::leaked_region::GlobalLeakedRegions;

fn main() -> Result<()> {
    let regions = GlobalLeakedRegions::new();
    for i in 0..2 {
        let path = format!("foo bar banm bum {i}");
        let path: &Path = path.as_ref();
        let mut region = regions.get_region();
        for _ in 0..10000 {
            let p = region.allocate_path(path);
            assert_eq!(p, path);
        }
        eprintln!("finished {i}");
    }
    Ok(())
}
