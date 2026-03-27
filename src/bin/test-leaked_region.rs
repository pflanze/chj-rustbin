use std::{path::Path, sync::Mutex, thread};

use anyhow::Result;
use chj_rustbin::leaked_region::GlobalLeakedRegions;

fn main() -> Result<()> {
    let regions = &GlobalLeakedRegions::new(1000);
    let pss = Mutex::new(Vec::new());
    {
        let pss = &pss;
        thread::scope(|scope| {
            for i in 0..2 {
                scope.spawn(move || {
                    let path = format!("foo bar banm bum {i}");
                    let path: &Path = path.as_ref();
                    let mut region = regions.get_region();
                    let mut ps = Vec::new();
                    // 150*(18+1) = 2850, meaning 3 regions used
                    for _ in 0..150 {
                        let p = region.allocate_path(path);
                        assert_eq!(p, path);
                        ps.push(p);
                    }
                    pss.lock().expect("no panics").push((i, ps));
                    eprintln!("finished {i}");
                });
            }
        });
    }
    let pss = pss.into_inner().expect("no panics");
    assert_eq!(pss.len(), 2);
    for (i, ps) in pss {
        let path = format!("foo bar banm bum {i}");
        let path: &Path = path.as_ref();
        for p in ps {
            assert_eq!(p, path);
        }
    }

    Ok(())
}
