use std::{path::Path, sync::Mutex, thread};

use anyhow::Result;
use chj_rustbin::leaked_region::GlobalLeakedRegions;

fn main() -> Result<()> {
    let regions = &GlobalLeakedRegions::new(1000);
    let pss = &Mutex::new(Vec::new());
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
                pss.lock().expect("no panics").push(ps);
                eprintln!("finished {i}");
            });
        }
    });
    dbg!(pss);
    // thread::scope(|scope| {
    //     for i in 2..4 {
    //         scope.spawn(move || {
    //             let path = format!("foo bar banm bum {i}");
    //             let path: &Path = path.as_ref();
    //             let mut region = regions_rf.get_region();
    //             for _ in 0..1000 {
    //                 let p = region.allocate_path(path);
    //                 assert_eq!(p, path);
    //             }
    //             eprintln!("finished {i}");
    //         });
    //     }
    // });
    Ok(())
}
