//! Provide a handler type with methods for a fixed set of sub-directories

/// Define a struct that holds a `base_dir` path and has methods to get a
/// sub-path (assumed to be a directory) with the same file name as the method
/// name
///
/// Also generates a `create_dirs_if_not_exist` method to eagerly create a
/// directory at each sub-path that does not exist yet (`base_dir` must already
/// exist) and bail on errors.
///
/// Example: `def_subdirs! {Dirs => patches, linux, hashes}`
///
/// The name of the struct is `Dirs` here, and the name of the methods and
/// sub-dirs are the identifiers given after the arrow.
///
#[macro_export]
macro_rules! def_subdirs {
    // Initial call
    { $T:ident => $($subdir:ident $(,)?)* } =>  {
        def_subdirs!{
            { $($subdir,)* };
            $T,
            0,
            this,
            {}
        }
    };
    // Iteration
    { { $subdir:ident, $($rest:tt)* }; $T:ident, $i:expr, $this:ident, { $($init:tt)* } } =>  {
        impl Dirs {
            fn $subdir(&self) -> &Arc<Path> {
                &self.subdirs[$i]
            }
        }
        def_subdirs!{
            { $($rest)* };
            $T,
            $i + 1,
            $this,
            {
                $($init)*
                $this.subdirs.push((&$this.base_dir).append(stringify!($subdir)).into());
            }
        }
    };
    { {}; $T:ident, $i:expr, $this:ident, { $($init:tt)* } } => {

        #[derive(Debug)]
        struct $T {
            base_dir: PathBuf,
            subdirs: Vec<Arc<Path>>,
        }

        impl $T {
            fn new(base_dir: PathBuf) -> Self {
                let mut $this = Self {
                    base_dir,
                    subdirs: Vec::new()
                };
                $($init)*
                $this
            }

            fn create_dirs_if_not_exist(&self) -> anyhow::Result<()> {
                use chj_rustbin::io_utils::dirs::create_dir_if_not_exists;
                use anyhow::Context;

                for path in &self.subdirs {
                    create_dir_if_not_exists(path)
                        .with_context(|| format!("creating dir {path:?}"))?;
                }
                Ok(())
            }
        }
    }
}
