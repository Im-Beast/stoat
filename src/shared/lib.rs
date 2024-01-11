pub mod interner;
pub mod span;

#[macro_export]
macro_rules! dbg_line {
    () => {
        format!("{}:{}", file!(), line!())
    };
}
