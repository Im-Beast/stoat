#[derive(Clone, Copy, Debug)]
pub struct Span {
    pub start: usize,
    pub length: usize,
}

impl Into<Span> for (usize, usize) {
    fn into(self) -> Span {
        Span {
            start: self.0,
            length: self.1,
        }
    }
}

#[macro_export]
macro_rules! span {
    ($start:expr, $end:expr) => {
        ($start, $end - $start).into()
    };

    (absolute; $start: expr, $len: expr) => {
        ($start, $end).into()
    };
}
