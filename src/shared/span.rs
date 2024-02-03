use miette::SourceSpan;

#[derive(Copy, Debug)]
pub struct Span {
    pub start: usize,
    pub length: usize,
}

impl Clone for Span {
    fn clone(&self) -> Self {
        *self
    }
}

impl From<(usize, usize)> for Span {
    fn from((start, length): (usize, usize)) -> Self {
        Self { start, length }
    }
}

impl From<&Span> for SourceSpan {
    fn from(span: &Span) -> Self {
        (span.start, span.length).into()
    }
}

#[macro_export]
macro_rules! span {
    ($start:expr, $end:expr) => {
        ($start, $end - $start).into()
    };
}
