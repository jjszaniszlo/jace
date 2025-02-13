use std::fmt::Display;
use miette::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span(pub usize, pub usize);

impl Span {
    pub fn combine(self, other: Span) -> Span {
        Span(self.0, other.1)
    }
}

impl From<Span> for SourceSpan {
    fn from(value: Span) -> Self {
        SourceSpan::new(value.0.into(), value.1.into())
    }
}

pub fn error_maybe<T>(v: Result<T, impl Display>, msg: String) -> T {
    match v {
        Ok(v) => v,
        Err(e) => {
            eprintln!("jace: {msg}: {err_msg}", err_msg = e.to_string());

            std::process::exit(1);
        }
    }
}
