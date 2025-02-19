use std::fmt::Display;
use std::sync::Arc;
use miette::{Diagnostic, Severity, SourceSpan};
use thiserror::Error;

pub trait CombineSourceSpan {
    fn combine_span(self, span: SourceSpan) -> SourceSpan;
}

impl CombineSourceSpan for SourceSpan {
    fn combine_span(self, span: SourceSpan) -> SourceSpan {
        SourceSpan::new(self.offset().into(), self.len() + span.len() + span.offset().max(self.offset() + self.len()) - span.offset().min(self.offset()+ self.len()))
    }
}

#[derive(Debug, Diagnostic, Error, Clone, Eq, PartialEq)]
#[error("Failed to parse jace module")]
pub struct JaceError {
    #[source_code]
    pub src: Arc<String>,
    #[related]
    pub diagnostics: Vec<JaceDiagnostic>,
}

#[derive(Debug, Diagnostic, Error, Clone, Eq, PartialEq)]
#[error("{}", message.clone().unwrap_or_else(|| "Error occurred".into()))]
pub struct JaceDiagnostic {
    pub message: Option<String>,
    pub label: Option<String>,
    pub help : Option<String>,

    #[diagnostic(severity)]
    pub severity: Severity,
    #[label("{}", label.clone().unwrap_or_else(|| "here".into()))]
    pub span: SourceSpan,
    #[source_code]
    pub src: Arc<String>,
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
