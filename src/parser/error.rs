use std::ops::Range;
use std::sync::Arc;
use miette::{Severity, SourceSpan};

pub struct ParserError {
    pub message: Option<String>,
    pub label: Option<String>,
    pub help : Option<String>,
    pub severity: Option<Severity>,
    pub span: Option<Range<usize>>,
}

impl ParserError {
    pub fn new() -> Self {
        Self {
            message: None,
            label: None,
            help: None,
            severity: None,
            span: None,
        }
    }

    pub fn message(&mut self, message: String) -> &mut Self {
        self.message = Some(message);
        self
    }

    pub fn label(&mut self, label: String) -> &mut Self {
        self.label = Some(label);
        self
    }

    pub fn help(&mut self, help: String) -> &mut Self {
        self.help = Some(help);
        self
    }

    pub fn severity(&mut self, severity: Severity) -> &mut Self {
        self.severity = Some(severity);
        self
    }

    pub fn span(&mut self, span: Range<usize>) -> &mut Self {
        self.span = Some(span);
        self
    }

    pub fn build(&mut self) -> Self {
        Self {
            message: self.message.clone(),
            label: self.label.clone(),
            help: self.help.clone(),
            severity: self.severity,
            span: self.span.clone(),
        }
    }
}
