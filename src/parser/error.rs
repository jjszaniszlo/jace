use crate::parser::error::ErrorType::{Recoverable, Unrecoverable};
use miette::Severity;
use std::ops::Range;

#[derive(Debug, Clone)]
pub enum ErrorType {
    // incomplete means that the parser needs additional information
    // to complete error.
    Incomplete,

    // unrecoverable means that a branch has parsed far enough to determine what
    // kind of structure is being parsed for sure, and that the parser shouldn't
    // try other branches.
    Unrecoverable(ParserError),

    // recoverable means that a branch has not parsed, so try another branch.
    Recoverable(ParserError),
}

impl ErrorType {
    pub fn unrecoverable(self) -> Option<ErrorType> {
        match self {
            ErrorType::Incomplete => None,
            Unrecoverable(e) | Recoverable(e) => Some(Unrecoverable(e)),
        }
    }

    pub fn recoverable(self) -> Option<ErrorType> {
        match self {
            ErrorType::Incomplete => None,
            Unrecoverable(e) | Recoverable(e) => Some(Recoverable(e)),
        }
    }

    pub fn inner(err: ErrorType) -> Option<ParserError> {
        match err {
            Recoverable(err) | Unrecoverable(err) => Some(err),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParserError {
    pub message: Option<String>,
    pub label: Option<String>,
    pub help: Option<String>,
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
