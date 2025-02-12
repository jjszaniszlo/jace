use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::TokenKind;

use super::Span;

// Error - an error which is recoverable.
// Failure - an error which is not recoverable.
// Complete - parse was successful
pub enum ParseResult {
    Error(ParserError),
    Failure(ParserError),
    Complete,
}

#[derive(thiserror::Error, miette::Diagnostic, Debug)]
pub enum ParserError {
    #[error("Unexpected EOF")]
    UnexpectedEOF,

    #[error("Expected: {expected}, Got: {got}")]
    ExpectedGot {
        expected: TokenKind,
        got: TokenKind,

        #[label("Here")]
        span: SourceSpan
    },

    #[error("Unexpected Token: {tok}")]
    UnexpectedToken {
        tok: TokenKind,

        #[label("Here")]
        span: SourceSpan
    }
}
