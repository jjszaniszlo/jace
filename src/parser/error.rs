use miette::SourceSpan;

use crate::Token;
use crate::TokenKind;

#[derive(thiserror::Error, miette::Diagnostic, Debug)]
pub enum ParserError {
    #[error("Unexpected EOF")]
    UnexpectedEOF,

    #[error("Expected: {expected}, Got: {got}")]
    ExpectedGot {
        expected: String,
        got: TokenKind,

        #[label("Here")]
        span: SourceSpan,
    },

    #[error("Unexpected Token: {tok}")]
    UnexpectedToken {
        tok: TokenKind,

        #[label("Here")]
        span: SourceSpan,
    },

    #[error("Unexpected Parse")]
    UnexpectedParse {
        #[label("Here")]
        span: SourceSpan,
    },
}

impl ParserError {
    pub fn unexpected_eof() -> Self {
        ParserError::UnexpectedEOF
    }

    pub fn unexpected_tok(tok: &Token) -> Self {
        ParserError::UnexpectedToken { tok: tok.kind(), span: tok.span().into() }
    }

    pub fn expected_got(expected: &'static str, got: &Token) -> Self {
        ParserError::ExpectedGot { expected: expected.to_string(), got: got.kind(), span: got.span().into() }
    }
}

