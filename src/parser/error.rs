use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::TokenKind;

#[derive(Error, Debug, Clone, PartialEq, Diagnostic)]
pub enum ParserError {
    #[error("Could not match token")]
    CouldNotMatchToken,

    #[error("Expected a token but no remaining tokens.")]
    UnexpectedEOF,

    #[error("Could not parse zero_or_more")]
    ZeroOrMoreParseError,

    #[error("Expected token {}, got {}", expected, got)]
    ExpectedTokenGot {
        expected : TokenKind,
        got : TokenKind,
    },

    #[error("Expected an Identifier, got: {}", got)]
    ExpectedIdentifierGot {
        got : TokenKind,
    },

    #[error("Expected a Literal (Integer, Float, Bool, String, Set), got: {}", got)]
    ExpectedLiteralGot {
        got : TokenKind,
    },

    #[error("Expected an Operator, got: {}", got)]
    ExpectedOperatorGot {
        got : TokenKind,
    },

    #[error("Expected a Wrapped Operator, got: {}", got)]
    ExpectedWrappedOperatorGot {
        got : TokenKind,
    },

    #[error("Expected a Unary Operator, got: {}", got)]
    ExpectedUnaryOperatorGot {
        got : TokenKind,
    },

    #[error("expected: {}, {:?}", expected, got)]
    CouldNotParseBinExp {
        #[label("this operator")]
        error_span: SourceSpan,

        expected : String,
        got : Option<TokenKind>,
    },
}
