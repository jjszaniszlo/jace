use std::backtrace::Backtrace;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::TokenKind;

#[derive(Error, Debug, Diagnostic)]
#[error("Unrecoverable Error: {message}")]
pub struct UnrecoverableError {
    message: String,

    #[label("here")]
    error_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("Contextual error in {context}")]
pub struct ContextualError {
    context: String,

    #[source]
    #[diagnostic_source]
    error: InnerError,
}


#[derive(Error, Debug, Diagnostic)]
#[error("Parser Error")]
pub enum ParserError {
    #[diagnostic_source]
    UnrecoverableError(#[from] UnrecoverableError),

    #[diagnostic_source]
    ContextualError(#[from] ContextualError),

    #[diagnostic_source]
    InnerError(#[from] InnerError),
}

impl ParserError {
    pub fn unrecoverable(message: &'static str, cause: ParserError, at: SourceSpan) -> ParserError {
        match cause {
            ParserError::UnrecoverableError(err) => ParserError::UnrecoverableError(err),
            ParserError::ContextualError(cause) => ParserError::UnrecoverableError(
                UnrecoverableError {
                    message: message.to_string(),
                    error_span: at,
                }),
            ParserError::InnerError(inner_error) => ParserError::UnrecoverableError(
                UnrecoverableError{
                    message: message.to_string(),
                    error_span: at,
                }),
        }
    }

    pub fn contextual(context: &'static str, error: ParserError) -> ParserError {
        match error {
            ParserError::UnrecoverableError(err) => ParserError::ContextualError(
                ContextualError {
                    context: context.to_string(),
                    error: InnerError::UnexpectedEOF,
                }),
            ParserError::ContextualError(context) => ParserError::ContextualError(context),
            ParserError::InnerError(inner_error) => ParserError::ContextualError(
                ContextualError{
                    context: context.to_string(),
                    error: inner_error,
                }),
        }
    }
}

#[derive(Error, Debug, Diagnostic)]
pub enum InnerError {
    #[error("Errors: {errors:?}")]
    #[diagnostic(severity(Error))]
    CombinedErrors {
        #[related]
        errors: Vec<ParserError>,
    },

    #[error("Could not match token")]
    CouldNotMatchToken {
        #[label("this token")]
        error_span: SourceSpan,
    },

    #[error("Expected a token but no remaining tokens.")]
    UnexpectedEOF,

    #[error("Could not parse zero_or_more")]
    ZeroOrMoreParseError,

    #[error("Expected token {}, got {}", expected, got)]
    ExpectedTokenGot {
        #[label("this token")]
        error_span: SourceSpan,

        expected : TokenKind,
        got : TokenKind,
    },

    #[error("Expected an Identifier, got: {}", got)]
    ExpectedIdentifierGot {
        #[label("this token")]
        error_span: SourceSpan,

        got : TokenKind,
    },

    #[error("Expected a Literal (Integer, Float, Bool, String, Set), got: {}", got)]
    ExpectedLiteralGot {
        #[label("this token")]
        error_span: SourceSpan,

        got : TokenKind,
    },

    #[error("Expected an Operator, got: {}", got)]
    ExpectedOperatorGot {
        #[label("this token")]
        error_span: SourceSpan,

        got : TokenKind,
    },

    #[error("Expected a Wrapped Operator, got: {}", got)]
    ExpectedWrappedOperatorGot {
        #[label("this token")]
        error_span: SourceSpan,

        got : TokenKind,
    },

    #[error("Expected a Unary Operator, got: {}", got)]
    ExpectedUnaryOperatorGot {
        #[label("this token")]
        error_span: SourceSpan,

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

impl InnerError {
    pub fn combined(errors: Vec<ParserError>) -> Self {
        InnerError::CombinedErrors { errors }
    }
}
