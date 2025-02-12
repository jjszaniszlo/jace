use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::TokenKind;

use super::Span;

#[derive(Error, Debug, Diagnostic)]
#[error("Unrecoverable Error: {message}")]
pub struct UnrecoverableError {
    pub message: String,

    #[label("here")]
    pub error_span: SourceSpan,

    #[source]
    #[diagnostic_source]
    pub cause: InnerError,
}

#[derive(Error, Debug, Diagnostic)]
#[error("Contextual error in {context}")]
pub struct ContextualError {
    pub context: String,

    #[label("here")]
    pub error_span: SourceSpan,

    #[source]
    #[diagnostic_source]
    pub error: InnerError,
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

impl From<InnerError> for UnrecoverableError {
    fn from(value: InnerError) -> Self {
        match value {
            InnerError::UnexpectedEOF => ,
            InnerError::CombinedErrors { errors } => todo!(),
            InnerError::CouldNotMatchToken { error_span } => todo!(),
            InnerError::ZeroOrMoreParseError { error_span } => todo!(),
            InnerError::ExpectedTokenGot { error_span, expected, got } => todo!(),
            InnerError::ExpectedIdentifierGot { error_span, got } => todo!(),
            InnerError::ExpectedLiteralGot { error_span, got } => todo!(),
            InnerError::ExpectedOperatorGot { error_span, got } => todo!(),
            InnerError::ExpectedWrappedOperatorGot { error_span, got } => todo!(),
            InnerError::ExpectedUnaryOperatorGot { error_span, got } => todo!(),
            InnerError::CouldNotParseBinExp { error_span, expected, got } => todo!(),
        }
    }
}

impl ParserError {
    pub fn unrecoverable(message: &'static str, cause: ParserError, at: SourceSpan) -> ParserError {
        match cause {
            ParserError::UnrecoverableError(err) => ParserError::UnrecoverableError(err),
            ParserError::ContextualError(cause) => ParserError::UnrecoverableError(
                UnrecoverableError {
                    message: message.to_string(),
                    error_span: at,
                    cause: cause.error,
                }),
            ParserError::InnerError(inner_error) => ParserError::UnrecoverableError(
                UnrecoverableError{
                    message: message.to_string(),
                    error_span: at,
                    cause: inner_error.into()
                }),
        }
    }

    pub fn contextual(context: &'static str, error: ParserError, span: Span) -> ParserError {
        match error {
            ParserError::UnrecoverableError(err) => ParserError::ContextualError(
                ContextualError {
                    context: context.to_string(),
                    error_span: span.into(),
                    error: InnerError::UnexpectedEOF,
                }),
            ParserError::ContextualError(context) => ParserError::ContextualError(context),
            ParserError::InnerError(inner_error) => ParserError::ContextualError(
                ContextualError{
                    context: context.to_string(),
                    error_span: span.into(),
                    error: inner_error,
                }),
        }
    }
}

#[derive(Error, Debug, Diagnostic)]
pub enum InnerError {
    #[error("Expected a token but no remaining tokens.")]
    UnexpectedEOF,

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

    #[error("Could not parse zero_or_more")]
    ZeroOrMoreParseError {
        #[label("here")]
        error_span: SourceSpan,
    },

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
