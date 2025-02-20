use std::num::ParseIntError;

use miette::Severity;
use miette::SourceSpan;
use winnow::error::AddContext;
use winnow::error::FromExternalError;
use winnow::error::FromRecoverableError;
use winnow::error::ParserError;
use winnow::stream::Location;
use winnow::stream::Stream;

use super::ParserInput;

#[derive(Debug, Clone, Default, Eq, PartialEq)]
struct JaceParseContext {
    message: Option<String>,
    label: Option<String>,
    help: Option<String>,
    severity: Option<Severity>,
}

impl JaceParseContext {
    fn message(mut self, txt: impl AsRef<str>) -> Self {
        self.message = Some(txt.as_ref().to_string());
        self
    }

    fn label(mut self, txt: impl AsRef<str>) -> Self {
        self.label = Some(txt.as_ref().to_string());
        self
    }

    fn help(mut self, txt: impl AsRef<str>) -> Self {
        self.help = Some(txt.as_ref().to_string());
        self
    }

     fn severity(mut self, severity: Severity) -> Self {
         self.severity = Some(severity);
         self
     }
}

#[derive(Debug, Default, Clone, Eq, PartialEq)]
pub struct JaceParseError {
    pub message: Option<String>,
    pub span: Option<SourceSpan>,
    pub label: Option<String>,
    pub help: Option<String>,
    pub severity: Option<Severity>,
}

impl<I: Stream> ParserError<I> for JaceParseError {
    type Inner = JaceParseError;

    fn from_input(input: &I) -> Self {
        Self {
            ..Default::default()
        }
    }

    fn into_inner(self) -> winnow::Result<Self::Inner, Self> {
        Ok(self)
    }
}

impl<I: Stream> AddContext<I, JaceParseContext> for JaceParseError {
    #[inline]
    fn add_context(
        mut self,
        input: &I,
        start: &<I as Stream>::Checkpoint,
        c: JaceParseContext,
    ) -> Self {
        self.message = self.message.or(c.message);
        self.label = self.label.or(c.label);
        self.help = self.help.or(c.help);
        self.severity = self.severity.or(c.severity);
        self
    }
}

//impl<I: Stream + Location> FromRecoverableError<I, Self> for JaceParseError {
//    fn from_recoverable_error(
//        token_start: &<I as Stream>::Checkpoint,
//        err_start: &<I as Stream>::Checkpoint,
//        input: &I,
//        e: Self,
//    ) -> Self {
//        e.span = e.span.or_else(|| Some(span_from_checkpoint(input, token_start)));
//        e
//    }
//}
