use crate::lexer::token::{Token, TokenKind};
use std::ops::Range;

#[derive(Debug, Clone, Copy)]
pub struct TokenStream<'a> {
    toks: &'a [Token],
    last_span: (usize, usize),
}

pub trait TokenResult {
    fn kind(&self) -> TokenKind;
    fn span(&self) -> Range<usize>;
}

impl TokenResult for (TokenKind, Range<usize>) {
    fn kind(&self) -> TokenKind {
        self.0.clone()
    }

    fn span(&self) -> Range<usize> {
        self.1.clone()
    }
}

impl<'a> TokenStream<'a> {
    pub fn new(toks: &'a [Token]) -> TokenStream {
        Self {
            toks,
            last_span: (0, 0),
        }
    }

    pub fn next(self) -> Option<((TokenKind, Range<usize>), TokenStream<'a>)> {
        let (tok, rest) = self.toks.split_first()?;
        let (kind, span) = tok.clone().into_inner();
        Some(((kind, span.clone()), Self {
            toks: rest,
            last_span: (span.start, span.end),
        }))
    }

    pub fn peek(&self) -> Option<(TokenKind, Range<usize>)> {
        Some(self.toks.first().cloned()?.into_inner())
    }

    pub fn last_span(&self) -> Range<usize> {
        self.last_span.0..self.last_span.1
    }
}
