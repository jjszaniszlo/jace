use crate::lexer::token::{Token, TokenKind};
use std::ops::Range;

use super::state::{IdentCounter, ParserState};

#[derive(Debug)]
pub struct TokenStream<'a> {
    toks: &'a [Token],
    last_span: Range<usize>,
    state: ParserState,
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
            last_span: 0..0,
            state: ParserState::new(),
        }
    }

    pub fn next(&mut self) -> Option<(TokenKind, Range<usize>)> {
        let (tok, rest) = self.toks.split_first()?;
        let (kind, span) = tok.clone().into_inner();
        self.toks = rest;
        self.last_span = span.clone();
        Some((kind, span))
    }

    pub fn peek(&self) -> Option<(TokenKind, Range<usize>)> {
        Some(self.toks.first().cloned()?.into_inner())
    }

    pub fn last_span(&self) -> Range<usize> {
        self.last_span.clone()
    }

    pub fn checkpoint(&self) -> Checkpoint<'a> {
        Checkpoint {
            toks: self.toks,
        }
    }

    pub fn reset(&mut self, checkpoint: &Checkpoint<'a>) {
        self.toks = checkpoint.toks;
    }
}

impl<'a> IdentCounter for TokenStream<'a> {
    fn increment(&mut self) {
        self.state.increment();
    }
}

pub struct Checkpoint<'a> {
    toks: &'a [Token],
}
