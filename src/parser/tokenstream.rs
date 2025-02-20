use std::ops::Range;
use crate::lexer::token::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct TokenStream<'a> {
    input: &'a [Token],
    last_span: Range<usize>,
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
            input: toks,
            last_span: 0..0,
        }
    }

    pub fn next(&mut self) -> Option<(TokenKind, Range<usize>)> {
        let (tok, rest) = self.input.split_first()?;
        let (kind, span) = tok.clone().into_inner();
        self.input = rest;
        self.last_span = span.clone();
        Some((kind, span.clone()))
    }

    pub fn peek(&self) -> Option<(TokenKind, Range<usize>)> {
        Some(self.input.first().cloned()?.into_inner())
    }

    pub fn last_span(&self) -> Range<usize> {
        self.last_span.clone()
    }

    pub fn checkpoint(&self) -> Checkpoint<'a> {
        Checkpoint {
            input: self.input,
        }
    }

    pub fn restore_checkpoint(&mut self, checkpoint: Checkpoint<'a>) {
        self.input = checkpoint.input;
    }
}

#[derive(Debug, Clone)]
pub struct Checkpoint<'a> {
    input: &'a [Token],
}
