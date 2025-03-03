use crate::lexer::token::{Token, TokenKind};
use std::ops::Range;

use super::pratt::{OpBindingPower, PrattBindingPowers};

#[derive(Debug)]
pub struct TokenStream<'a> {
    toks: &'a [Token],
    last_span: Range<usize>,
    
    op_bps: PrattBindingPowers,
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
    #[inline(always)]
    pub fn new(toks: &'a [Token]) -> TokenStream {
        Self {
            toks,
            last_span: 0..0,
            op_bps: PrattBindingPowers::new(),
        }
    }

    #[inline(always)]
    pub fn next(&mut self) -> Option<(TokenKind, Range<usize>)> {
        let (tok, rest) = self.toks.split_first()?;
        let (kind, span) = tok.clone().into_inner();
        self.toks = rest;
        self.last_span = span.clone();
        Some((kind, span))
    }

    #[inline(always)]
    pub fn peek(&self) -> Option<(TokenKind, Range<usize>)> {
        Some(self.toks.first().cloned()?.into_inner())
    }

    #[inline(always)]
    pub fn last_span(&self) -> Range<usize> {
        self.last_span.clone()
    }
    
    #[inline(always)]
    pub fn toks_remaining(&self) -> usize {
        self.toks.len()
    }

    #[inline(always)]
    pub fn checkpoint(&self) -> Checkpoint<'a> {
        Checkpoint {
            toks: self.toks,
        }
    }

    #[inline(always)]
    pub fn reset(&mut self, checkpoint: &Checkpoint<'a>) {
        self.toks = checkpoint.toks;
    }

}

impl<'a> OpBindingPower for TokenStream<'a> {
    #[inline(always)]
    fn prefix_bp(&self, op: String) -> Option<usize> {
        self.op_bps.prefix_bp(op)
    }

    #[inline(always)]
    fn infix_bp(&self, op: String) -> Option<(usize, usize)> {
        self.op_bps.infix_bp(op)
    }

    #[inline(always)]
    fn postfix_bp(&self, op: String) -> Option<usize> {
        self.op_bps.postfix_bp(op)
    }
}

pub struct Checkpoint<'a> {
    toks: &'a [Token],
}
