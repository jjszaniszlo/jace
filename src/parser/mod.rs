mod token;
mod error;
mod ast;
mod ptr;
mod expr;

use std::ops::Range;
use ast::{Expr, Identifier, Literal};
use error::JaceParseError;
use winnow::error::{FromRecoverableError, ParserError};
use winnow::prelude::*;
use winnow::stream::{Location, Recover, Recoverable, Stream, TokenSlice};
use winnow::Result;
use winnow::{
    ascii::{digit1 as digits, multispace0 as multispaces},
    combinator::alt,
    combinator::dispatch,
    combinator::fail,
    combinator::peek,
    combinator::repeat,
    combinator::{delimited, preceded, terminated},
    token::any,
    token::one_of,
};
use crate::parser::ast::AstSpan;
use crate::parser::expr::parse_expression;
use crate::Token;
use crate::TokenKind;

pub type ParserInput<'a> = Recoverable<TokenSlice<'a, Token>, JaceParseError>;
pub type ParserOutput<T> = winnow::Result<T, JaceParseError>;

pub fn parse_literal(input: &mut ParserInput) -> ParserOutput<Literal>
{
    one_of(|t: &Token|
        matches!(
            t.kind(),
            TokenKind::Integer(_) |
            TokenKind::Float(_) |
            TokenKind::Bool(_) |
            TokenKind::String(_)))
        .with_span()
    .map(|(tok, span): (&Token, Range<usize>)|
        match tok.kind() {
            TokenKind::Integer(i) => Literal::Integer(i, span),
            TokenKind::Float(f) => Literal::Float(f, span),
            TokenKind::Bool(b) => Literal::Bool(b, span),
            TokenKind::String(s) => Literal::String(s, span),
            _ => unreachable!()})
    .parse_next(input)
}

pub fn parse_identifier(input: &mut ParserInput) -> ParserOutput<Identifier> {
    one_of(|t: &Token|
        matches!(t.kind(), TokenKind::Identifier(_)))
        .with_span()
    .map(|(tok, span): (&Token, Range<usize>)|
        match tok.kind() {
            TokenKind::Identifier(s) => Identifier(s, span),
            _ => unreachable!()})
    .parse_next(input)
}

#[test]
pub fn test1() {
    let toks = vec![
        Token::new(TokenKind::Integer(100), 0, 3),
    ];
    let res = parse_expression(&mut Recoverable::new(TokenSlice::new(&toks)));
    println!("{:#?}", res);
}

