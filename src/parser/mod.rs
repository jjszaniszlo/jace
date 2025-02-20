mod token;
mod error;
mod ast;
mod ptr;

use ast::{Expr, Identifier, Literal};
use error::JaceParseError;
use winnow::error::ParserError;
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

use crate::Token;
use crate::TokenKind;

pub type ParserInput<'a> = Recoverable<TokenSlice<'a, Token>, JaceParseError>;
pub type ParserOutput<T> = winnow::Result<T, JaceParseError>;

pub fn parse_literal<'a>(input: &mut ParserInput<'a>) -> ParserOutput<Literal> {
    one_of(|t: &Token|
        matches!(
            t.kind(),
            TokenKind::Integer(_) |
            TokenKind::Float(_) |
            TokenKind::Bool(_) |
            TokenKind::String(_)))
    .map(|t: &Token|
        match t.kind() {
            TokenKind::Integer(i) => Literal::Integer(i),
            TokenKind::Float(f) => Literal::Float(f),
            TokenKind::Bool(b) => Literal::Bool(b),
            TokenKind::String(s) => Literal::String(s),
            _ => unreachable!()})
    .parse_next(input)
}

pub fn parse_identifier<'a>(input: &mut ParserInput<'a>) -> ParserOutput<Identifier> {
    one_of(|t: &Token|
        matches!(t.kind(), TokenKind::Identifier(_)))
    .map(|t: &Token|
        match t.kind() {
            TokenKind::Identifier(s) => Identifier(s),
            _ => unreachable!()})
    .parse_next(input)
}

pub fn parse_expression<'a>(input: &mut ParserInput<'a>) -> ParserOutput<Expr> {
    let start = input.checkpoint();
    alt((
        parse_identifier.map(|i| Expr::IdentExpr(i, 0..1)),
        parse_literal.map(|l| Expr::LitExpr(l, 0..1))
    ))
    .parse_next(input)
}

#[test]
pub fn test1() {
    let toks = vec![Token::new(TokenKind::Integer(10), 0, 2)];
    let res = parse_expression(&mut Recoverable::new(TokenSlice::new(&toks)));
    assert_eq!(res, Ok(Expr::LitExpr(Literal::Integer(10), 0..1)))
}

