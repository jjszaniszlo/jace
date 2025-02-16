use std::ops::Range;
use miette::SourceSpan;
use super::combinator::*;
use crate::parser::ast::*;
use crate::parser::tokenstream::{TokenResult, TokenStream};
use crate::{lexer::token::TokenKind, parser::{ast, error::*}};

pub type Output<'a, Out> = miette::Result<(TokenStream<'a>, Out, Range<usize>), ParserError>;

pub trait Parser<'a, Out> {
    fn parse(&self, input: TokenStream<'a>) -> Output<'a, Out>;

    fn map<Func, NewOut>(self, map_fn: Func) -> BoxedParser<'a, NewOut>
    where
        Self: Sized + 'a,
        Out: 'a,
        NewOut: 'a,
        Func: Fn(Out, Range<usize>) -> NewOut + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    fn and_then<Func, NewParser, NewOut>(self, f: Func) -> BoxedParser<'a, NewOut>
    where
        Self: Sized + 'a,
        Out: 'a,
        NewOut: 'a,
        NewParser: Parser<'a, NewOut> + 'a,
        Func: Fn(Out, Range<usize>) -> NewParser + 'a,
    {
        BoxedParser::new(and_then(self, f))
    }
}

impl<'a, F, Out> Parser<'a, Out> for F
where
    F: Fn(TokenStream<'a>) -> Output<'a, Out>,
{
    fn parse(&self, input: TokenStream<'a>) -> Output<'a, Out> {
        self(input)
    }
}

pub struct BoxedParser<'a, Out> {
    parser: Box<dyn Parser<'a, Out> + 'a>,
}

impl<'a, Out> BoxedParser<'a, Out> {
    pub fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Out> + 'a,
    {
        Self {
            parser: Box::new(parser)
        }
    }
}

impl<'a, Out> Parser<'a, Out> for BoxedParser<'a, Out> {
    fn parse(&self, input: TokenStream<'a>) -> Output<'a, Out> {
        self.parser.parse(input)
    }
}

pub fn match_token<'a>(expected: TokenKind) -> impl Parser<'a, ()> {
    move |input: TokenStream<'a>| {
        match input.next() {
            Some((res, next_input))
            if res.kind() == expected => {
                Ok((next_input, (), res.span()))
            }
            Some((res, _)) => Err(
                ParserError::new()
                    .message(format!("unexpected token: `{:?}`", res.kind()))
                    .span(res.span())
                    .build()),
            None => Err(ParserError::new()
                .message("unexpected eof".to_string())
                .span(input.last_span())
                .build())
        }
    }
}

pub fn parse_identifier<'a>() -> impl Parser<'a, Identifier> {
    move |input: TokenStream<'a>| {
        match input.next() {
            Some((res, next_input)) => {
                match res.kind() {
                    TokenKind::Identifier(ident) => {
                        Ok((next_input, Identifier(ident), res.span()))
                    }
                    _ => Err(ParserError::new()
                        .message(format!("unexpected token: `{:?}`", res.kind()))
                        .span(res.span())
                        .build())
                }
            }
            None => Err(ParserError::new()
                .message("unexpected eof".to_string())
                .span(input.last_span())
                .build())
        }
    }
}

pub fn parse_literal<'a>() -> impl Parser<'a, Literal> {
    move |input: TokenStream<'a>| {
        match input.next() {
            Some((res, next_input)) => {
                let lit = match res.kind() {
                    TokenKind::Bool(b) => Literal::Bool(b),
                    TokenKind::Integer(i) => Literal::Integer(i),
                    TokenKind::String(s) => Literal::String(s),
                    TokenKind::Float(f) => Literal::Float(f),

                    _ => return Err(ParserError::new()
                        .message(format!("unexpected token: `{:?}`", res.kind()))
                        .span(res.span())
                        .build())
                };

                Ok((next_input, lit, res.span()))
            }
            None => Err(ParserError::new()
                .message("unexpected eof".to_string())
                .span(input.last_span())
                .build())
        }
    }
}

pub fn parse_literal_integer<'a>() -> impl Parser<'a, usize> {
    move |input: TokenStream<'a>| {
        match input.next() {
            Some((res, next_input)) => match res.kind() {
                TokenKind::Integer(i) => {
                    Ok((next_input, i, res.span()))
                }
                _ => Err(ParserError::new()
                    .message(format!("unexpected token: `{:?}`", res.kind()))
                    .span(res.span())
                    .build())
            },
            None => Err(ParserError::new()
                .message("unexpected eof".to_string())
                .span(input.last_span())
                .build())
        }
    }
}

pub fn parse_operator<'a>() -> impl Parser<'a, BinOperator> {
    move |input: TokenStream<'a>| {
        match input.next() {
            Some((res, next_input)) => {
                let op = match res.kind() {
                    TokenKind::Plus => BinOperator::Plus,
                    TokenKind::Minus => BinOperator::Minus,
                    TokenKind::Multiply => BinOperator::Multiply,
                    TokenKind::Divide => BinOperator::Divide,
                    TokenKind::Exp => BinOperator::Exp,
                    TokenKind::And => BinOperator::And,
                    TokenKind::Or => BinOperator::Or,
                    TokenKind::EqualsEquals => BinOperator::EqualsEquals,
                    TokenKind::NotEquals => BinOperator::NotEquals,
                    TokenKind::Less => BinOperator::Less,
                    TokenKind::LessEquals => BinOperator::LessEquals,
                    TokenKind::Greater => BinOperator::Greater,
                    TokenKind::GreaterEquals => BinOperator::GreaterEquals,
                    TokenKind::Colon => BinOperator::AppendSet,
                    _ => return Err(ParserError::new()
                        .message(format!("unexpected token: `{:?}`", res.kind()))
                        .span(res.span())
                        .build())
                };

                Ok((next_input, op, res.span()))
            }
            None => Err(ParserError::new()
                .message("unexpected eof".to_string())
                .span(input.last_span())
                .build())
        }
    }
}