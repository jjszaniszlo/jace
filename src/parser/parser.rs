use super::combinator::*;
use crate::parser::ast::*;
use crate::parser::tokenstream::{TokenResult, TokenStream};
use crate::{lexer::token::TokenKind, parser::error::*};
use std::ops::Range;

pub type Output<'a, Out> = miette::Result<(TokenStream<'a>, Out, Range<usize>), ErrorType>;

pub trait Parser<'a, Out> {
    fn parse(&self, input: TokenStream<'a>) -> Result<Out, ParserError> {
        match self.parse_next(input) {
            Ok((_, out, _)) => Ok(out),
            Err(e) => match e {
                ErrorType::Incomplete => todo!(),
                ErrorType::Unrecoverable(e) => {
                    println!("{:?}", e);
                    Err(e)
                }
                ErrorType::Recoverable(e) => Err(e),
            }
        }
    }

    fn parse_next(&self, input: TokenStream<'a>) -> Output<'a, Out>;

    fn map<Func, NewOut>(self, map_fn: Func) -> BoxedParser<'a, NewOut>
    where
        Self: Sized + 'a,
        Out: 'a,
        NewOut: 'a,
        Func: Fn(Out, Range<usize>) -> NewOut + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }
}

impl<'a, F, Out> Parser<'a, Out> for F
where
    F: Fn(TokenStream<'a>) -> Output<'a, Out>,
{
    fn parse_next(&self, input: TokenStream<'a>) -> Output<'a, Out> {
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
    fn parse_next(&self, input: TokenStream<'a>) -> Output<'a, Out> {
        self.parser.parse_next(input)
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
                ErrorType::Recoverable(
                    ParserError::new()
                        .message(format!("unexpected token: `{:?}`", res.kind()))
                        .span(res.span())
                        .build())),
            None => Err(ErrorType::Incomplete)
        }
    }
}

pub fn parse_identifier<'a>() -> impl Parser<'a, Identifier> {
    move |input: TokenStream<'a>| {
        match input.next() {
            Some((res, next_input)) => {
                match res.kind() {
                    TokenKind::Identifier(ident) => {
                        Ok((next_input, Identifier(ident, res.span()), res.span()))
                    }
                    _ => Err(
                        ErrorType::Recoverable(
                            ParserError::new()
                                .message(format!("unexpected token: `{:?}`", res.kind()))
                                .span(res.span())
                                .build()))
                }
            }
            None => Err(ErrorType::Incomplete)
        }
    }
}

pub fn parse_literal<'a>() -> impl Parser<'a, Literal> {
    move |input: TokenStream<'a>| {
        match input.next() {
            Some((res, next_input)) => {
                let lit = match res.kind() {
                    TokenKind::Bool(b) => Literal::Bool(b, res.span()),
                    TokenKind::Integer(i) => Literal::Integer(i, res.span()),
                    TokenKind::String(s) => Literal::String(s, res.span()),
                    TokenKind::Float(f) => Literal::Float(f, res.span()),

                    _ => return Err(
                        ErrorType::Recoverable(
                            ParserError::new()
                                .message(format!("unexpected token: `{:?}`", res.kind()))
                                .span(res.span())
                                .build()))
                };

                Ok((next_input, lit, res.span()))
            }
            None => Err(ErrorType::Incomplete)
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
                _ => Err(
                    ErrorType::Recoverable(
                        ParserError::new()
                            .message(format!("unexpected token: `{:?}`", res.kind()))
                            .span(res.span())
                            .build()))
            },
            None => Err(ErrorType::Incomplete)
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
                    _ => return Err(
                        ErrorType::Recoverable(
                            ParserError::new()
                                .message(format!("unexpected token: `{:?}`", res.kind()))
                                .span(res.span())
                                .build()))
                };

                Ok((next_input, op, res.span()))
            }
            None => Err(ErrorType::Incomplete)
        }
    }
}
