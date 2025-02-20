use std::ops::Range;
use miette::SourceSpan;
use super::combinator::*;
use crate::parser::ast::*;
use crate::parser::tokenstream::{TokenResult, TokenStream};
use crate::{lexer::token::TokenKind, parser::{ast, error::*}};

pub type Output<'a, Out> = miette::Result<(Out, Range<usize>), ErrorType>;

pub trait Parser<'a, Out> {
    fn parse(&mut self, mut input: TokenStream<'a>) -> Result<Out, ParserError> {
        match self.parse_next(&mut input) {
            Ok( (out, _)) => Ok(out),
            Err(e) => match e {
                ErrorType::Incomplete => todo!(),
                ErrorType::Unrecoverable(e) => {
                    println!("{:?}", e);
                    Err(e)
                },
                ErrorType::Recoverable(e) => Err(e),
            }
        }
    }

    fn parse_next(&mut self, input: &mut TokenStream<'a>) -> Output<'a, Out>;

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
    F: FnMut(&mut TokenStream<'a>) -> Output<'a, Out>,
{
    fn parse_next(&mut self, input: &mut TokenStream<'a>) -> Output<'a, Out> {
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
    fn parse_next(&mut self, input: &mut TokenStream<'a>) -> Output<'a, Out> {
        self.parser.parse_next(input)
    }
}

pub fn match_token<'a>(expected: TokenKind) -> impl Parser<'a, ()> {
    move |input: &mut TokenStream<'a>| {
        let start = input.checkpoint();

        let result = match input.next() {
            Some(res)
            if res.kind() == expected => {
                Ok(((), res.span()))
            }
            Some(res) => Err(
                ErrorType::Recoverable(
                    ParserError::new()
                        .message(format!("unexpected token: `{:?}`", res.kind()))
                        .span(res.span())
                        .build())),
            None => Err(ErrorType::Incomplete)
        };

        match result {
            ok @ Ok(_) => ok,
            Err(e) => {
                input.restore_checkpoint(start);
                Err(e)
            }
        }
    }
}

pub fn parse_identifier<'a>() -> impl Parser<'a, Identifier> {
    move |input: &mut TokenStream<'a>| {
        let start = input.checkpoint();

        let result = match input.next() {
            Some(res) => {
                match res.kind() {
                    TokenKind::Identifier(ident) => {
                        Ok((Identifier(ident), res.span()))
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
        };

        match result {
            ok @ Ok(_) => ok,
            Err(e) => {
                input.restore_checkpoint(start);
                Err(e)
            }
        }
    }
}

pub fn parse_literal<'a>() -> impl Parser<'a, Literal> {
    move |input: &mut TokenStream<'a>| {
        let start = input.checkpoint();

        let result = match input.next() {
            Some(res) => {
                let lit = match res.kind() {
                    TokenKind::Bool(b) => Literal::Bool(b),
                    TokenKind::Integer(i) => Literal::Integer(i),
                    TokenKind::String(s) => Literal::String(s),
                    TokenKind::Float(f) => Literal::Float(f),

                    _ => return Err(
                        ErrorType::Recoverable(
                            ParserError::new()
                            .message(format!("unexpected token: `{:?}`", res.kind()))
                            .span(res.span())
                            .build()))
                };

                Ok((lit, res.span()))
            }
            None => Err(ErrorType::Incomplete)
        };

        match result {
            ok @ Ok(_) => ok,
            Err(e) => {
                input.restore_checkpoint(start);
                Err(e)
            }
        }
    }
}

pub fn parse_literal_integer<'a>() -> impl Parser<'a, usize> {
    move |input: &mut TokenStream<'a>| {
        let start = input.checkpoint();

        let result = match input.next() {
            Some(res) => match res.kind() {
                TokenKind::Integer(i) => {
                    Ok((i, res.span()))
                }
                _ => Err(
                    ErrorType::Recoverable(
                        ParserError::new()
                        .message(format!("unexpected token: `{:?}`", res.kind()))
                        .span(res.span())
                        .build()))
            },
            None => Err(ErrorType::Incomplete)
        };

        match result {
            ok @ Ok(_) => ok,
            Err(e) => {
                input.restore_checkpoint(start);
                Err(e)
            }
        }
    }
}

pub fn parse_operator<'a>() -> impl Parser<'a, BinOperator> {
    move |input: &mut TokenStream<'a>| {
        let start = input.checkpoint();

        let result = match input.next() {
            Some(res) => {
                let op = match res.kind() {
                    TokenKind::Plus => Ok(BinOperator::Plus),
                    TokenKind::Minus => Ok(BinOperator::Minus),
                    TokenKind::Multiply => Ok(BinOperator::Multiply),
                    TokenKind::Divide => Ok(BinOperator::Divide),
                    TokenKind::Exp => Ok(BinOperator::Exp),
                    TokenKind::And => Ok(BinOperator::And),
                    TokenKind::Or => Ok(BinOperator::Or),
                    TokenKind::EqualsEquals => Ok(BinOperator::EqualsEquals),
                    TokenKind::NotEquals => Ok(BinOperator::NotEquals),
                    TokenKind::Less => Ok(BinOperator::Less),
                    TokenKind::LessEquals => Ok(BinOperator::LessEquals),
                    TokenKind::Greater => Ok(BinOperator::Greater),
                    TokenKind::GreaterEquals => Ok(BinOperator::GreaterEquals),
                    TokenKind::Colon => Ok(BinOperator::AppendSet),
                    _ => Err(
                        ErrorType::Recoverable(
                            ParserError::new()
                            .message(format!("unexpected token: `{:?}`", res.kind()))
                            .span(res.span())
                            .build()))
                };

                match op {
                    Ok(op) => Ok((op, res.span())),
                    Err(e) => Err(e),
                }
            }
            None => Err(ErrorType::Incomplete)
        };

        match result {
            ok @ Ok(_) => ok,
            Err(e) => {
                input.restore_checkpoint(start);
                Err(e)
            }
        }
    }
}
