use std::{collections::VecDeque, sync::Arc};

use crate::lexer::token::Token;

use super::*;

#[derive(Debug, Clone, PartialEq)]
enum ParserError {
    CouldNotMatchToken,
}

type ParseResult<'a, Output> = Result<(&'a [Token], Output), ParserError>;

trait Parser<'a, Output> {
    fn parse(&self, input: &'a [Token]) -> ParseResult<'a, Output>;
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a [Token]) -> ParseResult<'a, Output>,
{
    fn parse(&self, input: &'a [Token]) -> ParseResult<'a, Output> {
        self(input)
    }
}

fn match_token<'a>(expected: Token) -> impl Parser<'a, ()> {
    move |toks: &'a [Token]| match toks.get(0) {
        Some(tok) if *tok == expected => Ok((&toks[1..], ())),
        _ => Err(ParserError::CouldNotMatchToken),
    }
}

fn match_identifier<'a>(input: &'a [Token]) -> ParseResult<'a, &'a Token> {
    match input.get(0) {
        Some(t) => match t {
            Token::Identifier(_) => Ok((&input[1..], t)),
            _ => Err(ParserError::CouldNotMatchToken),
        },
        None => Err(ParserError::CouldNotMatchToken),
    }
}

fn sequence<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(next, r1)| {
            parser2.parse(next).map(|(last, r2)| (last, (r1, r2)))
        })
    }
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| parser.parse(input).map(|(next, result)| (next, map_fn(result)))
}

fn left<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(sequence(p1, p2), |(l, _)| l)
}

fn right<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(sequence(p1, p2), |(_, r)| r)
}

fn or<'a, P1, P2, R>(p1: P1, p2: P2) -> impl Parser<'a, R>
where
    P1: Parser<'a, R>,
    P2: Parser<'a, R>,
{
    move |input| {
        p1.parse(input).or_else(|_| p2.parse(input))
    }
}

#[test]
fn left_combinator() {
    let toks = vec![
        Token::from("num"),
        Token::InferredEquals,
        Token::Integer(2),
    ];

    let assign = left(match_identifier, match_token(Token::InferredEquals));

    assert_eq!(
        Ok((&[Token::Integer(2)] as &[Token], &Token::from("num"))),
        assign.parse(&toks)
    );
}

#[test]
fn right_combinator() {
    let toks = vec![
        Token::from("num"),
        Token::InferredEquals,
        Token::Integer(2),
    ];

    let assign = right(match_identifier, match_token(Token::InferredEquals));

    assert_eq!(
        Ok((&[Token::Integer(2)] as &[Token], ())),
        assign.parse(&toks)
    );
}

#[test]
fn or_combinator() {
    let toks1= vec![
        Token::TypeKeyword,
    ];

    let toks2= vec![
        Token::ClassKeyword,
    ];

    let assign = or(
        match_token(Token::TypeKeyword),
        match_token(Token::ClassKeyword)
    );

    assert_eq!(
        Ok((&[] as &[Token], ())),
        assign.parse(&toks1)
    );

    assert_eq!(
        Ok((&[] as &[Token], ())),
        assign.parse(&toks2)
    );
}

#[test]
fn sequence_combinator() {
}
