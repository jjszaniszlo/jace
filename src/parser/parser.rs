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

impl <'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a [Token]) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a [Token]) -> ParseResult<'a, Output> {
        self(input)
    }
}

fn match_token<'a>(expected: Token)
    -> impl Parser<'a, ()> {
    move |toks: &'a [Token]| match toks.get(0) {
        Some(tok) if *tok == expected => {
            Ok((&toks[1..], ()))
        }
        _ => Err(ParserError::CouldNotMatchToken)
    }
}

fn match_identifier(input: &[Token]) -> ParseResult<Token> {
    match input.iter().next() {
        Some(t) => match t {
            Token::Identifier(s) => Ok((&input[1..], t.clone())),
            _ => Err(ParserError::CouldNotMatchToken)
        },
        _ => Err(ParserError::CouldNotMatchToken)
    }
}

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2)
    -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(next, r1)| {
            parser2.parse(next)
                .map(|(last, r2)| (last, (r1, r2)))
        })
    }
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F)
    -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input|
        parser.parse(input)
            .map(|(next, result)| (next, map_fn(result)))
}

fn left<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(p1, p2), |(l, r)| l)
}

fn right<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(p1, p2), |(l, r)| r)
}

 #[test]
fn pair_combinator() {
    let assign = left(
    match_identifier,
    match_token(Token::InferredEquals));

    let toks = vec![
        Token::Identifier("num".to_string()),
        Token::InferredEquals,
        Token::Integer(2),
    ];
    assert_eq!(
        Ok((&[Token::Integer(2)] as &[Token], Token::Identifier("num".to_string()))),
        assign.parse(&toks));
}
