use super::{error::*, Parser};
use crate::parser::tokenstream::TokenStream;
use crate::parser::BoxedParser;
use std::fmt::Debug;
use std::ops::Range;

pub fn pair<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
    R1: 'a,
    R2: 'a,
{
    move |input: TokenStream<'a>| {
        match p1.parse_next(input) {
            Ok((next, r1, s1)) => {
                match p2.parse_next(next) {
                    Ok((last, r2, s2)) => Ok((last, (r1, r2), s1.start..s2.end)),
                    Err(err) => Err(err),
                }
            }
            Err(err) => Err(err),
        }
    }
}

pub fn or_n<'a, Out>(parsers: Vec<BoxedParser<'a, Out>>) -> impl Parser<'a, Out>
where
    Out: 'a,
{
    move |input: TokenStream<'a>| {
        for p in &parsers {
            match p.parse_next(input) {
                Ok((next, o, s)) => {
                    return Ok((next, o, s))
                }
                Err(ErrorType::Unrecoverable(err)) => return Err(ErrorType::Unrecoverable(err)),
                Err(_) => {}
            }
        }

        Err(
            ErrorType::Recoverable(
                ParserError::new()
                    .message("Could not parse any branch".to_string())
                    .build()))
    }
}

pub fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A> + 'a,
    F: Fn(A, Range<usize>) -> B + 'a,
{
    move |input: TokenStream<'a>|
        parser.parse_next(input)
            .map(|(next, result, s)| (next, map_fn(result, s.clone()), s))
}

pub fn left<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1> + 'a,
    P2: Parser<'a, R2> + 'a,
    R1: 'a,
    R2: 'a,
{
    pair(p1, p2).map(|(left, _), s| left)
}

pub fn right<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1> + 'a,
    P2: Parser<'a, R2> + 'a,
    R1: 'a,
    R2: 'a,
{
    pair(p1, p2).map(|(_, r), s| r)
}

// cuts result in that branch erroring immediately.  
// This is for when we know in a parser that the input should parse,
// but there is a user error.
pub fn or<'a, P1, P2, R>(p1: P1, p2: P2) -> impl Parser<'a, R>
where
    P1: Parser<'a, R>,
    P2: Parser<'a, R>,
{
    move |input: TokenStream<'a>| {
        match p1.parse_next(input) {
            Ok(result) => Ok(result),
            Err(ErrorType::Unrecoverable(err)) => Err(ErrorType::Unrecoverable(err)),
            Err(err1) => match p2.parse_next(input) {
                Ok(result) => Ok(result),
                Err(ErrorType::Unrecoverable(err)) => Err(ErrorType::Unrecoverable(err)),
                Err(err2) => Err(err2),
            }
        }
    }
}

pub fn zero_or_more<'a, P, A>(p: P) -> BoxedParser<'a, Vec<A>>
where
    P: Parser<'a, A> + 'a,
    A: 'a,
{
    BoxedParser::new(move |mut input: TokenStream<'a>| {
        let mut results = vec![];
        let mut spans = vec![];

        loop {
            let parse_result = p.parse_next(input);
            match parse_result {
                Ok((next, result, s)) => {
                    spans.push(s);
                    results.push(result);
                    input = next;
                }
                Err(ErrorType::Unrecoverable(err)) => return Err(ErrorType::Unrecoverable(err)),
                Err(_) => break,
            }
        }
        let first = spans.first().cloned().unwrap_or_else(|| 0..0);
        let span = spans.iter().fold(first, |prev, next| prev.start..next.end);

        Ok((input, results, span))
    })
}

pub fn one_or_more<'a, P, A>(p: P) -> BoxedParser<'a, Vec<A>>
where
    P: Parser<'a, A> + 'a,
    A: 'a,
{
    BoxedParser::new(move |mut input: TokenStream<'a>| {
        let mut results = vec![];
        let mut spans = vec![];

        match p.parse_next(input) {
            Ok((next, first, s)) => {
                spans.push(s);
                input = next;
                results.push(first);
            }
            Err(ErrorType::Unrecoverable(err)) => return Err(ErrorType::Unrecoverable(err)),
            Err(err) => return Err(err)
        }

        loop {
            let parse_result = p.parse_next(input);
            match parse_result {
                Ok((next, result, s)) => {
                    input = next;
                    spans.push(s);
                    results.push(result);
                }
                Err(ErrorType::Unrecoverable(err)) => return Err(ErrorType::Unrecoverable(err)),
                Err(_) => break,
            }
        }

        let first = spans.first().cloned().unwrap_or_else(|| 0..0);
        let span = spans.iter().fold(first, |prev, next| prev.start..next.end);

        Ok((input, results, span))
    })
}

pub fn zero_or_one<'a, P, A>(p: P) -> BoxedParser<'a, Option<A>>
where
    P: Parser<'a, A> + 'a,
    A: 'a,
{
    BoxedParser::new(move |input: TokenStream<'a>| {
        match p.parse_next(input) {
            Ok((next, result, s)) => Ok((next, Some(result), s)),
            Err(ErrorType::Unrecoverable(err)) => return Err(ErrorType::Unrecoverable(err)),
            Err(_) => Ok((input.clone(), None, input.last_span())),
        }
    })
}

pub fn terminated<'a, P, A>(parser_take: P, parser_terminator: P) -> impl Parser<'a, A>
where
    P: Parser<'a, A> + 'a,
    A: 'a,
{
    left(
        parser_take,
        parser_terminator)
}

pub fn surrounded<'a, P1, P2, P3, R>(
    parser_begin: P1,
    parser_take: P2,
    parser_end: P3) -> impl Parser<'a, R>
where
    P1: Parser<'a, ()> + 'a,
    P2: Parser<'a, R> + 'a,
    P3: Parser<'a, ()> + 'a,
    R: 'a,
{
    left(
        right(
            parser_begin,
            parser_take),
        parser_end)
}

pub fn not<'a, P, Out>(p: P) -> impl Parser<'a, ()>
where
    P: Parser<'a, Out> + 'a,
    Out: Debug + 'a,
{
    move |input: TokenStream<'a>| {
        match p.parse_next(input) {
            Ok((next, a, s)) =>
                Err(
                    ErrorType::Recoverable(
                        ParserError::new()
                            .message(format!("unexpected parser output: `{:?}`", a))
                            .span(s)
                            .build())),
            Err(_) => Ok((input, (), input.last_span())),
        }
    }
}

// transforms recoverable errors into unrecoverable errors.
pub fn cut<'a, P, A>(p: P) -> impl Parser<'a, A>
where
    P: Parser<'a, A> + 'a,
    A: 'a,
{
    move |input: TokenStream<'a>| {
        match p.parse_next(input) {
            Ok((next, a, s)) => Ok((next, a, s)),
            Err(ErrorType::Recoverable(err)) => Err(ErrorType::Unrecoverable(err)),
            Err(e) => Err(e),
        }
    }
}


#[cfg(test)]
mod tests {
use crate::parser::combinator::*;
use crate::parser::parser::{match_token, Parser};
use crate::parser::tokenstream::{TokenStream};
use crate::lexer::token::{Token, TokenKind};
use std::ops::Range;

fn dummy_token(kind: TokenKind, start: usize, length: usize) -> Token {
    Token::new(kind, start, length)
}

fn dummy_token_stream<'a>(tokens: &'a [Token]) -> TokenStream<'a> {
    TokenStream::new(tokens)
}

#[test]
fn test_pair() {
    let tokens = vec![
        dummy_token(TokenKind::Plus, 0, 1),
        dummy_token(TokenKind::Minus, 1, 1),
    ];

    let parser = pair(
        match_token(TokenKind::Plus),
        match_token(TokenKind::Minus),
    );

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_ok());
    let (_, (left, right), span) = result.unwrap();
    assert_eq!(left, ());
    assert_eq!(right, ());
    assert_eq!(span, 0..2);
}

#[test]
fn test_or() {
    let tokens = vec![dummy_token(TokenKind::Minus, 0, 1)];

    let parser = or(
        match_token(TokenKind::Plus),
        match_token(TokenKind::Minus),
    );

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_ok());
    let (_, output, span) = result.unwrap();
    assert_eq!(output, ());
    assert_eq!(span, 0..1);
}

#[test]
fn test_or_no_match() {
    let tokens = vec![dummy_token(TokenKind::Multiply, 0, 1)];

    let parser = or(
        match_token(TokenKind::Plus),
        match_token(TokenKind::Minus),
    );

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_err());
}

#[test]
fn test_zero_or_more() {
    let tokens = vec![
        dummy_token(TokenKind::Plus, 0, 1),
        dummy_token(TokenKind::Plus, 1, 1),
    ];
    let parser = zero_or_more(match_token(TokenKind::Plus));

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_ok());
    let (_, output, span) = result.unwrap();
    assert_eq!(output.len(), 2);
    assert_eq!(span, 0..2);
}

#[test]
fn test_zero_or_more_no_match() {
    let tokens = vec![dummy_token(TokenKind::Minus, 0, 1)];
    let parser = zero_or_more(match_token(TokenKind::Plus));

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_ok());
    let (_, output, span) = result.unwrap();
    assert!(output.is_empty());
    assert_eq!(span, 0..0);
}

#[test]
fn test_one_or_more() {
    let tokens = vec![
        dummy_token(TokenKind::Plus, 0, 1),
        dummy_token(TokenKind::Plus, 1, 1),
    ];
    let parser = one_or_more(match_token(TokenKind::Plus));

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_ok());
    let (_, output, span) = result.unwrap();
    assert_eq!(output.len(), 2);
    assert_eq!(span, 0..2);
}

#[test]
fn test_one_or_more_no_match() {
    let tokens = vec![dummy_token(TokenKind::Minus, 0, 1)];
    let parser = one_or_more(match_token(TokenKind::Plus));

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_err());
}

#[test]
fn test_left() {
    let tokens = vec![
        dummy_token(TokenKind::Plus, 0, 1),
        dummy_token(TokenKind::Minus, 1, 1),
    ];

    let parser = left(
        match_token(TokenKind::Plus),
        match_token(TokenKind::Minus),
    );

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_ok());
    let (_, output, span) = result.unwrap();
    assert_eq!(output, ());
    assert_eq!(span, 0..2);
}

#[test]
fn test_right() {
    let tokens = vec![
        dummy_token(TokenKind::Plus, 0, 1),
        dummy_token(TokenKind::Minus, 1, 1),
    ];

    let parser = right(
        match_token(TokenKind::Plus),
        match_token(TokenKind::Minus),
    );

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_ok());
    let (_, output, span) = result.unwrap();
    assert_eq!(output, ());
    assert_eq!(span, 0..2);
}

#[test]
fn test_map() {
    let tokens = vec![dummy_token(TokenKind::Plus, 0, 1)];
    let parser = match_token(TokenKind::Plus).map(|_, _| "mapped");

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_ok());
    let (_, output, span) = result.unwrap();
    assert_eq!(output, "mapped");
    assert_eq!(span, 0..1);
}

#[test]
fn test_not() {
    let tokens = vec![dummy_token(TokenKind::Minus, 0, 1)];
    let parser = not(match_token(TokenKind::Plus));

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_ok());
    let (_, output, span) = result.unwrap();
    assert_eq!(output, ());
    assert_eq!(span, 0..0);
}

#[test]
fn test_not_with_match() {
    let tokens = vec![dummy_token(TokenKind::Plus, 0, 1)];
    let parser = not(match_token(TokenKind::Plus));

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_err());
}

#[test]
fn test_surrounded() {
    let tokens = vec![
        dummy_token(TokenKind::LeftParen, 0, 1),
        dummy_token(TokenKind::Plus, 1, 1),
        dummy_token(TokenKind::RightParen, 2, 1),
    ];
    let parser = surrounded(
        match_token(TokenKind::LeftParen),
        match_token(TokenKind::Plus),
        match_token(TokenKind::RightParen),
    );

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_ok());
    let (_, output, span) = result.unwrap();
    assert_eq!(output, ());
    assert_eq!(span, 0..3);
}

#[test]
fn test_surrounded_missing_closing() {
    let tokens = vec![
        dummy_token(TokenKind::LeftParen, 0, 1),
        dummy_token(TokenKind::Plus, 1, 1),
    ];
    let parser = surrounded(
        match_token(TokenKind::LeftParen),
        match_token(TokenKind::Plus),
        match_token(TokenKind::RightParen),
    );

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_err());
}
}