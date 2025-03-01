use super::{error::*, Parser};
use crate::parser::ast::AstSpan;
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
    move |input: &mut TokenStream<'a>| {
        let start = input.checkpoint();
        match p1.parse_next(input) {
            Ok((r1, s1)) => {
                match p2.parse_next(input) {
                    Ok((r2, s2)) => Ok(((r1, r2), s1.start..s2.end)),
                    Err(err) => {
                        input.reset(&start);
                        Err(err)
                    },
                }
            }
            Err(err) => {
                input.reset(&start);
                Err(err)
            },
        }
    }
}

pub fn or_n<'a, Out>(parsers: Vec<BoxedParser<'a, Out>>) -> impl Parser<'a, Out>
where
    Out: 'a,
{
    move |input: &mut TokenStream<'a>| {
        let checkpoint = input.checkpoint();
        for p in &parsers {
            match p.parse_next(input) {
                Ok((o, s)) => {
                    return Ok((o, s))
                }
                Err(ErrorType::Unrecoverable(err)) => return Err(ErrorType::Unrecoverable(err)),
                Err(_) => {
                    input.reset(&checkpoint);
                }
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
    move |input: &mut TokenStream<'a>|
        parser.parse_next(input)
            .map(|(result, s)| (map_fn(result, s.clone()), s))
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
    move |input: &mut TokenStream<'a>| {
        let start = input.checkpoint();
        match p1.parse_next(input) {
            Ok(result) => Ok(result),
            Err(ErrorType::Unrecoverable(err)) => Err(ErrorType::Unrecoverable(err)),
            Err(_) => {
                input.reset(&start);
                match p2.parse_next(input) {
                    Ok(result) => Ok(result),
                    Err(ErrorType::Unrecoverable(err)) => Err(ErrorType::Unrecoverable(err)),
                    Err(err2) => {
                        input.reset(&start);
                        Err(err2)
                    },
                }
            }
        }
    }
}

pub fn zero_or_more<'a, P, A>(p: P) -> BoxedParser<'a, Vec<A>>
where
    P: Parser<'a, A> + 'a,
    A: 'a,
{
    BoxedParser::new(move |input: &mut TokenStream<'a>| {
        let mut results = vec![];
        let mut spans = vec![];
        let mut start = input.checkpoint();

        loop {
            let parse_result = p.parse_next(input);
            match parse_result {
                Ok((result, s)) => {
                    spans.push(s);
                    results.push(result);
                    start = input.checkpoint();
                }
                Err(ErrorType::Unrecoverable(err)) => return Err(ErrorType::Unrecoverable(err)),
                Err(_) => {
                    input.reset(&start);
                    break
                }
            }
        }
        let first = spans.first().cloned().unwrap_or_else(|| 0..0);
        let span = spans.iter().fold(first, |prev, next| prev.start..next.end);

        Ok((results, span))
    })
}

pub fn one_or_more<'a, P, A>(p: P) -> BoxedParser<'a, Vec<A>>
where
    P: Parser<'a, A> + 'a,
    A: 'a,
{
    BoxedParser::new(move |input: &mut TokenStream<'a>| {
        let mut results = vec![];
        let mut spans = vec![];
        let mut start = input.checkpoint();

        match p.parse_next(input) {
            Ok((first, s)) => {
                spans.push(s);
                results.push(first);
                start = input.checkpoint();
            }
            Err(ErrorType::Unrecoverable(err)) => return Err(ErrorType::Unrecoverable(err)),
            Err(err) => {
                input.reset(&start);
                return Err(err)
            }
        }

        loop {
            let parse_result = p.parse_next(input);
            match parse_result {
                Ok((result, s)) => {
                    spans.push(s);
                    results.push(result);
                    start = input.checkpoint();
                }
                Err(ErrorType::Unrecoverable(err)) => return Err(ErrorType::Unrecoverable(err)),
                Err(_) => {
                    input.reset(&start);
                    break
                }
            }
        }

        let first = spans.first().cloned().unwrap_or_else(|| 0..0);
        let span = spans.iter().fold(first, |prev, next| prev.start..next.end);

        Ok((results, span))
    })
}

pub fn zero_or_one<'a, P, A>(p: P) -> BoxedParser<'a, Option<A>>
where
    P: Parser<'a, A> + 'a,
    A: 'a,
{
    BoxedParser::new(move |input: &mut TokenStream<'a>| {
        let start = input.checkpoint();
        match p.parse_next(input) {
            Ok((result, s)) => Ok((Some(result), s)),
            Err(ErrorType::Unrecoverable(err)) => return Err(ErrorType::Unrecoverable(err)),
            Err(_) => {
                input.reset(&start);
                Ok((None, input.last_span()))
            }
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
    move |input: &mut TokenStream<'a>| {
        let start = input.checkpoint();
        match p.parse_next(input) {
            Ok((a, s)) => {
                input.reset(&start);
                Err(
                    ErrorType::Recoverable(
                        ParserError::new()
                            .message(format!("unexpected parser output: `{:?}`", a))
                            .span(s)
                            .build()))
            },
            Err(_) => {
                Ok(((), input.last_span()))
            },
        }
    }
}

// transforms recoverable errors into unrecoverable errors.
pub fn cut<'a, P, A>(p: P) -> impl Parser<'a, A>
where
    P: Parser<'a, A> + 'a,
    A: 'a,
{
    move |input: &mut TokenStream<'a>| {
        match p.parse_next(input) {
            Ok((a, s)) => Ok((a, s)),
            Err(ErrorType::Recoverable(err)) => Err(ErrorType::Unrecoverable(err)),
            Err(e) => Err(e),
        }
    }
}
