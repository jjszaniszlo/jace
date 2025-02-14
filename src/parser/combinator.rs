use std::ops::Deref;
use mlua::ffi::lua_WarnFunction;
use crate::err::Span;
use crate::lexer::token::TokenKind;
use super::{error::*, POut, Parser};
use crate::parser::BoxedParser;
use crate::parser::error::ParserError::UnexpectedParse;
use crate::parser::parser::match_token;

pub fn pair<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        match p1.parse(input) {
            Ok((next_input, r1, s1)) => match p2.parse(next_input) {
                Ok((last, r2, s2)) => Ok((last, (r1, r2), s1.combine(s2))),
                Err(err) => Err(err),
            }
            Err(err) => Err(err),
        }
    }
}

pub fn and_then<'a, P, F, A, B, NextP>(parser: P, f: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    NextP: Parser<'a, B>,
    F: Fn(A) -> NextP,
{
    move |input| match parser.parse(input) {
        Ok((next_input, result, _)) => f(result).parse(next_input),
        Err(err) => Err(err),
    }
}

pub fn or_n<'a, Out>(parsers: Vec<BoxedParser<'a, Out>>) -> impl Parser<'a, Out>
where
    Out: 'a,
{
    move |input| {
        for p in &parsers {
            match p.parse(input) {
                Ok(result) => return Ok(result),
                Err(_) => {},
            }
        }

        POut::err(ParserError::UnexpectedEOF)
    }
}

pub fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A> + 'a,
    F: Fn(A, Span) -> B + 'a,
{
    move |input|
        parser.parse(input)
            .map(|(next, result, span)| (next, map_fn(result, span.clone()), span))
}

pub fn left<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1> + 'a,
    P2: Parser<'a, R2> + 'a,
    R1: 'a,
    R2: 'a,
{
    pair(p1, p2).map(|(left, _), _| left)
}

pub fn right<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1> + 'a,
    P2: Parser<'a, R2> + 'a,
    R1: 'a,
    R2: 'a,
{
    pair(p1, p2).map( |(_, r), _| r)
}

// cuts result in that branch erroring immediately.  
// This is for when we know in a parser that the input should parse,
// but there is a user error.
pub fn or<'a, P1, P2, R>(p1: P1, p2: P2) -> impl Parser<'a, R>
where
    P1: Parser<'a, R>,
    P2: Parser<'a, R>,
{
    move |input| match p1.parse(input) {
        Ok(result) => Ok(result),
        Err(err1) => match p2.parse(input) {
            Ok(result) => Ok(result),
            Err(err2) => Err(err2),
        }
    }
}

pub fn zero_or_more<'a, P, A>(p: P) -> BoxedParser<'a, Vec<A>>
where
    P: Parser<'a, A> + 'a,
    A: 'a,
{
    BoxedParser::new(move |mut input| {
        let mut results = vec![];
        let mut spans = vec![];

        loop {
            let parse_result = p.parse(input);
            match parse_result {
                Ok((next_input, result, span)) => {
                    spans.push(span);
                    results.push(result);
                    input = next_input;
                }
                Err(_) => break,
            }
        }
        let mut final_span = Span(0, 0);
        if spans.len() > 1 {
            final_span =
                spans.first().cloned().unwrap()
                    .combine(spans.last().cloned().unwrap());
        } else if spans.len() == 1 {
            final_span = spans.first().cloned().unwrap();
        }

        Ok((input, results, final_span))
    })
}

pub fn one_or_more<'a, P, A>(p: P) -> BoxedParser<'a, Vec<A>>
where
    P: Parser<'a, A> + 'a,
    A: 'a,
{
    BoxedParser::new(move |mut input| {
        let mut results = vec![];
        let mut spans = vec![];

        match p.parse(input) {
            Ok((next, first, span)) => {
                spans.push(span);
                input = next;
                results.push(first);
            },
            Err(err) => return Err(err)
        }

        loop {
            let parse_result = p.parse(input);
            match parse_result {
                Ok((next_input, result, span)) => {
                    spans.push(span);
                    results.push(result);
                    input = next_input;
                }
                Err(_) => break,
            }
        }
        let mut final_span = Span(0, 0);
        if spans.len() > 1 {
            final_span =
                spans.first().cloned().unwrap()
                    .combine(spans.last().cloned().unwrap());
        } else if spans.len() == 1 {
            final_span = spans.first().cloned().unwrap();
        }

        Ok((input, results, final_span))
    })
}

pub fn zero_or_one<'a, P, A>(p: P) -> BoxedParser<'a, Option<A>>
where
    P: Parser<'a, A> + 'a,
    A: 'a,
{
    BoxedParser::new(move |input| {
        match p.parse(input) {
            Ok((next, result, span)) => Ok((next, Some(result), span)),
            Err(_) => Ok((input, None, Span(0, 0))),
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

pub fn surrounded<'a, Sur1, Take, Sur2, IgnRes1, Res, IgnRes2>(
    parser_begin: Sur1,
    parser_take: Take,
    parser_end: Sur2) -> impl Parser<'a, Res>
where
    Sur1: Parser<'a, IgnRes1> + 'a,
    Take: Parser<'a, Res> + 'a,
    Sur2: Parser<'a, IgnRes2> + 'a,
    IgnRes1: 'a,
    Res: 'a,
    IgnRes2: 'a,
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
    Out: 'a,
{
    move |input| {
        match p.parse(input) {
            Ok((_, _, span)) => Err(UnexpectedParse { span: span.into() }.into()),
            Err(_) => Ok((input, (), Span(0, 0))),
        }
    }
}
