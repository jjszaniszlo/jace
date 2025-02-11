use crate::{parser::BoxedParser, TokenKind};
use super::{error::{InnerError, ParserError}, Parser};

pub fn pair<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        match p1.parse(input) {
            Ok((next_input, r1)) => match p2.parse(next_input) {
                Ok((last, r2)) => Ok((last, (r1, r2))),
                Err(ParserError::UnrecoverableError(e)) =>
                    return Err(ParserError::UnrecoverableError(e)),
                Err(err) => Err(err),
            }
            Err(ParserError::UnrecoverableError(e)) =>
                return Err(ParserError::UnrecoverableError(e)),
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
        Ok((next_input, result)) => f(result).parse(next_input),
        Err(err) => Err(err),
    }
}

pub fn seq<'a, P, R>(parsers: Vec<P>) -> impl Parser<'a, Vec<R>> 
where
    P : Parser<'a, R>,
{
    move |input| {
        parsers.iter().fold(Ok((input, Vec::new())), |prev_results, parser| {
            prev_results.and_then(|(next_inputs, mut parser_outputs)| {
                parser.parse(next_inputs).map(|(next_inputs, result)| {
                    parser_outputs.push(result);
                    (next_inputs, parser_outputs)
                })
            })
        })
    }
}

pub fn or_n<'a, Out>(parsers: Vec<BoxedParser<'a, Out>>) -> impl Parser<'a, Out> 
where
    Out: 'a,
{
    move |input| {
        let mut errors = vec![];

        for p in &parsers {
            match p.parse(input) {
                Ok(result) => return Ok(result),
                Err(ParserError::UnrecoverableError(e)) =>
                    return Err(ParserError::UnrecoverableError(e)),
                Err(err) => errors.push(err),
            }
        }

        Err(InnerError::combined(errors).into())
    }
}

pub fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A> + 'a,
    F: Fn(A) -> B + 'a,
{
    move |input|
        parser.parse(input)
            .map(|(next, result)| (next, map_fn(result)))
}

pub fn left<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1> + 'a,
    P2: Parser<'a, R2> + 'a,
    R1: 'a,
    R2: 'a,
{
    pair(p1, p2).map(|(left, _)| left)
}

pub fn right<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1> + 'a,
    P2: Parser<'a, R2> + 'a,
    R1: 'a,
    R2: 'a,
{
    pair(p1, p2).map( |(_, r)| r)
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
        Err(ParserError::UnrecoverableError(e)) =>
            return Err(ParserError::UnrecoverableError(e)),
        Err(err1) => match p2.parse(input) {
            Ok(result) => Ok(result),
            Err(ParserError::UnrecoverableError(e)) =>
                return Err(ParserError::UnrecoverableError(e)),
            Err(err2) => Err(InnerError::combined(vec![err1, err2]).into()),
        }
    }
}

pub fn zero_or_more<'a, P, A>(p: P) -> BoxedParser<'a, Vec<A>>
where
    P: Parser<'a, A> + 'a
{
    BoxedParser::new(move |mut input| {
        let mut results = vec![];

        loop {
            let parse_result = p.parse(input);
            match parse_result {
                Ok((next_input, result)) => {
                    results.push(result);
                    input = next_input;
                }
                Err(ParserError::UnrecoverableError(e)) =>
                    return Err(ParserError::UnrecoverableError(e)),
                Err(_) => break,
            }
        }

        Ok((input, results))
    })
}

pub fn one_or_more<'a, P, A>(p: P) -> BoxedParser<'a, Vec<A>>
where
    P: Parser<'a, A> + 'a,
{
    BoxedParser::new(move |mut input| {
        let mut results = vec![];

        match p.parse(input) {
            Ok((next, first)) => {
                input = next;
                results.push(first);
            },
            Err(err) => return Err(err)
        }

        loop {
            let parse_result = p.parse(input);
            match parse_result {
                Ok((next_input, result)) => {
                    results.push(result);
                    input = next_input;
                }
                Err(ParserError::UnrecoverableError(e)) =>
                    return Err(ParserError::UnrecoverableError(e)),
                Err(_) => break,
            }
        }


        Ok((input, results))
    })
}

pub fn zero_or_one<'a, P, A>(p: P) -> BoxedParser<'a, Option<A>> 
where
    P: Parser<'a, A> + 'a
{
    BoxedParser::new(move |input| {
        match p.parse(input) {
            Ok((next, result)) => Ok((next, Some(result))),
            Err(_) => Ok((input, None)),
        }
    })
}

pub fn cut<'a, P, O>(parser: P, msg: &'static str) -> impl Parser<'a, O>
where
    P: Parser<'a, O>,
{
    move |input| match parser.parse(input) {
        Ok(result) => Ok(result),
        Err(err) => {
            let span = input.get(0).unwrap();
            Err(ParserError::unrecoverable(msg, err, span.1.into()))
        },
    }
}

pub fn context<'a, P, O>(context: &'static str, parser: P) -> impl Parser<'a, O>
where
    P: Parser<'a, O>,
{
    move |input| match parser.parse(input) {
        Ok(result) => Ok(result),
        Err(ParserError::UnrecoverableError(e)) => Err(ParserError::UnrecoverableError(e)),
        Err(err) => Err(ParserError::contextual(context, err)),
    }
}

pub fn recover<'a, P, O, F>(parser: P, fallback: F) -> impl Parser<'a, (O, Option<ParserError>)> where
    P: Parser<'a, O> + 'a,
    F: Fn() -> O + 'a,
    O: Clone + 'a,
{
    move |input| match parser.parse(input) {
        Ok((next, result)) => Ok((next, (result, None))),
        Err(err) => {
            Ok((input, (fallback(), Some(err))))
        }
    }
}

pub fn recover_sync<'a, P, O, F>(parser: P, fallback: F, tok: TokenKind) -> impl Parser<'a, (O, Option<ParserError>)> where
    P: Parser<'a, O> + 'a,
    F: Fn() -> O + 'a,
    O: Clone + 'a,
{
    move |mut input| match parser.parse(input) {
        Ok((next, result)) => Ok((next, (result, None))),
        Err(err) => {
            while let Some((token, rest)) = input.split_first() {
                if token.0 == tok {
                    input = rest;
                    break;
                }
                input = rest;
            }

            Ok((input, (fallback(), Some(err))))
        }
    }
}
