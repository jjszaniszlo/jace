use crate::parser::BoxedParser;
use super::{error::ParserError, Parser};

pub fn pair<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        p1.parse(input).and_then(|(next_input, result1)| {
            p2.parse(next_input)
                .map(|(last_input, result2)| (last_input, (result1, result2)))
        })
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
        for p in &parsers {
            if let Ok(result) = p.parse(input) {
                return Ok(result);
            }
        }
        Err(ParserError::CouldNotMatchToken)
    }
}

pub fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A> + 'a,
    F: Fn(A) -> B + 'a,
{
    move |input| parser.parse(input).map(|(next, result)| (next, map_fn(result)))
}

pub fn left<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1> + 'a,
    P2: Parser<'a, R2> + 'a,
    R1: 'a,
    R2: 'a,
{
    pair(p1, p2).map( |(l, _)| l)
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

pub fn or<'a, P1, P2, R>(p1: P1, p2: P2) -> impl Parser<'a, R>
where
    P1: Parser<'a, R>,
    P2: Parser<'a, R>,
{
    move |input| match p1.parse(input) {
        ok @ Ok(_) => ok,
        Err(_) => p2.parse(input),
    }
}

pub fn zero_or_more<'a, P, A>(p: P) -> BoxedParser<'a, Vec<A>>
where
    P: Parser<'a, A> + 'a
{
    BoxedParser::new(move |mut input| {
        let mut results = vec![];
        while let Ok((next_input, result)) = p.parse(input) {
            if next_input == input {
                return Err(ParserError::CouldNotMatchToken);
            }
            results.push(result);
            input = next_input;
        }
        Ok((input, results))
    })
}

pub fn one_or_more<'a, P, A>(p: P) -> BoxedParser<'a, Vec<A>>
where
    P: Parser<'a, A> + 'a,
{
    BoxedParser::new(move |mut input| {
        let mut result = vec![];

        match p.parse(input) {
            Ok((next, first)) => {
                input = next;
                result.push(first);
            },
            Err(err) => return Err(err)
        }

        while let Ok((next_input, next_item)) = p.parse(input) {
            result.push(next_item);
            input = next_input;
        }

        Ok((input, result))
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
