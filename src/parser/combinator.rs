use super::parser::*;

pub fn pair<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        p1.parse(input).and_then(|(next, r1)| {
            p2.parse(next).map(|(last, r2)| (last, (r1, r2)))
        })
    }
}

pub fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| parser.parse(input).map(|(next, result)| (next, map_fn(result)))
}

pub fn left<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(p1, p2), |(l, _)| l)
}

pub fn right<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(p1, p2), |(_, r)| r)
}

pub fn or<'a, P1, P2, R>(p1: P1, p2: P2) -> impl Parser<'a, R>
where
    P1: Parser<'a, R>,
    P2: Parser<'a, R>,
{
    move |input| {
        p1.parse(input).or_else(|_| p2.parse(input))
    }
}

pub fn or_n<'a, P, R>(ps: Vec<P>) -> impl Parser<'a, R>
where
    P: Parser<'a, R>
{
    move |input| {
        ps.iter().fold(
            Err(ParserError::CouldNotMatchToken),
            |prev, curr| prev.or_else(|_| curr.parse(input))
        )
    }
}

pub fn zero_or_more<'a, P, A>(p: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>
{
    move |mut input| {
        let result: Vec<A> = std::iter::from_fn(|| {
            match p.parse(input) {
                Ok((next, result)) => {
                    input = next;
                    Some(result)
                },
                Err(_) => None,
            }
        })
        .collect();

        Ok((input, result))
    }
}

pub fn one_or_more<'a, P, A>(p: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let result: Vec<A> = std::iter::from_fn(|| {
            match p.parse(input) {
                Ok((next, result)) => {
                    input = next;
                    Some(result)
                },
                Err(_) => None,
            }
        })
        .collect();

        Ok((input, result))
    }
}

pub fn zero_or_one<'a, P, A>(p: P) -> impl Parser<'a, Option<A>> 
where
    P: Parser<'a, A>
{
    move |input| {
        match p.parse(input) {
            Ok((next, result)) => Ok((next, Some(result))),
            Err(_) => Ok((input, None)),
        }
    }
}
