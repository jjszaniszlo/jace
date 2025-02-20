use crate::{error::{Empty, ErrorType, ParseError, ParserError}, input::ParserInput, parser::Parser};



pub trait Choice<In, Out, Err> {
    fn choice(&self, input: &mut In) -> Result<Out, Err>;
}

pub fn choice<C, In, Out, Err>(mut choice: C) -> impl Parser<In, Out, Err>
where
    C: Choice<In, Out, Err>,
    In: ParserInput,
    Err: ParseError<In>,
{
    move |i: &mut In| choice.choice(i)
}

fn match_char<In, Err>(ch: char) -> impl Parser<In, (), Err>
where
    In: ParserInput + Compare<char>,
    Err: ParseError<In>,
{
    move |input: &mut In| {
        let start = input.checkpoint();
        if input.compare(&ch) {
            Ok(())
        } else {
            Err(ParserError::new(*input, start, Empty{}))
        }
    }
}

pub trait Compare<T> {
    fn compare(&self, other: &T) -> bool;
}

impl Compare<char> for &str {
    fn compare(&self, other: &char) -> bool {
        match self.chars().next() {
            Some(c) if c == *other => true,
            _ => false,
        }
    }
}
