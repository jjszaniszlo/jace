use crate::{error::{ParseError, ParserError}, input::ParserInput};


pub trait Parser<In, Out, Err> {
    fn parse(&mut self, mut input: In) -> Result<Out, ParserError<In, Err>>
    where
        In: ParserInput,
        Err: ParseError<In>,
    {
        let start = input.checkpoint();
        let out = self.parse_next(&mut input)
            .map_err(|err| ParserError::new(input, start, err))?;

        Ok(out)
    }

    fn parse_next(&mut self, input: &mut In) -> Result<Out, Err>;
}

impl<In, Out, Err, F> Parser<In, Out, Err> for F
where
    F: FnMut(&mut In) -> Result<Out, Err>,
    In: ParserInput,
{
    fn parse_next(&mut self, input: &mut In) -> Result<Out, Err> {
        todo!()
    }
}

