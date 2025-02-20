pub trait ParserInput: Sized {
    type Item;

    fn next(&mut self) -> Option<Self::Item>;
    fn peek(&self) -> Option<Self::Item>;

    fn checkpoint(&self) -> ParserCheckpoint<Self>;
    fn reset_checkpoint(&mut self, checkpoint: &ParserCheckpoint<Self>);
}

pub struct ParserCheckpoint<I: ParserInput> {
    input: I,
}

impl ParserInput for &str {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.chars().next()?;
        *self = &self[1..];
        Some(c)
    }

    fn peek(&self) -> Option<Self::Item> {
        self.chars().next()
    }

    fn checkpoint(&self) -> ParserCheckpoint<Self> {
        ParserCheckpoint {
            input: self,
        }
    }

    fn reset_checkpoint(&mut self, checkpoint: &ParserCheckpoint<Self>) {
        *self = checkpoint.input
    }
}
