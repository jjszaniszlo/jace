
pub trait IdentCounter {
    fn increment(&mut self);
}

#[derive(Debug)]
pub struct ParserState {
    ident_counter: usize,
}

impl ParserState {
    pub fn new() -> ParserState {
        Self {
            ident_counter: 0,
        }
    }

    pub fn increment_ident_counter(&mut self) {
        self.ident_counter += 1;
    }
}

impl IdentCounter for ParserState {
    fn increment(&mut self) {
        self.increment_ident_counter();
    }
}