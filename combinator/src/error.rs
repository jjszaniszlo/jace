use std::ops::Range;

use crate::input::{ParserCheckpoint, ParserInput};

pub enum ErrorType<T> {
    // unrecoverable means that a branch has parsed far enough to determine what
    // kind of structure is being parsed for sure, and that the parser shouldn't
    // try other branches.
    Unrecoverable(T),

    // recoverable means that a branch has not parsed, so try another branch.
    Recoverable(T),
}

impl<T> ErrorType<T> {
    pub fn unrecoverable(self) -> Self {
        match self {
            Self::Unrecoverable(e) | Self::Recoverable(e) => Self::Unrecoverable(e),
        }
    }

    pub fn inner(err: Self) -> T {
        match err {
            Self::Recoverable(err) | Self::Unrecoverable(err) => err,
        }
    }
}

pub trait ParseError<I: ParserInput>: Sized {
    type Inner;

    fn inner(self) -> Result<Self::Inner, Self>;
}

impl <I: ParserInput, E: ParseError<I>> ParseError<I> for ErrorType<E> {
    type Inner = E;

    fn inner(self) -> Result<Self::Inner, Self> {
        match self {
            ErrorType::Unrecoverable(e) | ErrorType::Recoverable(e) => Ok(e),
        }
    }
}

pub struct ParserError<I, E> {
    input: I,
    inner: E,
}

impl<I: ParserInput, E: ParseError<I>> ParserError<I, E> {
    pub fn new(mut input: I, checkpoint: ParserCheckpoint<I>, inner: E) -> Self {
        input.reset_checkpoint(&checkpoint);
        Self {
            input,
            inner,
        }
    }
}

pub struct Empty;

impl<I: ParserInput> ParseError<I> for Empty {
    type Inner = ();

    fn inner(self) -> Result<Self::Inner, Self> {
        Ok(())
    }
}
