pub mod token;
pub mod lexer;
mod tests;

pub mod prelude {
    pub use crate::lexer::{lexer::{Lexer, LexerIterator}, token::{Token, TokenKind}};
}
