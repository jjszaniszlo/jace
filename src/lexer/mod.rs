pub mod token;
pub mod lexer;

pub mod prelude {
    pub use crate::lexer::{lexer::Lexer, token::{Token, TokenKind}};
}
