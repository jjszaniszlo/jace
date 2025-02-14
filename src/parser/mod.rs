mod ast;
mod parser;
mod combinator;
mod error;
mod ptr;
mod tests;

pub mod prelude {
    pub use crate::{parser::parser::*, parser::error::*, parser::combinator::*, parser::ast::*};
}

use crate::lexer::token::Token;
use parser::*;

// Parser entry point.
pub fn parse<'a>(input: &'a [Token]) -> Output<'a, ast::Module> {
    parse_module().parse(input)
}
