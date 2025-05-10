mod ast;
mod parser;
mod combinator;
mod error;
mod ptr;
mod tests;
mod expr;
mod def;
mod stmt;

pub mod prelude {
    pub use crate::{parser::ast::*, parser::parser::*};
}

use crate::lexer::token::Token;
use parser::*;
use crate::parser::def::parse_module;

// Parser entry point.
pub fn parse(input: &[Token]) -> Output<ast::Module> {
    parse_module().parse(input)
}
