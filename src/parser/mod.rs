mod ast;
mod parser;
mod ptr;
mod tests;

pub mod prelude {
}

use crate::lexer::token::TokenKind;
use parser::*;

// Parser entry point.
pub fn parse<'a>(input: &'a [TokenKind]) -> ParseResult<'a, (ast::Module, Vec<parser::ParserError>)> {
    parse_module().parse(input)
}
