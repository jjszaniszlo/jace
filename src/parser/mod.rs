mod ast;
mod parser;
mod ptr;
mod tests;

use crate::lexer::token::Token;
use parser::*;

// Parser entry point.
pub fn parse<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Module> {
    parse_module().parse(input)
}
