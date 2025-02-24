mod ast;
mod parser;
mod combinator;
mod error;
mod ptr;
mod tests;
mod expr;
mod def;
mod stmt;
mod tokenstream;
mod typeparam;
mod state;

pub mod prelude {
    pub use crate::{parser::ast::*, parser::parser::*};
}

use crate::err::JaceError;
use crate::jace_file::JaceFile;
use crate::lexer::token::Token;
use crate::parser::def::parse_module;
use crate::parser::tokenstream::TokenStream;
use parser::*;
use std::sync::Arc;
use crate::parser::state::ParserState;

// Parser entry point.
pub fn parse(input: &[Token], src: JaceFile) -> Result<ast::Module, JaceError> {
    let binding = TokenStream::new(input);
    let x = match parse_module().parse(binding) {
        Ok(res) => Ok(res),
        Err(_) => Err(JaceError {
            src: Arc::new(src.contents().to_string()),
            diagnostics: vec![],
        })
    };
    x
}