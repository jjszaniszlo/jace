use std::{collections::HashMap, usize};

use crate::lexer::lexer::LexerIterator;

mod ast;
mod ptr;
mod expr;

pub struct Parser<'a> {
    lexer: LexerIterator<'a>,

    // parse time custom operator precedence
    infix_precedence: HashMap<String, (usize, usize)>,
    prefix_precedence: HashMap<String, usize>,
    postfix_precedence: HashMap<String, usize>,
}

fn get_default_operator_hms() -> (HashMap<String, usize>, HashMap<String, (usize, usize)>, HashMap<String, usize>) {
    let prefix_hm = HashMap::new();
    let infix_hm = HashMap::new();
    let postfix_hm = HashMap::new();

    (prefix_hm, infix_hm, postfix_hm)
}

impl<'a> Parser<'a> {
    pub fn new(lexer: LexerIterator<'a>) -> Self {
        let (pre, infix, post) = get_default_operator_hms();
        Self {
            lexer,
            prefix_precedence: pre,
            infix_precedence: infix,
            postfix_precedence: post,
    
        }
    }
}
