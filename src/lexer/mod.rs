use thiserror::Error;

pub mod token;
use crate::lexer::token::Token;

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Invalid char @ {line:?}")]
    InvalidCharAt {
        line: usize,
    },

    #[error("Unexpected EOF")]
    UnexpectedEOF,

    #[error("Match error in consume_while")]
    MatchError,
}



pub fn lex_identifier(src: &str) -> Result<(Token, usize), LexerError> {
    match src.chars().next() {
        Some(ch) if ch.is_digit(10) => return Err(LexerError::InvalidCharAt { line:0 }),
        None => return Err(LexerError::UnexpectedEOF),
        _ => {},
    }

    let (consumed_string, read_bytes) = consume_while(src, |ch| ch == '_' || ch.is_alphanumeric())?;

    let tok = Token::from(consumed_string);
    Ok((tok, read_bytes))
}

fn consume_while<F>(src: &str, mut pred: F) -> Result<(&str, usize), LexerError>
    where F: FnMut(char) -> bool {
    let mut pos = 0;

    for ch in src.chars() {
        if !pred(ch) {
            break;
        }
        pos += ch.len_utf8()
    }

    if pos == 0 {
        Err(LexerError::MatchError)
    } else {
        Ok((&src[..pos], pos))
    }
}
