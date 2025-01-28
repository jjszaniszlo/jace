mod tests;
mod token;

use crate::lexer::token::Token;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Invalid char {}", ch)]
    InvalidChar {
        ch: char,
    },

    #[error("Unexpected EOF")]
    UnexpectedEOF,

    #[error("Match error in consume_while")]
    MatchError,

    #[error("Malformed float")]
    MalformedFloat,

    #[error("Malformed integer")]
    MalformedInteger,

    #[error("Malformed integer")]
    MalformedString,

    #[error("Invalid symbol")]
    InvalidSymbol,
}

pub fn lex_identifier(src: &str) -> Result<(Token, usize), LexerError> {
    match src.chars().next() {
        Some(ch) if ch.is_digit(10) => return Err(LexerError::InvalidChar {ch}),
        None => return Err(LexerError::UnexpectedEOF),
        _ => {},
    }

    let (consumed_string, read_bytes) = consume_while(src, |ch| Ok(ch == '_' || ch.is_alphanumeric()))?;

    let tok = match consumed_string {
        "type" => Token::TypeKeyword,
        "class" => Token::ClassKeyword,
        "instance" => Token::InstanceKeyword,
        "case" => Token::CaseKeyword,
        "if" => Token::IfKeyword,
        "then" => Token::ThenKeyword,
        "else" => Token::ElseKeyword,
        _ => Token::from(consumed_string)
    };

    Ok((tok, read_bytes))
}

pub fn lex_number(src: &str) -> Result<(Token, usize), LexerError> {
    // early float validity checks.
    let mut chars = src.chars();
    let float_starts_with_0 = match chars.next() {
        Some(ch) if ch == '0' => Ok(true),
        None => Err(LexerError::UnexpectedEOF),
        _ => Ok(false),
    }?;

    // a float that starts with 0, must be in the form 0.nnnn where n is a number 0-9.
    if float_starts_with_0 {
        match chars.next() {
            Some(ch) if ch == '.' => {},
            None => return Err(LexerError::UnexpectedEOF),
            Some(_) => return Err(LexerError::MalformedFloat),
        }
    }

    let mut found_dot = false;

    // a little bit ugly but should match all valid floats and integers.
    let (number, read_bytes) = consume_while(src, |ch| {
        if ch.is_digit(10) {
            Ok(true)
        } else if ch == '.' {
            if found_dot {
                Err(LexerError::MalformedFloat)
            } else {
                found_dot = true;
                Ok(true)
            }
        // don't let it absorb symbols or whitespace and fail.
        } else if ch.is_ascii_punctuation() || ch.is_whitespace() {
            Ok(false)
        // let everything else be aborbed for error cases.
        // (stuff like 123a or 123bar, should fail.)
        } else {
            Ok(true)
        }
    })?;

    // allowing it to absorb alphabetic chars in the last step allows for easy fail cases here.
    if found_dot {
        let float  = match number.parse::<f64>() {
            Ok(f) => Ok(f),
            Err(_) => Err(LexerError::MalformedFloat),
        }?;
        Ok((Token::from(float), read_bytes))
    } else {
        let integer  = match number.parse::<isize>() {
            Ok(i) => Ok(i),
            Err(_) => Err(LexerError::MalformedInteger),
        }?;
        Ok((Token::from(integer), read_bytes))
    }
}

fn lex_symbol_valid_single_only(ch: char) -> bool {
    match ch {
        '(' => true,
        ')' => true,
        '{' => true,
        '}' => true,
        '[' => true,
        ']' => true,
        '+' => true,
        '-' => true,
        '/' => true,
        '*' => true,
        _ => false,
    }
}

fn lex_symbol(src: &str) -> Result<(Token, usize), LexerError> {
    let mut cont = true;

    // parse single variables wihtout looking ahead, whilst continue parsing for possible symbol unions.
    // hack way of doing this, but it works.
    let (symbol, read_bytes) = consume_while(src, |ch| {
        if !cont {
            Ok(false)
        } else {
            if lex_symbol_valid_single_only(ch) {
                cont = false;
                Ok(true)
            } else {
                Ok(ch.is_ascii_punctuation())
            }
        }
    })?;

    match symbol {
        "=" => Ok((Token::Equals, read_bytes)),
        ":=" => Ok((Token::InferredEquals, read_bytes)),
        "==" => Ok((Token::EqualsEquals, read_bytes)),
        "!=" => Ok((Token::NotEquals, read_bytes)),
        "&&" => Ok((Token::And, read_bytes)),
        "||" => Ok((Token::Or, read_bytes)),
        ">" => Ok((Token::GreaterThan, read_bytes)),
        "<" => Ok((Token::LessThan, read_bytes)),
        ">=" => Ok((Token::GreaterEqualThan, read_bytes)),
        "<=" => Ok((Token::LessEqualThan, read_bytes)),
        ":" => Ok((Token::Colon, read_bytes)),
        "::" => Ok((Token::ColonColon, read_bytes)),
        "=>" => Ok((Token::FatArrow, read_bytes)),
        "," => Ok((Token::Comma, read_bytes)),
        "(" => Ok((Token::LeftParen, read_bytes)),
        ")" => Ok((Token::RightParen, read_bytes)),
        "{" => Ok((Token::LeftBrace, read_bytes)),
        "}" => Ok((Token::RightBrace, read_bytes)),
        "[" => Ok((Token::LeftBracket, read_bytes)),
        "]" => Ok((Token::RightBracket, read_bytes)),
        "|" => Ok((Token::Union, read_bytes)),
        "+" => Ok((Token::Plus, read_bytes)),
        "-" => Ok((Token::Minus, read_bytes)),
        "/" => Ok((Token::Divide, read_bytes)),
        "*" => Ok((Token::Multiply, read_bytes)),
        _ => Err(LexerError::InvalidSymbol),
    }
}

fn lex_string_allowed_whitespace(ch: char) -> bool {
    match ch {
        '\t' => true,
        ' ' => true,
        _ => false,
    }
}

fn lex_string(src: &str) -> Result<(Token, usize), LexerError> {
    // check if the string starts with a ".
    match src.chars().next() {
        Some(ch) if ch == '"' => {},
        None => return Err(LexerError::UnexpectedEOF),
        Some(_) => return Err(LexerError::InvalidSymbol),
    };

    // TODO: handle escape sequences.
    let (string, bytes_read) = consume_while(src, |ch| {
        if ch.is_ascii_whitespace() && !lex_string_allowed_whitespace(ch) {
            Err(LexerError::MalformedString)
        } else {
            Ok(true)
        }
    })?;

    Ok((Token::String(string.to_string()), bytes_read))
}


fn consume_while<F>(src: &str, mut pred: F) -> Result<(&str, usize), LexerError>
    where F: FnMut(char) -> Result<bool, LexerError> {
    let mut pos = 0;

    for ch in src.chars() {
        match pred(ch) {
            Ok(b) if !b => break,
            Err(err) => return Err(err),
            Ok(_) => {},
        }
        pos += ch.len_utf8()
    }

    if pos == 0 {
        Err(LexerError::MatchError)
    } else {
        Ok((&src[..pos], pos))
    }
}

fn skip_whitespace(src: &str) -> usize {
    match consume_while(src, |ch| Ok(ch.is_whitespace())) {
        Ok((_, bytes_read)) => bytes_read,
        _ => 0,
    }
}

fn lex_tokenize(src: &str) -> Result<(Token, usize), LexerError> {
    let ch = match src.chars().next() {
        Some(ch) => Ok(ch),
        None => Err(LexerError::UnexpectedEOF),
    }?;

    match ch {
        '"' => lex_string(src),
        '0'..='9' => lex_number(src),
        c if c != '_' && c.is_ascii_punctuation() => lex_symbol(src),
        c if c == '_' || c.is_alphabetic() => lex_identifier(src),
        invalid_char => Err(LexerError::InvalidChar {ch: invalid_char}),
    }
}

pub struct Lexer<'a> {
    pos: usize,
    src: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn init(src: &str) -> Lexer {
        Lexer {
            pos: 0,
            src
        }
    }

    pub fn next(&mut self) -> Result<Option<(Token, usize, usize)>, LexerError> {
        self.skip_whitespace();

        if self.src.is_empty() {
            Ok(None)
        } else {
            let start = self.pos;
            let (token, read_bytes) = lex_tokenize(self.src)?;
            self.consume(read_bytes);
            Ok(Some((token, start, self.pos)))
        }
    }

    pub fn skip_whitespace(&mut self) {
        self.consume(skip_whitespace(self.src));
    }

    pub fn consume (&mut self, bytes: usize) {
        self.src = &self.src[bytes..];
        self.pos += bytes;
    }
}

pub fn tokenize_into_vec(src: &str) -> Result<Vec<(Token, usize, usize)>, LexerError> {
    let mut lexer = Lexer::init(src);
    let mut tokens = Vec::new();

    while let Some(token) = lexer.next()? {
        tokens.push(token)
    }

    Ok(tokens)
}
