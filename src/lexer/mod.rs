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

macro_rules! lexer_test {
    (FAIL: $name:ident, $func:ident, $src:expr) => {
        #[cfg(test)]
        #[test]
        fn $name() {
            let got = $func($src);
            assert!(got.is_err(), "{:?} should error but is not!", got);
        }
    };
    ($name:ident, $func:ident, $src:expr => $should_be:expr) => {  
        #[cfg(test)]
        #[test]
        fn $name() {
            let (got, _bytes_read) = $func($src).unwrap();
            assert_eq!(got, $should_be, "Got {:?} but should be {:?}", $src, $should_be);
        }
    };
}

lexer_test!(lex_identifier_single_letter, lex_identifier, "F" => Token::from("F"));
lexer_test!(lex_identifier_underscore_first, lex_identifier, "_nice" => Token::from("_nice"));
lexer_test!(lex_identifier_underscore_throughout, lex_identifier, "_n_i_c_e" => Token::from("_n_i_c_e"));
lexer_test!(lex_identifier_number, lex_identifier, "nice4" => Token::from("nice4"));
lexer_test!(FAIL: lex_identifier_cant_start_with_number, lex_identifier, "1wow");
lexer_test!(lex_type_keyword, lex_identifier, "type" => Token::TypeKeyword);
lexer_test!(lex_class_keyword, lex_identifier, "class" => Token::ClassKeyword);
lexer_test!(lex_instance_keyword, lex_identifier, "instance" => Token::InstanceKeyword);
lexer_test!(lex_case_keyword, lex_identifier, "case" => Token::CaseKeyword);
lexer_test!(lex_if_keyword, lex_identifier, "if" => Token::IfKeyword);
lexer_test!(lex_then_keyword, lex_identifier, "then" => Token::ThenKeyword);
lexer_test!(lex_else_keyword, lex_identifier, "else" => Token::ElseKeyword);
