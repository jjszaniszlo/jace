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

    #[error("Malformed float")]
    MalformedFloat,

    #[error("Malformed integer")]
    MalformedInteger,
}

pub fn lex_identifier(src: &str) -> Result<(Token, usize), LexerError> {
    match src.chars().next() {
        Some(ch) if ch.is_digit(10) => return Err(LexerError::InvalidCharAt { line:0 }),
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
        } else {
            Ok(true)
        }
    })?;

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

lexer_test!(lex_integer, lex_number, "123" => Token::from(123));
lexer_test!(lex_float, lex_number, "1.23" => Token::from(1.23));
lexer_test!(lex_float2, lex_number, "0.56" => Token::from(0.56));
lexer_test!(lex_float3, lex_number, "1000.35" => Token::from(1000.35));
lexer_test!(lex_float4, lex_number, "1432.356161" => Token::from(1432.356161));
lexer_test!(FAIL:lex_malformed_float, lex_number, "001.23");
lexer_test!(FAIL:lex_malformed_float2, lex_number, "1000.a");
lexer_test!(FAIL:lex_malformed_float3, lex_number, "5.a44f");
lexer_test!(FAIL:lex_malformed_integer, lex_number, "0123");
lexer_test!(FAIL:lex_malformed_integer2, lex_number, "0000123");
lexer_test!(FAIL:lex_malformed_integer3, lex_number, "1a23");
