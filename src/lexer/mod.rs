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

    #[error("Invalid symbol")]
    InvalidSymbol,
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

fn lex_symbol(src: &str) -> Result<(Token, usize), LexerError> {
    let (symbol, read_bytes) = consume_while(src, |ch| Ok(ch.is_ascii_punctuation()))?;

    match symbol {
        "=" => Ok((Token::Equals, read_bytes)),
        ":=" => Ok((Token::InferredEquals, read_bytes)),
        "==" => Ok((Token::EqualsEquals, read_bytes)),
        "!=" => Ok((Token::NotEquals, read_bytes)),
        "&&" => Ok((Token::And, read_bytes)),
        "||" => Ok((Token::Or, read_bytes)),
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

lexer_test!(lex_equals, lex_symbol, "=" => Token::Equals);
lexer_test!(lex_infered_equals, lex_symbol, ":=" => Token::InferredEquals);
lexer_test!(lex_equals_equals, lex_symbol, "==" => Token::EqualsEquals);
lexer_test!(lex_not_equals, lex_symbol, "!=" => Token::NotEquals);
lexer_test!(lex_and, lex_symbol, "&&" => Token::And);
lexer_test!(lex_or, lex_symbol, "||" => Token::Or);
lexer_test!(lex_colon, lex_symbol, ":" => Token::Colon);
lexer_test!(lex_colon_colon, lex_symbol, "::" => Token::ColonColon);
lexer_test!(lex_fat_arrow, lex_symbol, "=>" => Token::FatArrow);
lexer_test!(lex_comma, lex_symbol, "," => Token::Comma);
lexer_test!(lex_left_paren, lex_symbol, "(" => Token::LeftParen);
lexer_test!(lex_right_paren, lex_symbol, ")" => Token::RightParen);
lexer_test!(lex_left_brace, lex_symbol, "{" => Token::LeftBrace);
lexer_test!(lex_right_brace, lex_symbol, "}" => Token::RightBrace);
lexer_test!(lex_left_bracket, lex_symbol, "[" => Token::LeftBracket);
lexer_test!(lex_right_bracket, lex_symbol, "]" => Token::RightBracket);
lexer_test!(lex_union, lex_symbol, "|" => Token::Union);
lexer_test!(lex_plus, lex_symbol, "+" => Token::Plus);
lexer_test!(lex_minus, lex_symbol, "-" => Token::Minus);
lexer_test!(lex_divide, lex_symbol, "/" => Token::Divide);
lexer_test!(lex_multiply, lex_symbol, "*" => Token::Multiply);

