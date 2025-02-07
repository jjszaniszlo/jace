mod tests;
pub mod token;

use std::sync::LazyLock;

use crate::lexer::token::Token;
use thiserror::Error;

use regex::Regex;

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Invalid char {} @ {}:{}", ch, line, col)]
    InvalidChar {
        ch: char,
        line: usize,
        col: usize,
    },

    #[error("UnexpectedEOF")]
    UnexpectedEOF,

    #[error("Invalid char when parsing float {} @ {}:{}", ch, line, col)]
    InvalidCharFloat {
        ch: char,
        line: usize,
        col: usize,
    },

    #[error("Malformed float when tokenizing {}:{}", line, col)]
    MalformedFloat {
        line: usize,
        col : usize,
    },

    #[error("Malformed integer when tokenizing {}:{}", line, col)]
    MalformedInteger {
        line: usize,
        col : usize,
    },

    #[error("Malformed operator when tokenizing '{}' {}:{}", st,  line, col)]
    MalformedOperator {
        st : String,
        line: usize,
        col : usize,
    },

    #[error("Invalid string escape sequence when tokenizing {}:{}", line, col)]
    InvalidStringEscapeSequence {
        line: usize,
        col : usize,
    },

    #[error("Invalid char when parsing string {} @ {}:{}", ch, line, col)]
    InvalidCharString {
        ch: char,
        line: usize,
        col: usize,
    },

    #[error("Unterminated string! @ {}:{}", line, col)]
    UnterminatedString {
        line: usize,
        col: usize,
    },
}

type Line = usize;
type ColStart = usize;
type TokByteSize = usize;

type TokenResult = Result<(Token, Line, ColStart, TokByteSize), LexerError>;

fn lex_identifer(src: &str, line: usize, col: usize) -> TokenResult {
    let mut chars = src.chars();
    let mut identifier = String::new();

    while let Some(next_char) = chars.next() {
        match next_char {
            ch if ch.is_alphanumeric() || ch == '_' => identifier.push(ch),
            _ => break,
        };
    }

    match identifier.as_str() {
        "type" => Ok((Token::TypeKeyword, line, col, identifier.len())),
        "class" => Ok((Token::ClassKeyword, line, col, identifier.len())),
        "instance" => Ok((Token::InstanceKeyword, line, col, identifier.len())),
        "true" => Ok((Token::Bool(true), line, col, identifier.len())),
        "false" => Ok((Token::Bool(false), line, col, identifier.len())),
        "case" => Ok((Token::CaseKeyword, line, col, identifier.len())),
        "let" => Ok((Token::LetKeyword, line, col, identifier.len())),
        "in" => Ok((Token::InKeyword, line, col, identifier.len())),
        "if" => Ok((Token::IfKeyword, line, col, identifier.len())),
        "then" => Ok((Token::ThenKeyword, line, col, identifier.len())),
        "else" => Ok((Token::ElseKeyword, line, col, identifier.len())),
        "elseif" => Ok((Token::ElseIfKeyword, line, col, identifier.len())),
        "do" => Ok((Token::DoKeyword, line, col, identifier.len())),
        "def" => Ok((Token::DefKeyword, line, col, identifier.len())),
        _ => Ok((Token::from(identifier.clone()), line, col, identifier.len()))
    }
}

fn lex_number(src: &str, line: usize, col: usize) -> TokenResult {
    let mut chars = src.chars();
    let mut number = String::new();
    
    // this looks weird, but since the caller of lex_number guarantees this char to be between '0'
    // and '9', this basically pushes any number, but does something special if its '0'.
    let starts_with_zero = match chars.next() {
        Some(c @ '0'..='9') => {
            number.push(c);
            c == '0'
        },
        _ => false,
    };

    let mut is_float = false;

    while let Some(next_char) = chars.next() {
        match next_char {
            ch if ch.is_numeric() => 
                if starts_with_zero && !is_float {
                    return Err(LexerError::InvalidCharFloat { ch , line, col });
                } else {
                    number.push(ch)
                },
            ch if ch.is_alphabetic()  => return Err(LexerError::InvalidCharFloat { ch , line, col }),
            ch @ '.' => {
                if is_float {
                    return Err(LexerError::InvalidCharFloat { ch , line, col })
                } else {
                    number.push(ch);
                    is_float = true;
                }
            },
            _ => break,
        };
    }

    if is_float {
        match number.parse::<f64>() {
            Ok(f) => Ok((Token::from(f), line, col, number.len())),
            Err(_) => Err(LexerError::MalformedFloat { line, col }),
        }
    } else {
        match number.parse::<usize>() {
            Ok(f) => Ok((Token::from(f), line, col, number.len())),
            Err(_) => Err(LexerError::MalformedInteger { line, col }),
        }
    }
}


// Regex for potential custom operator creation within classes.
static RE_WRAPPED_OP: LazyLock<Regex> = LazyLock::new(|| Regex::new(
    r#"^\(([!@#$%^&*\-\+=<>?/~`|\\]*)\)$"#).unwrap());

fn lex_operator(src: &str, line: usize, col: usize) -> TokenResult {
    let mut chars = src.chars();
    let mut operator = String::new();

    while let Some(next_char) = chars.next() {
        match next_char {
            ch if ch.is_ascii_punctuation() => operator.push(ch),
            _ => break,
        }
    }

    // match longest matching operator
    while match operator.as_str() {
            "=="   | "!="   | "&&"   | "||"  | ">="  | "<=" | ">"    |
            "<"    | "+"    | "-"    | "/"   | "*"   | "^"  | "(==)" |
            "(!=)" | "(>=)" | "(<=)" | "(>)" | "(<)" | "(+)"| "(-)"  |
            "(/)"  | "(*)"  | "(^)"  | "="   | ":="  | ":"  | "::"   |
            "|"    | "=>"   | ","    | "."   | "("   | ")"  | "{"    |
            "}"    |  "["   | "]"    | "()"  | "!" => false,
            _ => true,
    } { operator.pop(); }

    match operator.as_str() {
        "==" => Ok((Token::EqualsEquals, line, col, operator.len())),
        "!=" => Ok((Token::NotEquals, line, col, operator.len())),
        "&&" => Ok((Token::And, line, col, operator.len())),
        "||" => Ok((Token::Or, line, col, operator.len())),
        ">=" => Ok((Token::GreaterEquals, line, col, operator.len())),
        "<=" => Ok((Token::LessEquals, line, col, operator.len())),
        ">" => Ok((Token::Greater, line, col, operator.len())),
        "<" => Ok((Token::Less, line, col, operator.len())),
        "+" => Ok((Token::Plus, line, col, operator.len())),
        "-" => Ok((Token::Minus, line, col, operator.len())),
        "/" => Ok((Token::Divide, line, col, operator.len())),
        "*" => Ok((Token::Multiply, line, col, operator.len())),
        "^" => Ok((Token::Exp, line, col, operator.len())),

        "(==)" => Ok((Token::WrappedEqualsEquals, line, col, operator.len())),
        "(!=)" => Ok((Token::WrappedNotEquals, line, col, operator.len())),
        "(>=)" => Ok((Token::WrappedGreaterEquals, line, col, operator.len())),
        "(<=)" => Ok((Token::WrappedLessEquals, line, col, operator.len())),
        "(>)" => Ok((Token::WrappedGreater, line, col, operator.len())),
        "(<)" => Ok((Token::WrappedLess, line, col, operator.len())),
        "(+)" => Ok((Token::WrappedPlus, line, col, operator.len())),
        "(-)" => Ok((Token::WrappedMinus, line, col, operator.len())),
        "(/)" => Ok((Token::WrappedDivide, line, col, operator.len())),
        "(*)" => Ok((Token::WrappedMultiply, line, col, operator.len())),
        "(^)" => Ok((Token::WrappedExp, line, col, operator.len())),

        "=" => Ok((Token::Equals, line, col, operator.len())),
        ":=" => Ok((Token::InferredEquals, line, col, operator.len())),
        ":" => Ok((Token::Colon, line, col, operator.len())),
        "::" => Ok((Token::ColonColon, line, col, operator.len())),
        "|" => Ok((Token::Union, line, col, operator.len())),
        "=>" => Ok((Token::FatArrow, line, col, operator.len())),
        "," => Ok((Token::Comma, line, col, operator.len())),
        "." => Ok((Token::Dot, line, col, operator.len())),
        "(" => Ok((Token::LeftParen, line, col, operator.len())),
        ")" => Ok((Token::RightParen, line, col, operator.len())),
        "{" => Ok((Token::LeftBrace, line, col, operator.len())),
        "}" => Ok((Token::RightBrace, line, col, operator.len())),
        "[" => Ok((Token::LeftBracket, line, col, operator.len())),
        "]" => Ok((Token::RightBracket, line, col, operator.len())),

        "()" => Ok((Token::ProcType, line, col, operator.len())),
        "!" => Ok((Token::Bang, line, col, operator.len())),
        str => Err(LexerError::MalformedOperator {st: str.to_string(), line, col})
    }
}

fn lex_string_literal(src: &str, line: usize, col: usize) -> TokenResult {
    let mut chars = src.chars();
    let mut string_literal = String::new();

    // add first "
    match chars.next() {
        Some('"') => string_literal.push('"'),
        Some(ch) => return Err(LexerError::InvalidCharString {ch, line, col}),
        None => return Err(LexerError::UnexpectedEOF),
    }

    let mut escaped = false;
    let mut terminated = false;
    while let Some(next_char) = chars.next() {
        match next_char {
            ch @ '"' => {
                string_literal.push(ch);
                if !escaped {
                    terminated = true;
                    break;
                }
            }
            ch @ '\\' => {
                escaped = true;
                string_literal.push(ch);
            }
            // valid escape codes.
            ch @ ('a' |'b' | 'f' | 'n' |'r' |'t' | 'v' | '\''
                | '?' |'0') => {
                if escaped {
                    escaped = false;
                }
                string_literal.push(ch);
            },
            ch if ch.is_ascii_control() && ch != '\t' => return Err(LexerError::InvalidCharString {ch, line, col}),
            ch => {
                if escaped {
                    return Err(LexerError::InvalidStringEscapeSequence {line, col});
                }
                string_literal.push(ch);
            }
        }
    }

    if !terminated {
        return Err(LexerError::UnterminatedString{line, col});
    }

    // this shouldn't mutate the original string's length, but just in case, lets cache it.
    // this just removes the first and last quotes of the string.
    let bytes_read = string_literal.len();
    let mut str_chars = string_literal.chars();
    str_chars.next();
    str_chars.next_back();

    Ok((Token::String(str_chars.collect()), line, col, bytes_read))
}

fn lex_tokenize(src: &str, line: usize, col: usize) -> TokenResult {
    match src.chars().next() {
        Some('"') => lex_string_literal(src, line, col),
        Some('0'..='9') => lex_number(src, line, col),
        Some(c) if c != '_' && c.is_ascii_punctuation() => lex_operator(src, line, col),
        Some(c) if c == '_' || c.is_alphabetic() => lex_identifer(src, line, col),
        Some(ch) => Err(LexerError::InvalidChar {ch, line, col}),
        None => Err(LexerError::UnexpectedEOF),
    }
}

pub struct Lexer<'a> {
    src: &'a str,
    line: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    pub fn init(src: &str) -> Lexer {
        Lexer {
            src, 
            line: 0,
            col: 0,
        }
    }

    pub fn next(&mut self) -> Result<Option<(Token, Line, ColStart, usize)>, LexerError> {
        self.skip_whitespace();

        if self.src.is_empty() {
            Ok(None)
        } else {
            // pass in line and col to tokenizer for nice-ish tokenization errors.
            let (token, line, col, byte_size) = lex_tokenize(self.src, self.line, self.col)?;
            self.consume(byte_size);
            self.col += byte_size;
            Ok(Some((token, line, col, byte_size)))
        }
    }

    pub fn skip_whitespace(&mut self) {
        let mut chars = self.src.chars();
        let mut bytes_read = 0;

        while let Some(next_char) = chars.next() {
            match next_char {
                c if !c.is_whitespace() => break,
                c if c == '\n' => {
                    self.col = 0;
                    self.line += 1;
                },
                c => self.col += c.len_utf8(),
            }
            bytes_read += next_char.len_utf8();
        }

        self.consume(bytes_read);
    }

    pub fn consume (&mut self, bytes: usize) {
        self.src = &self.src[bytes..];
    }
}

pub fn tokenize_into_vec(src: &str) -> Result<Vec<(Token, usize, usize, usize)>, LexerError> {
    let mut lexer = Lexer::init(src);
    let mut tokens = Vec::new();

    while let Some(token) = lexer.next()? {
        tokens.push(token)
    }

    Ok(tokens)
}

// helper function for some lexer tests
pub fn tokenize_into_vec_no_positions(src: &str) -> Result<Vec<Token>, LexerError> {
    let mut lexer = Lexer::init(src);
    let mut tokens = Vec::new();

    while let Some((token,_,_,_)) = lexer.next()? {
        tokens.push(token)
    }

    Ok(tokens)
}
