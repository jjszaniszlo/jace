use std::{iter::Peekable, rc::Rc, str::Chars};

use super::token::{Token, TokenKind};
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::jace_file::JaceFile;

pub struct SourceIterator<'a> {
    src: Peekable<Chars<'a>>,
    pos: usize,
}

impl<'a> SourceIterator<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src: src.chars().peekable(),
            pos: 0,
        }
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.src.peek()
    }
}

impl<'a> Iterator for SourceIterator<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        match self.src.next() {
            Some(ch) => {
                self.pos += ch.len_utf8();
                Some(ch)
            }
            None => None,
        }
    }
}

pub struct LexerIterator<'a> {
    jace_file: Rc<JaceFile<'static>>,
    src_iter: SourceIterator<'a>,
}

impl<'a> LexerIterator<'a> {
    pub fn new(jace_file: JaceFile<'static>) -> Self {
        let jc: Rc<JaceFile<'static>> = jace_file.into();
        Self {
            jace_file: jc.clone(),
            src_iter: SourceIterator::new(jc.clone().contents()),
        }
    }

    pub fn next(&mut self) -> Option<miette::Result<Token>> {
        let start_pos = self.src_iter.pos();

        match self.skip_comment(start_pos) {
            Some(t) => return Some(Ok(t)),
            _ => {}
        }

        if let Some(ch) = self.src_iter.next() {
            let tok = match ch {
                ch if ch.is_alphabetic() || ch == '_' => Ok(self.eat_identifier(ch, start_pos)),
                '0'..='9' => self.eat_number(ch, start_pos),
                '=' | '>' | '<' | '!' | '&' | '|' |
                '+' | '-' | '*' | '/' | '^' | ':' |
                ',' | '.' | ';' => self.eat_symbol(ch, start_pos),
                '(' | ')' | '[' | ']' | '{' | '}' => self.eat_brackets(ch, start_pos),
                '"' => self.eat_string_literal(ch, start_pos),
                c if c.is_whitespace() => return self.next(),

                _ => Err(UnexpectedChar {
                    src: *self.jace_file,
                    error_span: (start_pos, ch.len_utf8()).into(),
                    ch,
                }.into())
            };

            Some(tok)
        } else {
            None
        }
    }

    pub fn eat_identifier(&mut self, first_char: char, start_pos: usize) -> Token {
        let mut identifier = String::from(first_char);

        while let Some(ch) = self.peek_char() {
            match ch {
                c if c.is_alphanumeric() || c == '_' => identifier.push(self.eat_char()),
                _ => break,
            }
        }

        let bytes = identifier.len();

        match identifier.as_str() {
            "type" => Token::new(TokenKind::TypeKeyword, start_pos, bytes),
            "class" => Token::new(TokenKind::ClassKeyword, start_pos, bytes),
            "instance" => Token::new(TokenKind::InstanceKeyword, start_pos, bytes),
            "case" => Token::new(TokenKind::CaseKeyword, start_pos, bytes),
            "let" => Token::new(TokenKind::LetKeyword, start_pos, bytes),
            "in" => Token::new(TokenKind::InKeyword, start_pos, bytes),
            "if" => Token::new(TokenKind::IfKeyword, start_pos, bytes),
            "then" => Token::new(TokenKind::ThenKeyword, start_pos, bytes),
            "else" => Token::new(TokenKind::ElseKeyword, start_pos, bytes),
            "elseif" => Token::new(TokenKind::ElseIfKeyword, start_pos, bytes),
            "def" => Token::new(TokenKind::DefKeyword, start_pos, bytes),
            "const" => Token::new(TokenKind::ConstKeyword, start_pos, bytes),
            "where" => Token::new(TokenKind::WhereKeyword, start_pos, bytes),
            "do" => Token::new(TokenKind::DoKeyword, start_pos, bytes),
            "True" => Token::new(TokenKind::Bool(true), start_pos, bytes),
            "False" => Token::new(TokenKind::Bool(false), start_pos, bytes),
            _ => Token::new(TokenKind::Identifier(identifier), start_pos, bytes)
        }
    }

    // parses integers and floats.  Handles all possible error cases.
    pub fn eat_number(&mut self, first_char: char, start_pos: usize) -> miette::Result<Token> {
        let mut number = String::from(first_char);

        let mut found_point = false;

        if first_char == '0' {
            if let Some(ch) = self.peek_char() {
                match ch {
                    '.' => {
                        found_point = true;
                        number.push(self.eat_char())
                    }
                    '0'..'9' => return Err(UnexpectedChar {
                        src: *self.jace_file,
                        error_span: (start_pos + number.len(), number.len()).into(),
                        ch,
                    }.into()),
                    _ => {}
                }
            }
        }

        while let Some(ch) = self.peek_char() {
            match ch {
                '.' => {
                    if found_point {
                        return Err(UnexpectedChar {
                            src: *self.jace_file,
                            error_span: (start_pos + number.len(), ch.len_utf8()).into(),
                            ch,
                        }.into());
                    } else {
                        found_point = true;
                        number.push(self.eat_char());
                        // check if digits after .
                        if let Some(ch) = self.peek_char() {
                            match ch {
                                '0'..='9' => {}
                                _ => return Err(ExpectedChar {
                                    src: *self.jace_file,
                                    error_span: (start_pos + number.len(), ch.len_utf8()).into(),
                                    help: Some(String::from("try adding digits after the '.'")),
                                    expected: String::from("a digit"),
                                    got: String::from(ch),
                                }.into())
                            }
                        } else {
                            return Err(ExpectedChar {
                                src: *self.jace_file,
                                error_span: (start_pos + number.len(), ch.len_utf8()).into(),
                                help: Some(String::from("try adding digits after the '.'")),
                                expected: String::from("a digit"),
                                got: String::from("EOF"),
                            }.into());
                        }
                    }
                }
                ch if ch.is_ascii_alphabetic() =>
                    return Err(ExpectedChar {
                        src: *self.jace_file,
                        error_span: (start_pos + number.len(), ch.len_utf8()).into(),
                        help: Some(String::from("numbers must not have letters!")),
                        expected: String::from("a digit"),
                        got: String::from(ch),
                    }.into()),
                '0'..='9' => number.push(self.eat_char()),
                _ => break,
            }
        }

        if found_point {
            match number.parse::<f64>() {
                Ok(f) => Ok(Token::new(TokenKind::Float(f), start_pos, number.len())),
                Err(_) => Err(MalformedLiteral {
                    src: *self.jace_file,
                    error_span: (start_pos + number.len(), number.len()).into(),
                }.into())
            }
        } else {
            match number.parse::<usize>() {
                Ok(f) => Ok(Token::new(TokenKind::Integer(f), start_pos, number.len())),
                Err(_) => Err(MalformedLiteral {
                    src: *self.jace_file,
                    error_span: (start_pos + number.len(), number.len()).into(),
                }.into())
            }
        }
    }

    pub fn eat_symbol(&mut self, start_char: char, start_pos: usize) -> miette::Result<Token> {
        let mut operator = String::from(start_char);

        while let Some(ch) = self.peek_char() {
            match ch {
                '=' | '>' | '<' | '!' | '&' | '|' |
                '+' | '-' | '*' | '/' | '^' | ':' => operator.push(self.eat_char()),
                _ => break,
            }
        }

        let bytes = operator.len();

        match operator.as_str() {
            "==" => Ok(Token::new(TokenKind::EqualsEquals, start_pos, bytes)),
            "!=" => Ok(Token::new(TokenKind::NotEquals, start_pos, bytes)),
            "&&" => Ok(Token::new(TokenKind::And, start_pos, bytes)),
            "||" => Ok(Token::new(TokenKind::Or, start_pos, bytes)),
            ">=" => Ok(Token::new(TokenKind::GreaterEquals, start_pos, bytes)),
            "<=" => Ok(Token::new(TokenKind::LessEquals, start_pos, bytes)),
            ">" => Ok(Token::new(TokenKind::Greater, start_pos, bytes)),
            "<" => Ok(Token::new(TokenKind::Less, start_pos, bytes)),
            "+" => Ok(Token::new(TokenKind::Plus, start_pos, bytes)),
            "-" => Ok(Token::new(TokenKind::Minus, start_pos, bytes)),
            "/" => Ok(Token::new(TokenKind::Divide, start_pos, bytes)),
            "*" => Ok(Token::new(TokenKind::Multiply, start_pos, bytes)),
            "^" => Ok(Token::new(TokenKind::Exp, start_pos, bytes)),
            "=" => Ok(Token::new(TokenKind::Equals, start_pos, bytes)),
            "|" => Ok(Token::new(TokenKind::Union, start_pos, bytes)),
            "=>" => Ok(Token::new(TokenKind::FatArrow, start_pos, bytes)),
            "!" => Ok(Token::new(TokenKind::Bang, start_pos, bytes)),
            ":" => Ok(Token::new(TokenKind::Colon, start_pos, bytes)),
            "::" => Ok(Token::new(TokenKind::ColonColon, start_pos, bytes)),
            ":=" => Ok(Token::new(TokenKind::InferredEquals, start_pos, bytes)),
            ";" => Ok(Token::new(TokenKind::SemiColon, start_pos, bytes)),
            "," => Ok(Token::new(TokenKind::Comma, start_pos, bytes)),
            "." => Ok(Token::new(TokenKind::Dot, start_pos, bytes)),
            op => Err(MalformedOperator {
                src: *self.jace_file,
                error_span: (start_pos, bytes).into(),
                op: op.to_string(),
            }.into())
        }
    }

    fn eat_brackets(&mut self, first_char: char, start_pos: usize) -> miette::Result<Token> {
        let bytes = first_char.len_utf8();
        match first_char {
            ')' => Ok(Token::new(TokenKind::RightParen, start_pos, bytes)),
            '[' => Ok(Token::new(TokenKind::LeftBracket, start_pos, bytes)),
            ']' => Ok(Token::new(TokenKind::RightBracket, start_pos, bytes)),
            '{' => Ok(Token::new(TokenKind::LeftBrace, start_pos, bytes)),
            '}' => Ok(Token::new(TokenKind::RightBrace, start_pos, bytes)),
            '(' => self.eat_wrapped_operator(first_char, start_pos),
            _ => unreachable!(),
        }
    }

    fn eat_wrapped_operator(&mut self, first_char: char, start_pos: usize) -> miette::Result<Token> {
        let mut operator = String::from(first_char);
        let bytes = operator.len();
        match self.peek_char() {
            Some(c @ ('=' | '!' | '<' | '>' | '+' | '-' | '/' | '*')) => {
                operator.push(self.eat_char());
                match self.peek_char() {
                    Some(')') => operator.push(self.eat_char()),
                    Some('=') => {
                        operator.push(self.eat_char());
                        match self.peek_char() {
                            Some(')') => operator.push(self.eat_char()),
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
            _ => return Ok(Token::new(TokenKind::LeftParen, start_pos, operator.len())),
        }

        let bytes = operator.len();

        match operator.as_str() {
            "(==)" => Ok(Token::new(TokenKind::WrappedEqualsEquals, start_pos, bytes)),    // (==)
            "(!=)" => Ok(Token::new(TokenKind::WrappedNotEquals, start_pos, bytes)),       // (!=)
            "(<=)" => Ok(Token::new(TokenKind::WrappedLessEquals, start_pos, bytes)),      // (<=)
            "(>=)" => Ok(Token::new(TokenKind::WrappedGreaterEquals, start_pos, bytes)),   // (>=)
            "(>)" => Ok(Token::new(TokenKind::WrappedGreater, start_pos, bytes)),         // (>)
            "(<)" => Ok(Token::new(TokenKind::WrappedLess, start_pos, bytes)),            // (<)
            "(+)" => Ok(Token::new(TokenKind::WrappedPlus, start_pos, bytes)),            // (+)
            "(-)" => Ok(Token::new(TokenKind::WrappedMinus, start_pos, bytes)),           // (-)
            "(/)" => Ok(Token::new(TokenKind::WrappedDivide, start_pos, bytes)),          // (/)
            "(*)" => Ok(Token::new(TokenKind::WrappedMultiply, start_pos, bytes)),        // (*)
            op => Err(MalformedOperator {
                src: *self.jace_file,
                error_span: (start_pos, bytes).into(),
                op: op.to_string(),
            }.into())
        }
    }

    fn eat_string_literal(&mut self, _first: char, start_pos: usize) -> Result<Token, miette::Error> {
        let mut string_literal = String::new();

        while let Some(ch) = self.peek_char() {
            match ch {
                '"' => {
                    self.eat_char();
                    break;
                }
                '\\' => {
                    // eat backslash
                    string_literal.push(self.eat_char());
                    // handle escape sequences.
                    if let Some(ch) = self.peek_char() {
                        match ch {
                            't' | 'n' | 'r' | '"' => {
                                string_literal.push(self.eat_char());
                            }
                            _ => return Err(InvalidEscapeSequence {
                                src: *self.jace_file.clone(),
                                error_span: (start_pos + string_literal.len(), ch.len_utf8()).into(),
                                ch: ch.to_string(),
                            }.into()),
                        };
                    } else {
                        return Err(UnexpectedEOF {
                            src: *self.jace_file,
                            error_span: (start_pos, string_literal.len()).into(),
                        }.into());
                    }
                }
                _ => string_literal.push(self.eat_char()),
            }
        }

        let bytes = string_literal.len();

        Ok(Token::new(TokenKind::String(string_literal), start_pos, bytes + 2 * '"'.len_utf8()))
    }

    fn skip_comment(&mut self, st: usize) -> Option<Token> {
        match self.peek_char() {
            Some('-') => {
                self.eat_char();
                match self.peek_char() {
                    Some('-') => {
                        self.eat_until('\n');
                        None
                    }
                    _ => Some(Token::new(TokenKind::Minus, st, '-'.len_utf8())),
                }
            }
            _ => None,
        }
    }

    pub fn eat_until(&mut self, look_for_char: char) {
        while let Some(ch) = self.peek_char() {
            if ch == look_for_char {
                break;
            }
            self.eat_char();
        }
    }

    pub fn eat_char(&mut self) -> char {
        self.src_iter.next().unwrap()
    }

    pub fn peek_char(&mut self) -> Option<char> {
        self.src_iter.peek().copied()
    }
}

impl<'a> Iterator for LexerIterator<'a> {
    type Item = miette::Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next()
    }
}

pub struct Lexer {
    jace_file: JaceFile<'static>,
}

impl Lexer {
    pub fn new(jace_file: JaceFile<'static>) -> Self {
        Self {
            jace_file
        }
    }
}

impl IntoIterator for Lexer {
    type Item = miette::Result<Token>;

    type IntoIter = LexerIterator<'static>;

    fn into_iter(self) -> Self::IntoIter {
        LexerIterator::new(self.jace_file)
    }
}

#[derive(Error, Debug, Diagnostic, PartialEq)]
#[error("Unexpected EOF")]
pub struct UnexpectedEOF<'a> {
    #[source_code]
    src: JaceFile<'a>,

    #[label("when parsing this token")]
    error_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic, PartialEq)]
#[error("Unexpected char, '{}'", ch)]
pub struct UnexpectedChar<'a> {
    #[source_code]
    src: JaceFile<'a>,

    #[label("this char")]
    error_span: SourceSpan,

    ch: char,
}

#[derive(Error, Debug, Diagnostic, PartialEq)]
#[error("Expected {}, got {}", expected, got)]
pub struct ExpectedChar<'a> {
    #[source_code]
    src: JaceFile<'a>,

    #[label("this char")]
    error_span: SourceSpan,

    #[help]
    help: Option<String>,

    expected: String,
    got: String,
}

#[derive(Error, Debug, Diagnostic, PartialEq)]
#[error("Malformed Literal")]
pub struct MalformedLiteral<'a> {
    #[source_code]
    src: JaceFile<'a>,

    #[label("this literal")]
    error_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic, PartialEq)]
#[error("Malformed Operator {}", op)]
pub struct MalformedOperator<'a> {
    #[source_code]
    src: JaceFile<'a>,

    #[label("this operator")]
    error_span: SourceSpan,

    op: String,
}

#[derive(Error, Debug, Diagnostic, PartialEq)]
#[error("Invalid Escape Sequence in String {}", ch)]
pub struct InvalidEscapeSequence<'a> {
    #[source_code]
    src: JaceFile<'a>,

    #[label("this escape")]
    error_span: SourceSpan,

    ch: String,
}
