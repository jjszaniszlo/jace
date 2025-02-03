use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // payload tokens
    Integer(usize),
    Float(f64),
    String(String),
    Identifier(String),
    Bool(bool),

    // binary operators
    EqualsEquals,       // ==
    NotEquals,          // !=
    And,                // &&
    Or,                 // ||
    GreaterEquals,      // >=
    LessEquals,         // <=
    Greater,            // >
    Less,               // <
    Plus,               // +
    Minus,              // -
    Divide,             // /
    Multiply,           // *

    // wrapped operators.
    WrappedEqualsEquals,    // (==)
    WrappedNotEquals,       // (!=)
    WrappedLessEquals,      // (<=)
    WrappedGreaterEquals,   // (>=)
    WrappedGreater,         // (>)
    WrappedLess,            // (<)
    WrappedPlus,            // (+)
    WrappedMinus,           // (-)
    WrappedDivide,          // (/)
    WrappedMultiply,        // (*)

    // other operators 
    Equals,             // =
    InferredEquals,     // :=
    Colon,              // :
    ColonColon,         // ::
    Union,              // |
    FatArrow,           // =>
    Comma,              // ,
    LeftParen,          // (
    RightParen,         // )
    LeftBrace,          // {
    RightBrace,         // }
    LeftBracket,        // [
    RightBracket,       // ]
 
    // reserved words
    TypeKeyword,    // type
    ClassKeyword,   // class
    InstanceKeyword,// instance
    CaseKeyword,    // case
    LetKeyword,     // let
    InKeyword,      // in
    IfKeyword,      // if
    ThenKeyword,    // then
    ElseKeyword,    // else
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?})", self)
    }
}

impl From<String> for Token {
    fn from(other: String) -> Token {
        Token::Identifier(other)
    }
}

impl<'a> From<&'a str> for Token {
    fn from(other: &'a str) -> Token {
        Token::Identifier(other.to_string())
    }
}

impl From<usize> for Token {
    fn from(other: usize) -> Token {
        Token::Integer(other)
    }
}

impl From<f64> for Token {
    fn from(other: f64) -> Token {
        Token::Float(other)
    }
}
