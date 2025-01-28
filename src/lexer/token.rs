use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum Token {
    // payload tokens
    Integer(isize),
    Float(f64),
    String(String),
    Identifier(String),
    WrappedOperator(String),

    // non-payload tokens
    Equals,         // =
    InferredEquals, // :=
    EqualsEquals,   // ==
    NotEquals,      // !=
    And,            // &&
    Or,             // ||
    Colon,          // :
    ColonColon,     // ::
    FatArrow,       // =>
    Comma,          // ,
    LeftParen,      // (
    RightParen,     // )
    LeftBrace,      // {
    RightBrace,     // }
    LeftBracket,    // [
    RightBracket,   // ]
    Union,          // |
    Plus,           // +
    Minus,          // -
    Divide,         // /
    Multiply,       // *
    
    // reserved words
    TypeKeyword,      // type
    ClassKeyword,     // class
    InstanceKeyword,  // instance
    CaseKeyword,      // case
    IfKeyword,        // if
    ThenKeyword,      // then
    ElseKeyword,      // else

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

impl From<isize> for Token {
    fn from(other: isize) -> Token {
        Token::Integer(other)
    }
}

impl From<f64> for Token {
    fn from(other: f64) -> Token {
        Token::Float(other)
    }
}
