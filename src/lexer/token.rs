use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // payload tokens
    Bool(bool),
    Integer(usize),
    Float(f64),
    String(String),
    Identifier(String),

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
    Exp,                // ^

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
    WrappedExp,             // (^)
    WrappedOperator(String),// (custom_operator)

    // other operators 
    Equals,             // =
    InferredEquals,     // :=
    Colon,              // :
    ColonColon,         // ::
    Union,              // |
    FatArrow,           // =>
    Comma,              // ,
    Dot,                // .
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
    ElseIfKeyword,  // elseif
}

impl Token {
    pub fn bool(self) -> bool {
        if let Token::Bool(b) = self {
            b
        } else {
            panic!("Not an identifier!")
        }
    }
    pub fn integer(self) -> usize {
        if let Token::Integer(i) = self {
            i
        } else {
            panic!("Not an identifier!")
        }
    }
    pub fn float(self) -> f64 {
        if let Token::Float(f) = self {
            f
        } else {
            panic!("Not an identifier!")
        }
    }
    pub fn identifier(self) -> String {
        if let Token::Identifier(s) = self {
            s
        } else {
            panic!("Not an identifier!")
        }
    }
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
