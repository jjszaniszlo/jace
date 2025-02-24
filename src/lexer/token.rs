use std::fmt::Display;
use std::ops::Range;

#[derive(Debug, PartialEq, Clone)]
pub struct Token(pub TokenKind, pub Range<usize>);

impl Token {
    pub fn new(kind: TokenKind, start: usize, length: usize) -> Token {
        Self(kind, start..(start + length))
    }

    pub fn kind(&self) -> TokenKind {
        self.0.clone()
    }

    pub fn span(&self) -> Range<usize> {
        self.1.clone()
    }

    pub fn into_inner(self) -> (TokenKind, Range<usize>) {
        (self.0, self.1)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
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

    // other operators 
    Equals,             // =
    InferredEquals,     // :=
    SemiColon,          // ;
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

    // Other
    Bang,               // !

    // reserved words
    TypeKeyword,    // type
    ClassKeyword,   // class
    InstanceKeyword, // instance
    CaseKeyword,    // case
    LetKeyword,     // let
    InKeyword,      // in
    IfKeyword,      // if
    ThenKeyword,    // then
    ElseKeyword,    // else
    ElseIfKeyword,  // elseif
    DefKeyword,     // def
    ConstKeyword,   // const
    WhereKeyword,   // where
    DoKeyword,   // do
}

impl TokenKind {
    pub fn bool(self) -> bool {
        if let TokenKind::Bool(b) = self {
            b
        } else {
            panic!("Not an identifier!")
        }
    }
    pub fn integer(self) -> usize {
        if let TokenKind::Integer(i) = self {
            i
        } else {
            panic!("Not an identifier!")
        }
    }
    pub fn float(self) -> f64 {
        if let TokenKind::Float(f) = self {
            f
        } else {
            panic!("Not an identifier!")
        }
    }
    pub fn identifier(self) -> String {
        if let TokenKind::Identifier(s) = self {
            s
        } else {
            panic!("Not an identifier!")
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?})", self)
    }
}

impl From<String> for TokenKind {
    fn from(other: String) -> TokenKind {
        TokenKind::Identifier(other)
    }
}

impl<'a> From<&'a str> for TokenKind {
    fn from(other: &'a str) -> TokenKind {
        TokenKind::Identifier(other.to_string())
    }
}

impl From<usize> for TokenKind {
    fn from(other: usize) -> TokenKind {
        TokenKind::Integer(other)
    }
}

impl From<f64> for TokenKind {
    fn from(other: f64) -> TokenKind {
        TokenKind::Float(other)
    }
}
