use thiserror::Error;

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

}

#[derive(Error, Debug)]
enum LexerError {
    #[error("Invalid char @ {line:?}")]
    InvalidCharAt {
        line: usize,
    },

    #[error("Unexpected EOF")]
    UnexpectedEOF,
}



fn lex_identifier(data: &str) -> Result<Token, LexerError> {
    match data.chars().next() {
        Some(ch) if ch.is_digit(10) => return Err(LexerError::InvalidCharAt { line:0 }),
        None => return Err(LexerError::UnexpectedEOF),
        _ => {},
    }

    Ok(Token::Or)
}
