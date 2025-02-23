use winnow::error::ParserError;
use winnow::stream::{Location, Stream, StreamIsPartial};
use winnow::token::one_of;
use crate::lexer::token::TokenKind;
use crate::Token;

impl<I, E> winnow::Parser<I, Token, E> for TokenKind
where
    I: Stream<Token = Token> + StreamIsPartial,
    E: ParserError<I>,
{
    fn parse_next(&mut self, input: &mut I) -> winnow::Result<Token, E> {
        one_of(self.clone()).parse_next(input)
    }
}

impl winnow::stream::ContainsToken<Token> for TokenKind {
    #[inline(always)]
    fn contains_token(&self, token: Token) -> bool {
        *self == token.kind()
    }
}

impl<const LEN: usize> winnow::stream::ContainsToken<Token> for [TokenKind; LEN] {
    fn contains_token(&self, token: Token) -> bool {
        self.iter().any(|t| *t == token.kind())
    }
}

impl Location for Token {
    fn previous_token_end(&self) -> usize {
        self.1.end
    }

    fn current_token_start(&self) -> usize {
        self.1.start
    }
}