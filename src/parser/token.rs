use winnow::stream::Location;

use crate::Token;


impl winnow::stream::ContainsToken<Token> for Token {
    #[inline(always)]
    fn contains_token(&self, token: Token) -> bool {
        self.kind() == token.kind()
    }
}
