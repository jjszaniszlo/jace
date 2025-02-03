
use crate::{lexer::token::Token, parser::{ast, combinator::*}};

use thiserror::Error;

#[derive(Error, Debug, Clone, PartialEq)]
pub enum ParserError {
    #[error("Could not match token")]
    CouldNotMatchToken,
}

pub type ParseResult<'a, O> = Result<(&'a [Token], O), ParserError>;

pub trait Parser<'a, O> {
    fn parse(&self, input: &'a [Token]) -> ParseResult<'a, O>;

    fn map<F, N>(self, map_fn: F) -> BoxedParser<'a, N>
    where
        Self: Sized + 'a,
        O: 'a,
        N: 'a,
        F: Fn(O) -> N + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a [Token]) -> ParseResult<'a, Output>,
{
    fn parse(&self, input: &'a [Token]) -> ParseResult<'a, Output> {
        self(input)
    }
}

pub struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        Self {
            parser: Box::new(parser)
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a [Token]) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
}

pub fn match_token<'a>(expected: Token) -> impl Parser<'a, ()> {
    move |toks: &'a [Token]| match toks.get(0) {
        Some(tok) if *tok == expected => Ok((&toks[1..], ())),
        _ => Err(ParserError::CouldNotMatchToken),
    }
}

pub fn match_literal<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Literal> {
    match input.get(0) {
        Some(t) => match t {
            Token::Bool(b) => Ok((&input[1..], ast::Literal::from(*b))),
            Token::Integer(i) => Ok((&input[1..], ast::Literal::from(*i))),
            Token::Float(f) => Ok((&input[1..], ast::Literal::from(*f))),
            Token::String(s) => Ok((&input[1..], ast::Literal::from(s.clone()))),
            _ => Err(ParserError::CouldNotMatchToken),
        },
        None => Err(ParserError::CouldNotMatchToken),
    }
}

pub fn match_identifier<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Identifier> {
    match input.get(0) {
        Some(t) => match t { 
            Token::Identifier(i) => Ok((&input[1..], ast::Identifier(i.clone()))),
            _ => Err(ParserError::CouldNotMatchToken),
        },
        None => Err(ParserError::CouldNotMatchToken),
    }
}

pub fn parse_expression<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Expr> {
    or(
        match_literal.map(|l| ast::Expr::Literal(l)),
        or(
            match_identifier.map(|i| ast::Expr::Identifier(i)),
            parse_set_literal.map(|s| ast::Expr::Literal(s))))
    .parse(input)
}

pub fn parse_type_assignment<'a>() -> impl Parser<'a, ast::Stmt> {
    pair(
        pair(
            match_identifier,
            right(
                match_token(Token::Colon),
                match_identifier)),
        right(
            match_token(Token::Equals),
            parse_expression))
    .map(|((i, t), e)| ast::Stmt::Asmt(i, Some(ast::TypeName(t)), e))
}

pub fn parse_inferred_assignment<'a>() -> impl Parser<'a, ast::Stmt> {
    pair(
        match_identifier,
        right(
            match_token(Token::InferredEquals),
            parse_expression))
    .map(|(id, e)| ast::Stmt::Asmt(id, None, e))
}

pub fn parse_type_definition<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Def> {
    pair(
        right(
            match_token(Token::TypeKeyword),
            left(
                match_identifier,
                match_token(Token::ColonColon))),
        one_or_more(
            pair(
                match_identifier,
                right(
                    match_token(Token::Colon),
                    match_identifier))))
    .map(|(i, v)| ast::Def::TypeDef(
            ast::TypeName(i), 
            v.into_iter().map(|(i, t)| (i, ast::TypeName(t))).collect()))
    .parse(input) 
}

pub fn parse_set_literal_field<'a>() -> impl Parser<'a, (ast::Identifier, ast::Expr)> {
    pair(
        left(
            match_identifier,
            match_token(Token::Equals)),
        parse_expression)
}

pub fn parse_set_comma_seperated_fields<'a>() -> impl Parser<'a, Vec<(ast::Identifier, ast::Expr)>> {
    pair(
        parse_set_literal_field(),
        zero_or_more(
            right(
                match_token(Token::Comma),
                parse_set_literal_field())))
    .map(|(first, rest)| {
        let mut fields = vec![first];
        fields.extend(rest);
        fields
    }) 
}

pub fn parse_set_literal<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Literal> {
    right(
        match_token(Token::LeftBrace),
        left(
            zero_or_one(parse_set_comma_seperated_fields()),
            match_token(Token::RightBrace)))
    .map(|v| ast::Literal::Set(v.unwrap_or_default()))
    .parse(input)
}

pub fn parse_statement<'a>() -> impl Parser<'a, ast::Stmt> {
    or(parse_inferred_assignment(),parse_type_assignment())
}

pub fn parse<'a>(input: &'a [Token]) -> ast::Expr {
    let (_, result) = parse_expression(input).unwrap();
    result
}
