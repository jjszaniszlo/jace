
use crate::{lexer::token::Token, parser::{ast, combinator::*, ptr::*}};

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

pub fn match_identifier<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Identifier> {
    match input.get(0) {
        Some(t) => match t { 
            Token::Identifier(i) => Ok((&input[1..], ast::Identifier(i.clone()))),
            _ => Err(ParserError::CouldNotMatchToken),
        },
        None => Err(ParserError::CouldNotMatchToken),
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

pub fn parse_set_literal_field<'a>() -> impl Parser<'a, (ast::Identifier, ast::Expr)> {
    pair(
        left(
            match_identifier,
            match_token(Token::Equals)),
        parse_expression)
}

pub fn parse_set_literal_comma_seperated_fields<'a>() -> impl Parser<'a, Vec<(ast::Identifier, ast::Expr)>> {
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

pub fn parse_set_literal<'a>() -> impl Parser<'a, ast::Literal> {
    right(
        match_token(Token::LeftBrace),
        left(
            zero_or_one(parse_set_literal_comma_seperated_fields()),
            match_token(Token::RightBrace)))
    .map(|v| {
        let mut final_fields : Vec<(ast::Identifier, ast::Expr)> = vec![];
        match v {
            Some(v) => final_fields.extend(v),
            None => {}
        }
        ast::Literal::Set(final_fields)
    })
}

pub fn parse_type_definition<'a>() -> impl Parser<'a, ast::Def> {
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
}

pub fn parse_expression<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Expr> {
    or_n(vec![
        parse_set_literal().map(|l| ast::Expr::Literal(l)),
        match_literal.map(|l| ast::Expr::Literal(l)),
        match_identifier.map(|i| ast::Expr::Identifier(i)),
        parse_let_in_expression().map(|f| f),
    ])
    .parse(input)
}

pub fn parse_let_in_expression<'a>() -> impl Parser<'a, ast::Expr> {
    right(
        match_token(Token::LetKeyword),
        pair(
            left(
                zero_or_more(parse_statement()),
                match_token(Token::InKeyword)),
            parse_expression))
    .map(|(v, e)| ast::Expr::LetInExpr(v, P(e)))
}

pub fn parse_fn_param_set_deconstruct<'a>() -> impl Parser<'a, ast::FnParam> {
    right(
        match_token(Token::LeftBrace),
        left(
                zero_or_one(
                    pair(
                        match_identifier,
                        zero_or_more(
                            right(
                                match_token(Token::Comma),
                                match_identifier)))),
                match_token(Token::RightBrace)))
    .map(|v| {
        let mut final_set_params : Vec<ast::Identifier> = vec![];
        match v {
            Some((first_param, params)) => {
                final_set_params.push(first_param);
                final_set_params.extend(params);
            },
            None => {}
        };
        ast::FnParam::SetDeconstruct(final_set_params)
    })
}

pub fn parse_fn_param<'a>() -> impl Parser<'a, ast::FnParam> {
    or_n(vec![
        match_identifier.map(|i| ast::FnParam::Identifier(i)),
        parse_fn_param_set_deconstruct().map(|f| f)
    ])
}

pub fn parse_fn_params<'a>() -> impl Parser<'a, Vec<ast::FnParam>> {
    pair(
        parse_fn_param(),
        zero_or_more(
            right(
            match_token(Token::Comma),
            parse_fn_param())))
    .map(|(first_param, params)| {
        let mut final_params = vec![first_param];
        final_params.extend(params);
        final_params
    })
}

pub fn parse_fn_expression_single<'a>() -> impl Parser<'a, ast::FnExpr> {
    pair(
        left(
            parse_fn_params(),
            match_token(Token::FatArrow)),
        parse_expression)
    .map(|(params, expr)| ast::FnExpr::Single(params, expr))
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

pub fn parse_statement<'a>() -> impl Parser<'a, ast::Stmt> {
    or(parse_inferred_assignment(),parse_type_assignment())
}

pub fn parse<'a>(input: &'a [Token]) -> ast::Expr {
    let (_, result) = parse_expression(input).unwrap();
    result
}
