
use crate::{lexer::token::Token, parser::{ast, combinator::*, ptr::*}};

use thiserror::Error;

use super::ast::GenericTypeParam;

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

pub fn parse_module<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Module> {
    pair(
        parse_definitions,
        parse_expression)
    .map(|(defs, expr)| ast::Module(defs, expr))
    .parse(input)
}

pub fn parse_definitions<'a>(input: &'a [Token]) -> ParseResult<'a, Vec<ast::Def>> {
    or_n(vec![
        BoxedParser::new(zero_or_more(parse_type_def())),
        BoxedParser::new(zero_or_more(parse_fn_def())),
        BoxedParser::new(zero_or_more(parse_class_def())),
    ])
    .parse(input)
}

pub fn parse_fn_def_header<'a>() -> impl Parser<'a, ast::Identifier> {
    left(
        match_identifier,
        match_token(Token::ColonColon))
}

pub fn parse_fn_def_types<'a>() -> impl Parser<'a, (Vec<ast::TypeName>, ast::TypeName)> {
    pair(
        parse_fn_type_params(),
        right(
            match_token(Token::FatArrow),
            match_identifier))
    .map(|(type_params, return_type)| (type_params, ast::TypeName(return_type)))
}

pub fn parse_fn_type_params<'a>() -> impl Parser<'a, Vec<ast::TypeName>> {
    pair(
        match_identifier,
        zero_or_more(
            right(
                match_token(Token::Comma),
                match_identifier)))
    .map(|(first_type, types)| {
        let mut final_types = vec![ast::TypeName(first_type)];
        final_types.extend(
            types.iter()
                .map(|i| ast::TypeName(i.clone())));
        final_types
    })
}

pub fn parse_fn_def<'a>() -> impl Parser<'a, ast::Def> {
    pair(
        pair(
            parse_fn_def_header(),
            parse_fn_def_types()),
        right(
            parse_fn_def_header(),
            parse_fn_expression))
    .map(|((i, (tp, rt)), exp)| ast::Def::FnDef(i, tp, rt, exp))
}

pub fn parse_type_def<'a>() -> impl Parser<'a, ast::Def> {
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

pub fn parse_class_def<'a>() -> impl Parser<'a, ast::Def> {
    pair(
        right(
            match_token(Token::ClassKeyword),
            pair(
                match_identifier,
                parse_class_generic_types())),
        parse_class_method_defs())
    .map(|((class_name, params), methods)|
        ast::Def::ClassDef(ast::ClassName(class_name), params, methods))
}

pub fn parse_class_generic_types<'a>() -> impl Parser<'a, Vec<ast::GenericTypeParam>> {
    one_or_more(match_identifier)
    .map(|v|
        v.iter().map(|i|
            ast::GenericTypeParam(i.clone())).collect())
}

pub fn parse_class_method_defs<'a>() -> impl Parser<'a, Vec<ast::MethodDef>> {
    one_or_more(parse_class_method_def())
}

pub fn parse_class_method_def<'a>() -> impl Parser<'a, ast::MethodDef> {
    or(
        parse_class_method_def_operator(),
        parse_class_method_def_named())
}

pub fn parse_class_method_def_operator<'a>() -> impl Parser<'a, ast::MethodDef> {
    pair(
        left(
            parse_method_operator,
            match_token(Token::ColonColon)),
        pair(
            parse_class_method_type_params(),
            right(
                    match_token(Token::FatArrow),
                    match_identifier)))
    .map(|(op, (params, ret))|
        ast::MethodDef::Operator(op, params, ast::TypeName(ret)))
}

pub fn parse_class_method_type_params<'a>() -> impl Parser<'a, Vec<GenericTypeParam>> {
    pair(
        match_identifier,
        zero_or_more(
            right(
                match_token(Token::Comma),
                match_identifier)))
    .map(|(first_type, types)| {
        let mut final_type_params = vec![ast::GenericTypeParam(first_type)];

        final_type_params.extend(
            types.into_iter()
                    .map(|t| ast::GenericTypeParam(t)));

        final_type_params
    })
}

pub fn parse_method_operator<'a>(input: &'a [Token]) -> ParseResult<'a, ast::MethodOperator> {
    match input.get(0) {
        Some(t) => match t {
            Token::WrappedEqualsEquals => Ok((&input[1..], ast::MethodOperator::EqualsEquals)),
            Token::WrappedNotEquals => Ok((&input[1..], ast::MethodOperator::NotEquals)),
            Token::LessEquals => Ok((&input[1..], ast::MethodOperator::LessEquals)),
            Token::GreaterEquals => Ok((&input[1..], ast::MethodOperator::GreaterEquals)),
            Token::Greater => Ok((&input[1..], ast::MethodOperator::Greater)),
            Token::Less => Ok((&input[1..], ast::MethodOperator::Less)),
            Token::Plus => Ok((&input[1..], ast::MethodOperator::Plus)),
            Token::Minus => Ok((&input[1..], ast::MethodOperator::Minus)),
            Token::Divide => Ok((&input[1..], ast::MethodOperator::Divide)),
            Token::Multiply => Ok((&input[1..], ast::MethodOperator::Multiply)),
            _ => Err(ParserError::CouldNotMatchToken),
        },
        None => Err(ParserError::CouldNotMatchToken),
    }
}

pub fn parse_class_method_def_named<'a>() -> impl Parser<'a, ast::MethodDef> { 
    pair(
        left(
            match_identifier,
            match_token(Token::ColonColon)),
        pair(
            parse_class_method_type_params(),
            right(
                    match_token(Token::FatArrow),
                    match_identifier)))
    .map(|(ident, (params, ret))|
        ast::MethodDef::Named(ident, params, ast::TypeName(ret)))
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

pub fn parse_set_literal<'a>() -> impl Parser<'a, ast::Expr> {
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
        ast::Expr::SetExpr(final_fields)
    })
}

pub fn parse_expression<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Expr> {
    // BoxedParser prevents stack overflow by utilizing the heap.
    or_n(vec![
        BoxedParser::new(parse_parenthesized_expression()),
        parse_fn_expression.map(|f| ast::Expr::FnExpr(P(f))),

        BoxedParser::new(parse_set_literal()),
        BoxedParser::new(parse_let_in_expression()),
        match_literal.map(|l| ast::Expr::LitExpr(l)),
        match_identifier.map(|i| ast::Expr::IdentExpr(i)),
    ])
    .parse(input)
}

pub fn parse_parenthesized_expression<'a>() -> impl Parser<'a, ast::Expr> {
    right(
        match_token(Token::LeftParen),
        left(
            parse_expression,
            match_token(Token::RightParen)))
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

pub fn parse_fn_param<'a>(input: &'a [Token]) -> ParseResult<'a, ast::FnParam> {
    or_n(vec![
        match_identifier.map(|i| ast::FnParam::Identifier(i)),
        BoxedParser::new(parse_fn_param_set_deconstruct()),
    ])
    .parse(input)
}

pub fn parse_fn_case_param<'a>(input: &'a [Token]) -> ParseResult<'a, ast::FnParam> {
    or_n(vec![
        match_identifier.map(|i| ast::FnParam::Identifier(i)),
        match_literal.map(|l| ast::FnParam::Literal(l)),
        BoxedParser::new(parse_fn_param_set_deconstruct()),
    ])
    .parse(input)
}

pub fn parse_fn_params<'a>() -> impl Parser<'a, Vec<ast::FnParam>> {
    pair(
        parse_fn_param,
        zero_or_more(
            right(
            match_token(Token::Comma),
            parse_fn_param)))
    .map(|(first_param, params)| {
        let mut final_params = vec![first_param];
        final_params.extend(params);
        final_params
    })
}

pub fn parse_fn_case_params<'a>() -> impl Parser<'a, Vec<ast::FnParam>> {
    pair(
        parse_fn_case_param,
        zero_or_more(
            right(
            match_token(Token::Comma),
            parse_fn_case_param)))
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

pub fn parse_fn_expression_case_single<'a>() -> impl Parser<'a, ast::FnExpr> {
    pair(
        left(
            parse_fn_case_params(),
            match_token(Token::FatArrow)),
        parse_expression)
    .map(|(params, expr)| ast::FnExpr::Single(params, expr))
}

pub fn parse_fn_expression_case<'a>() -> impl Parser<'a, ast::FnExpr> {
    right(
        match_token(Token::CaseKeyword),
        one_or_more(parse_fn_expression_case_single()))
    .map(|fn_exprs| ast::FnExpr::Case(fn_exprs))
}

pub fn parse_fn_expression<'a>(input: &'a [Token]) -> ParseResult<'a, ast::FnExpr> {
    or(
        parse_fn_expression_single(),
        parse_fn_expression_case())
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

pub fn parse_statement<'a>() -> impl Parser<'a, ast::Stmt> {
    or(parse_inferred_assignment(),parse_type_assignment())
}

pub fn parse<'a>(input: &'a [Token]) -> ast::Module {
    let (_, result) = parse_module(input).unwrap();
    result
}
