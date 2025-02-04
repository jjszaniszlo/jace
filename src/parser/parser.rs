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

pub fn match_token_fetch<'a>(expected: Token) -> impl Parser<'a, Token> {
    move |toks: &'a [Token]| match toks.get(0) {
        Some(tok) if *tok == expected => Ok((&toks[1..], tok.clone())),
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

pub fn match_number<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Literal> {
    match input.get(0) {
        Some(t) => match t {
            Token::Integer(i) => Ok((&input[1..], ast::Literal::from(*i))),
            Token::Float(f) => Ok((&input[1..], ast::Literal::from(*f))),
            _ => Err(ParserError::CouldNotMatchToken),
        },
        None => Err(ParserError::CouldNotMatchToken),
    }
}

pub fn parse_module<'a>() -> impl Parser<'a, ast::Module> {
    pair(
        zero_or_more(parse_definition()),
        parse_expression)
    .map(|(defs, expr)| ast::Module(defs, expr))
}

pub fn parse_definition<'a>() -> impl Parser<'a, ast::Def> {
    BoxedParser::new(
        or(
            parse_type_def(),
            or(
                parse_fn_def(),
                    parse_class_def())))
}

pub fn parse_fn_def_header<'a>() -> impl Parser<'a, ast::Identifier> {
    left(
        match_identifier,
        match_token(Token::ColonColon))
}

pub fn parse_fn_def<'a>() -> impl Parser<'a, ast::Def> {
    pair(
        pair(
            parse_fn_def_header(),
            parse_fn_def_types()),
        right(
            parse_fn_def_header(),
            parse_fn_fnexpr()))
    .map(|((i, (tp, rt)), exp)| ast::Def::FnDef(i, tp, rt, exp))
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
            Token::WrappedLessEquals => Ok((&input[1..], ast::MethodOperator::LessEquals)),
            Token::WrappedGreaterEquals => Ok((&input[1..], ast::MethodOperator::GreaterEquals)),
            Token::WrappedGreater => Ok((&input[1..], ast::MethodOperator::Greater)),
            Token::WrappedLess => Ok((&input[1..], ast::MethodOperator::Less)),
            Token::WrappedPlus => Ok((&input[1..], ast::MethodOperator::Plus)),
            Token::WrappedMinus => Ok((&input[1..], ast::MethodOperator::Minus)),
            Token::WrappedDivide => Ok((&input[1..], ast::MethodOperator::Divide)),
            Token::WrappedMultiply => Ok((&input[1..], ast::MethodOperator::Multiply)),
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

pub fn parse_expression<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Expr> {
    or_n(vec![
        BoxedParser::new(parse_fn_call()),
        BoxedParser::new(parse_equality_expr()),
        BoxedParser::new(parse_fn_expression()),
        BoxedParser::new(parse_if_then_else()),
        BoxedParser::new(parse_set_literal()),
        BoxedParser::new(parse_let_in_expression()),
    ])
    .parse(input)
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

pub fn parse_set_literal_field<'a>() -> impl Parser<'a, (ast::Identifier, ast::Expr)> {
    pair(
        left(
            match_identifier,
            match_token(Token::Equals)),
        parse_expression)
}

pub fn parse_parenthesized_expression<'a>() -> impl Parser<'a, ast::Expr> {
    right(
        match_token(Token::LeftParen),
        left(
            parse_expression,
            match_token(Token::RightParen)))
}

pub fn parse_equality_expr<'a>() -> impl Parser<'a, ast::Expr> {
    pair(
        parse_additive_expr(),
        zero_or_more(
            pair(
                or_n(vec![
                    match_token_fetch(Token::Greater),
                    match_token_fetch(Token::GreaterEquals),
                    match_token_fetch(Token::Less),
                    match_token_fetch(Token::LessEquals),
                    match_token_fetch(Token::EqualsEquals),
                    match_token_fetch(Token::NotEquals)]),
                parse_additive_expr())))
    .map(|(initial, rest)| {
        rest.into_iter().fold(initial, |acc, (op, next)| {
            let operator = match op {
                Token::Greater => ast::BinOperator::Greater,
                Token::GreaterEquals => ast::BinOperator::GreaterEquals,
                Token::Less => ast::BinOperator::Less,
                Token::LessEquals => ast::BinOperator::LessEquals,
                Token::EqualsEquals => ast::BinOperator::EqualsEquals,
                Token::NotEquals => ast::BinOperator::NotEquals,
                _ => unreachable!(),
            };
            ast::Expr::BinOpExpr(operator, P(acc), P(next))
        })
    })
}

pub fn parse_additive_expr<'a>() -> impl Parser<'a, ast::Expr> { 
    pair(
        parse_multiplicative_expr(),
        zero_or_more(
            pair(
                or(match_token_fetch(Token::Plus), match_token_fetch(Token::Minus)),
                parse_multiplicative_expr())))
    .map(|(initial, rest)| {
        rest.into_iter().fold(initial, |acc, (op, next)| {
            let operator = match op {
                Token::Plus => ast::BinOperator::Plus,
                Token::Minus => ast::BinOperator::Minus,
                _ => unreachable!(),
            };
            ast::Expr::BinOpExpr(operator, P(acc), P(next))
        })
    })
}

pub fn parse_multiplicative_expr<'a>() -> impl Parser<'a, ast::Expr> {
    pair(
        parse_primary,
        zero_or_more(
            pair(
                or(match_token_fetch(Token::Multiply), match_token_fetch(Token::Divide)),
                parse_primary)))
    .map(|(initial, rest)| {
        rest.into_iter().fold(initial, |acc, (op, next)| {
            let operator = match op {
                Token::Multiply => ast::BinOperator::Multiply,
                Token::Divide => ast::BinOperator::Divide,
                _ => unreachable!(),
            };
            ast::Expr::BinOpExpr(operator, P(acc), P(next))
        })
    })
}

pub fn parse_unary_minus<'a>() -> impl Parser<'a, ast::Expr> {
    right(match_token(Token::Minus), parse_primary)
        .map(|expr| ast::Expr::BinOpExpr(
            ast::BinOperator::Minus,
            P(ast::Expr::LitExpr(ast::Literal::Integer(0))),
            P(expr)))
}

pub fn parse_primary<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Expr> {
    or_n(vec![
        BoxedParser::new(parse_parenthesized_expression()),
        match_identifier.map(|i| ast::Expr::IdentExpr(i)),
        match_number.map(|n| ast::Expr::LitExpr(n)),
        BoxedParser::new(parse_unary_minus()),
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

pub fn parse_fn_param<'a>() -> impl Parser<'a, ast::FnParam> {
    or_n(vec![
        match_identifier.map(|i| ast::FnParam::Identifier(i)),
        BoxedParser::new(parse_fn_param_set_deconstruct()),
        BoxedParser::new(parse_fn_param_set_selector()),
    ])
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

pub fn parse_fn_param_set_selector<'a>() -> impl Parser<'a, ast::FnParam> {
    pair(
        right(
            match_token(Token::LeftBrace),
            match_identifier),
        right(
            match_token(Token::Colon),
                left(
                    match_identifier,
                    match_token(Token::RightParen))))
    .map(|(first, rest)| ast::FnParam::SetSelector(first, rest))                
}

pub fn parse_fn_case_param<'a>() -> impl Parser<'a, ast::FnParam> {
    or_n(vec![
        match_identifier.map(|i| ast::FnParam::Identifier(i)),
        match_literal.map(|l| ast::FnParam::Literal(l)),
        BoxedParser::new(parse_fn_param_set_deconstruct()),
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

pub fn parse_fn_case_params<'a>() -> impl Parser<'a, Vec<ast::FnParam>> {
    pair(
        parse_fn_case_param(),
        zero_or_more(
            right(
            match_token(Token::Comma),
            parse_fn_case_param())))
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

pub fn parse_fn_fnexpr<'a>() -> impl Parser<'a, ast::FnExpr> {
    or(
        parse_fn_expression_single(),
        parse_fn_expression_case())
}

pub fn parse_fn_expression<'a>() -> impl Parser<'a, ast::Expr> {
    parse_fn_fnexpr().map(|f| ast::Expr::FnExpr(P(f)))
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

pub fn parse_fn_call<'a>() -> impl Parser<'a, ast::Expr> {
    pair(
        match_identifier,
        pair(
            parse_expression,
            zero_or_more(parse_expression)))
    .map(|(func_name, (first_param, params))| {
        let mut final_params = vec![first_param];
        final_params.extend(params);
            ast::Expr::FnCall(func_name, final_params)
    })
}

pub fn parse_if_then_else<'a>() -> impl Parser<'a, ast::Expr> {
    pair(
    right(
        match_token(Token::IfKeyword),
        left(
            parse_expression,
            match_token(Token::ThenKeyword))),
    pair(
        pair(
                parse_expression,
                zero_or_more(parse_elseif_p_then_e())),
        right(
            match_token(Token::ElseKeyword),
            parse_expression)))
    .map(|(pred, ((expr, elseifs), else_expr))| {
        let mut final_result = vec![(pred, expr)];
        final_result.extend(elseifs);

        ast::Expr::IfThenElseIfExpr(final_result, P(else_expr))
    })
}

pub fn parse_elseif_p_then_e<'a>() -> impl Parser<'a, (ast::Expr, ast::Expr)> {
    pair(
        right(
            match_token(Token::ElseIfKeyword),
            left(
                parse_expression,
                match_token(Token::ThenKeyword))),
        parse_expression)
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
    let (_, result) = parse_module().parse(input).unwrap();
    result
}
