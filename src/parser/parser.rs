use crate::{lexer::token::Token, parser::{ast, combinator::*, ptr::*}};

use thiserror::Error;

use std::fmt::Debug;

#[derive(Error, Debug, Clone, PartialEq)]
pub enum ParserError {
    #[error("Could not match token")]
    CouldNotMatchToken,

    #[error("Empty")]
    Empty,
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

    fn pred<F>(self, pred_fn: F) -> BoxedParser<'a, O>
    where
        Self: Sized + 'a,
        O: 'a,
        F: Fn(&O) -> bool + 'a,
    {
        BoxedParser::new(pred(self, pred_fn))
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
    pub fn new<P>(parser: P) -> Self
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

pub fn pair<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        p1.parse(input).and_then(|(next_input, result1)| {
            p2.parse(next_input)
                .map(|(last_input, result2)| (last_input, (result1, result2)))
        })
    }
}

pub fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A> + 'a,
    F: Fn(A) -> B + 'a,
{
    move |input| parser.parse(input).map(|(next, result)| (next, map_fn(result)))
}

pub fn pred<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |input| {
        if let Ok((next_input, value)) = parser.parse(input) {
            if predicate(&value) {
                return Ok((next_input, value));
            }
        }
        Err(ParserError::CouldNotMatchToken)
    }
}

pub fn left<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1> + 'a,
    P2: Parser<'a, R2> + 'a,
    R1: 'a,
    R2: 'a,
{
    pair(p1, p2).map( |(l, _)| l)
}

pub fn right<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1> + 'a,
    P2: Parser<'a, R2> + 'a,
    R1: 'a,
    R2: 'a,
{
    pair(p1, p2).map( |(_, r)| r)
}

pub fn or<'a, P1, P2, R>(p1: P1, p2: P2) -> impl Parser<'a, R>
where
    P1: Parser<'a, R>,
    P2: Parser<'a, R>,
{
    move |input| match p1.parse(input) {
        ok @ Ok(_) => ok,
        Err(_) => p2.parse(input),
    }
}

pub fn zero_or_more<'a, P, A>(p: P) -> BoxedParser<'a, Vec<A>>
where
    P: Parser<'a, A> + 'a
{
    BoxedParser::new(move |mut input| {
        let mut results = vec![];
        while let Ok((next_input, result)) = p.parse(input) {
            if next_input == input {
                // No progress, return error to prevent infinite loop
                return Err(ParserError::CouldNotMatchToken);
            }
            results.push(result);
            input = next_input;
        }
        Ok((input, results))
    })
}

pub fn one_or_more<'a, P, A>(p: P) -> BoxedParser<'a, Vec<A>>
where
    P: Parser<'a, A> + 'a,
{
    BoxedParser::new(move |mut input| {
        let mut result = Vec::new();

        if let Ok((next_input, first_item)) = p.parse(input) {
            input = next_input;
            result.push(first_item);
        } else {
            return Err(ParserError::CouldNotMatchToken);
        }

        while let Ok((next_input, next_item)) = p.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    })
}

pub fn zero_or_one<'a, P, A>(p: P) -> BoxedParser<'a, Option<A>> 
where
    P: Parser<'a, A> + 'a
{
    BoxedParser::new(move |input| {
        match p.parse(input) {
            Ok((next, result)) => Ok((next, Some(result))),
            Err(_) => Ok((input, None)),
        }
    })
}

pub fn match_token<'a>(expected: Token) -> impl Parser<'a, ()> {
    move |toks: &'a [Token]| match toks.get(0) {
        Some(tok) if *tok == expected => Ok((&toks[1..], ())),
        _ => Err(ParserError::CouldNotMatchToken),
    }
}

pub fn parse_identifier<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Identifier> {
    match input.get(0) {
        Some(t) => match t { 
            Token::Identifier(i) => Ok((&input[1..], ast::Identifier(i.clone()))),
            _ => Err(ParserError::CouldNotMatchToken),
        },
        None => Err(ParserError::CouldNotMatchToken),
    }
}

pub fn parse_literal<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Literal> {
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

pub fn parse_operator<'a>(input: &'a [Token]) -> ParseResult<'a, ast::BinOperator> {
    use ast::BinOperator;
    match input.get(0) {
        Some(t) => match t {
            Token::Plus => Ok((&input[1..], BinOperator::Plus)),
            Token::Minus => Ok((&input[1..], BinOperator::Minus)),
            Token::Multiply => Ok((&input[1..], BinOperator::Multiply)),
            Token::Divide => Ok((&input[1..], BinOperator::Divide)),
            Token::Exp => Ok((&input[1..], BinOperator::Exp)),
            Token::And => Ok((&input[1..], BinOperator::And)),
            Token::Or => Ok((&input[1..], BinOperator::Or)),
            Token::EqualsEquals => Ok((&input[1..], BinOperator::EqualsEquals)),
            Token::NotEquals => Ok((&input[1..], BinOperator::NotEquals)),
            Token::Less => Ok((&input[1..], BinOperator::Less)),
            Token::LessEquals => Ok((&input[1..], BinOperator::LessEquals)),
            Token::Greater => Ok((&input[1..], BinOperator::Greater)),
            Token::GreaterEquals => Ok((&input[1..], BinOperator::GreaterEquals)),
            _ => Err(ParserError::CouldNotMatchToken),
        },
        _ => Err(ParserError::CouldNotMatchToken),
    }
}

pub fn empty_parser<'a>() -> impl Parser<'a, ()> {
    move |input| Ok((input, ()))
}

pub fn parse<'a>(input: &'a [Token]) -> ast::Expr {
    let (_, result) = parse_expression().parse(input).unwrap();
    result
}

pub fn parse_expression<'a>() -> impl Parser<'a, ast::Expr> {
    or(
        parse_fn_expression(),
        or(
            parse_bin_op(0),
            or(
                parse_set_literal(),
                parse_let_in_expression)))
}

// ********* BINARY EXPRESSION RELATED ***********

// PRATT parser for binary expressions
pub fn parse_bin_op<'a>(min_bp: u8) -> impl Parser<'a, ast::Expr> {
    move |input| {
        let (mut input, mut lhs) = parse_primary().parse(input)?;
        
        loop {
            let (rest, op) = match parse_operator(input) {
                Ok((i, o)) => (i, o),
                Err(_) => break,
            };

            let (l_bp, r_bp) = match infix_binding_power(op.clone()) {
                bp if bp.0 >= min_bp => bp,
                _ => break,
            };

            let (rest, rhs) = parse_bin_op(r_bp).parse(rest)?;
            input = rest;
            
            lhs = ast::Expr::BinOpExpr(op, P(lhs), P(rhs))
        }

        Ok((input, lhs))
    }
}

fn infix_binding_power(op: ast::BinOperator) -> (u8, u8) {
    use ast::BinOperator::*;
    match op {
        Or => (1, 2),
        And => (3, 4),
        EqualsEquals | NotEquals | Less | LessEquals | Greater | GreaterEquals => (5, 6),
        Plus | Minus => (9, 10),
        Multiply | Divide => (11, 12),
        Exp => (14, 13),
    }
}

pub fn parse_parenthesized_expression<'a>() -> impl Parser<'a, ast::Expr> {
    move |input| {
        let (input, _) = match_token(Token::LeftParen).parse(input)?;
        let (input, expr) = parse_expression().parse(input)?;
        let (input, _) = match_token(Token::RightParen).parse(input)?;

        Ok((input, expr))
    }
}

pub fn parse_unary_minus<'a>() -> impl Parser<'a, ast::Expr> {
    move |input| {
        let (input, _) = match_token(Token::Minus).parse(input)?;
        let (input, prim) = parse_primary().parse(input)?;

        Ok((input, prim))
    }
}

pub fn parse_primary<'a>() -> impl Parser<'a, ast::Expr> {
    or(
        parse_parenthesized_expression(),
        or(
            parse_unary_minus(),
            or(
                parse_literal.map(|l| ast::Expr::LitExpr(l)),
                parse_identifier.map(|i| ast::Expr::IdentExpr(i)))))
}
//
// *************** FUNCTION EXPRESSION ***********************

pub fn parse_fn_expression<'a>() -> impl Parser<'a, ast::Expr> {
    parse_fn_fnexpr().map(|f| ast::Expr::FnExpr(P(f)))
}

pub fn parse_fn_fnexpr<'a>() -> impl Parser<'a, ast::FnExpr> { 
    or(
    parse_fn_expression_single,
    parse_fn_expression_case)
}

pub fn parse_fn_expression_single<'a>(input: &'a [Token]) -> ParseResult<'a, ast::FnExpr> {
    pair(
        left(
            parse_fn_params(),
            match_token(Token::FatArrow)),
        parse_expression())
    .map(|(params, expr)| ast::FnExpr::Single(params, expr))
    .parse(input)
}

pub fn parse_fn_expression_case<'a>(input: &'a [Token]) -> ParseResult<'a, ast::FnExpr> {
    right(
        match_token(Token::CaseKeyword),
        one_or_more(parse_fn_expression_case_single))
    .map(|fn_exprs| ast::FnExpr::Case(fn_exprs))
    .parse(input)
}

pub fn parse_fn_expression_case_single<'a>(input: &'a [Token]) -> ParseResult<'a, ast::FnExpr> {
    pair(
        left(
            parse_fn_case_params(),
            match_token(Token::FatArrow)),
        parse_expression())
    .map(|(params, expr)| ast::FnExpr::Single(params, expr))
    .parse(input)
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

pub fn parse_fn_param<'a>() -> impl Parser<'a, ast::FnParam> {
    or(
    parse_identifier.map(|i| ast::FnParam::Identifier(i)),
        or(
            parse_fn_param_set_deconstruct(),
            parse_fn_param_set_selector()))
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

pub fn parse_fn_case_param<'a>() -> impl Parser<'a, ast::FnParam> {
    or(
        parse_identifier.map(|i| ast::FnParam::Identifier(i)),
        or(
            parse_literal.map(|l| ast::FnParam::Literal(l)),
            BoxedParser::new(parse_fn_param_set_deconstruct())))
}

pub fn parse_fn_param_set_deconstruct<'a>() -> impl Parser<'a, ast::FnParam> {
    right(
        match_token(Token::LeftBrace),
        left(
                zero_or_one(
                    pair(
                        parse_identifier,
                        zero_or_more(
                            right(
                                match_token(Token::Comma),
                                parse_identifier)))),
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
            parse_identifier),
        right(
            match_token(Token::Colon),
                left(
                    parse_identifier,
                    match_token(Token::RightParen))))
    .map(|(first, rest)| ast::FnParam::SetSelector(first, rest))                
}

// *************** SET LITERAL ***********************

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
        parse_set_literal_field,
        zero_or_more(
            right(
                match_token(Token::Comma),
                parse_set_literal_field)))
    .map(|(first, rest)| {
        let mut fields = vec![first];
        fields.extend(rest);
        fields
    }) 
}

pub fn parse_set_literal_field<'a>(input: &'a [Token]) -> ParseResult<'a, (ast::Identifier, ast::Expr)> {
    pair(
        left(
            parse_identifier,
            match_token(Token::Equals)),
        parse_expression())
    .parse(input)
}
// **************** LET IN ******************
pub fn parse_let_in_expression<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Expr> {
    right(
        match_token(Token::LetKeyword),
        pair(
            left(
                zero_or_more(parse_statement()),
                match_token(Token::InKeyword)),
            parse_expression()))
    .map(|(v, e)| ast::Expr::LetInExpr(v, P(e)))
    .parse(input)
}

// **************** STATEMENTS **********************

pub fn parse_statement<'a>() -> impl Parser<'a, ast::Stmt> {
    or(parse_inferred_assignment,parse_type_assignment)
}

pub fn parse_type_assignment<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Stmt> {
    pair(
        pair(
            parse_identifier,
            right(
                match_token(Token::Colon),
                parse_identifier)),
        right(
            match_token(Token::Equals),
            parse_expression()))
    .map(|((i, t), e)| ast::Stmt::Asmt(i, Some(ast::TypeName(t)), e))
    .parse(input)
}

pub fn parse_inferred_assignment<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Stmt> {
    pair(
        parse_identifier,
        right(
            match_token(Token::InferredEquals),
            parse_expression()))
    .map(|(id, e)| ast::Stmt::Asmt(id, None, e))
    .parse(input)
}
