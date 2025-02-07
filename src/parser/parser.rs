use crate::{lexer::token::Token, parser::{ast, ptr::*}};

use thiserror::Error;

use std::fmt::Debug;

#[derive(Error, Debug, Clone, PartialEq)]
pub enum ParserError {
    #[error("Could not match token")]
    CouldNotMatchToken,

    #[error("Expected a token but no remaining tokens.")]
    UnexpectedEOF,

    #[error("Expected an Identifier, got: {}", got)]
    ExpectedIdentifierGot {
        got : Token,
    },

    #[error("Expected a Literal (Integer, Float, Bool, String, Set), got: {}", got)]
    ExpectedLiteralGot {
        got : Token,
    },

    #[error("Expected an Operator, got: {}", got)]
    ExpectedOperatorGot {
        got : Token,
    },
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

pub fn seq<'a, P, R>(parsers: Vec<P>) -> impl Parser<'a, Vec<R>> 
where
    P : Parser<'a, R>,
{
    move |input| {
        parsers.iter().fold(Ok((input, Vec::new())), |prev_results, parser| {
            prev_results.and_then(|(next_inputs, mut parser_outputs)| {
                parser.parse(next_inputs).map(|(next_inputs, result)| {
                    parser_outputs.push(result);
                    (next_inputs, parser_outputs)
                })
            })
        })
    }
}

pub fn or_n<'a, Out>(parsers: Vec<BoxedParser<'a, Out>>) -> impl Parser<'a, Out> 
where
    Out: 'a,
{
    move |input| {
        for p in &parsers {
            if let Ok(result) = p.parse(input) {
                return Ok(result);
            }
        }
        Err(ParserError::CouldNotMatchToken)
    }
}
pub fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A> + 'a,
    F: Fn(A) -> B + 'a,
{
    move |input| parser.parse(input).map(|(next, result)| (next, map_fn(result)))
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

pub fn one_or_more<'a, P, A>(p: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A> + 'a,
{
    move |mut input| {
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
    }
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
        _ => Err(ParserError::UnexpectedEOF),
    }
}

pub fn parse_identifier<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Identifier> {
    match input.get(0) {
        Some(t) => match t { 
            Token::Identifier(i) => Ok((&input[1..], ast::Identifier(i.clone()))),
            _ => Err(ParserError::ExpectedIdentifierGot{got: t.clone()}),
        },
        None => Err(ParserError::UnexpectedEOF),
    }
}

pub fn parse_literal<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Literal> {
    match input.get(0) {
        Some(t) => match t {
            Token::Bool(b) => Ok((&input[1..], ast::Literal::from(*b))),
            Token::Integer(i) => Ok((&input[1..], ast::Literal::from(*i))),
            Token::Float(f) => Ok((&input[1..], ast::Literal::from(*f))),
            Token::String(s) => Ok((&input[1..], ast::Literal::from(s.clone()))),
            _ => Err(ParserError::ExpectedLiteralGot { got: t.clone() }),
        },
        None => Err(ParserError::UnexpectedEOF),
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
            Token::Colon => Ok((&input[1..], BinOperator::AppendSet)),
            _ => Err(ParserError::ExpectedOperatorGot{got: t.clone()}),
        },
        _ => Err(ParserError::UnexpectedEOF),
    }
}

// *************** DEFINITIONS **************************

pub fn parse_module<'a>() -> impl Parser<'a, ast::Module> {
    pair(
        zero_or_more(parse_definition()),
        zero_or_one(parse_expression()))
    .map(|(defs, expr)| ast::Module(defs, expr))
}

pub fn parse_definition<'a>() -> impl Parser<'a, ast::Def> { 
    or_n(vec![
        BoxedParser::new(parse_instance_def),
        BoxedParser::new(parse_type_def()),
        BoxedParser::new(parse_class_def()),
        BoxedParser::new(parse_fn_def),
        BoxedParser::new(parse_proc_def()),
    ])
}

pub fn parse_fn_def<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Def> {
    pair(
        pair(
            parse_fn_def_header,
            parse_fn_def_types()),
        parse_fn_expr())
    .map(|((i, (tp, rt)), exp)| ast::Def::FnDef(i, tp, rt, exp))
    .parse(input)
}

pub fn parse_fn_def_header<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Identifier> {
    left(
        right(
            match_token(Token::DefKeyword),
            parse_identifier),
        match_token(Token::ColonColon))
    .parse(input)
}

pub fn parse_fn_def_types<'a>() -> impl Parser<'a, (Vec<ast::TypeParam>, ast::TypeParam)> {
    pair(
        parse_fn_type_params(),
        right(
            match_token(Token::FatArrow),
            parse_fn_type_param()))
    .map(|(type_params, return_type)| (type_params, return_type))
}

pub fn parse_fn_type_params<'a>() -> impl Parser<'a, Vec<ast::TypeParam>> {
    pair(
        parse_fn_type_param(),
        zero_or_more(
            right(
                match_token(Token::Comma),
                parse_fn_type_param())))
    .map(|(first_type, types)| {
        let mut final_types = vec![first_type];
        final_types.extend(types);
        final_types
    })
}

pub fn parse_fn_type_param<'a>() -> impl Parser<'a, ast::TypeParam> {
    or_n(vec![
        parse_identifier.map(|i| ast::TypeParam::Type(i)),
        BoxedParser::new(parse_array_type_param()),
        BoxedParser::new(parse_fn_func_type_param),
    ])
}

pub fn parse_array_type_param<'a>() -> impl Parser<'a, ast::TypeParam> {
    right(
        match_token(Token::LeftBracket),
        left(parse_identifier,
            match_token(Token::RightBracket)))
    .map(|i| ast::TypeParam::ArrayType(i))
}

pub fn parse_fn_func_type_param<'a>(input: &'a[Token]) -> ParseResult<'a, ast::TypeParam> {
    pair(
        right(
            match_token(Token::LeftParen),
            parse_fn_type_params()),
        right(
            match_token(Token::FatArrow),
            left(
                parse_fn_type_param(),
                match_token(Token::RightParen))))
    .map(|(params, ret)| ast::TypeParam::FuncType(params, P(ret)))
    .parse(input)
}

pub fn parse_proc_def<'a>() -> impl Parser<'a, ast::Def> {
    pair(
        left(
            parse_fn_def_header,
            match_token(Token::ProcType)),
        zero_or_more(parse_statement()))
    .map(|(ident, stmts)| ast::Def::ProcDef(ident, stmts))
}

pub fn parse_do_block<'a>() -> impl Parser<'a, Vec<ast::Stmt>> {
    right(
        match_token(Token::DoKeyword),
        zero_or_more(parse_statement()))
}

pub fn parse_type_def<'a>() -> impl Parser<'a, ast::Def> {
    pair(
        right(
            match_token(Token::TypeKeyword),
            left(
                parse_identifier,
                match_token(Token::ColonColon))),
        one_or_more(
            pair(
                parse_identifier,
                right(
                    match_token(Token::Colon),
                    parse_identifier))))
    .map(|(i, v)| ast::Def::TypeDef(i, v))
}

pub fn parse_class_def<'a>() -> impl Parser<'a, ast::Def> {
    pair(
        right(
            match_token(Token::ClassKeyword),
            pair(
                parse_identifier,
                left(
                    parse_class_generic_types(),
                    match_token(Token::ColonColon)))),
        parse_class_method_defs())
    .map(|((class_name, params), methods)|
        ast::Def::ClassDef(class_name, params, methods))
}

pub fn parse_class_generic_types<'a>() -> impl Parser<'a, Vec<ast::Identifier>> {
    one_or_more(parse_identifier)
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
                    parse_identifier)))
    .map(|(op, (params, ret))|
        ast::MethodDef::Operator(op, params, ret))
}

pub fn parse_class_method_type_params<'a>() -> impl Parser<'a, Vec<ast::Identifier>> {
    pair(
        parse_identifier,
        zero_or_more(
            right(
                match_token(Token::Comma),
                parse_identifier)))
    .map(|(first_type, types)| {
        let mut final_type_params = vec![first_type];

        final_type_params.extend(types);

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
            Token::WrappedExp => Ok((&input[1..], ast::MethodOperator::Exp)),
            _ => Err(ParserError::CouldNotMatchToken),
        },
        None => Err(ParserError::CouldNotMatchToken),
    }
}

pub fn parse_class_method_def_named<'a>() -> impl Parser<'a, ast::MethodDef> { 
    pair(
        left(
            parse_identifier,
            match_token(Token::ColonColon)),
        pair(
            parse_class_method_type_params(),
            right(
                    match_token(Token::FatArrow),
                    parse_identifier)))
    .map(|(ident, (params, ret))|
        ast::MethodDef::Named(ident, params, ret))
}

pub fn parse_instance_def<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Def> {
    pair(
        left(
            pair(
                parse_instance_header(),
                parse_fn_expr_params()),
            match_token(Token::ColonColon)),
        parse_instance_method_impls())
    .map(|(((cls, typ), params), impls)|
        ast::Def::InstanceDef(cls, typ, params, impls))
    .parse(input)
}

pub fn parse_instance_header<'a>() -> impl Parser<'a, (ast::Identifier, ast::Identifier)> {
    right(
        match_token(Token::InstanceKeyword),
        pair(
            parse_identifier,
            parse_identifier))
    .map(|(class, ty)| (class, ty))
}

pub fn parse_instance_method_impls<'a>() -> impl Parser<'a, Vec<ast::MethodImpl>> {
    one_or_more(parse_instance_method_impl())
}

pub fn parse_instance_method_impl<'a>() -> impl Parser<'a, ast::MethodImpl> {
    or(
        parse_instance_method_impl_op(),
        parse_instance_method_impl_named())
}

pub fn parse_instance_method_impl_op<'a>() -> impl Parser<'a, ast::MethodImpl> {
    pair(
        left(
            parse_method_operator,
            match_token(Token::FatArrow)),
        parse_expression())
    .map(|(op, expr)| ast::MethodImpl::Operator(op, expr))
}

pub fn parse_instance_method_impl_named<'a>() -> impl Parser<'a, ast::MethodImpl> {
    pair(
        left(
            parse_identifier,
            match_token(Token::FatArrow)),
        parse_expression())
    .map(|(name, expr)| ast::MethodImpl::Named(name, expr))
}
// ******************** EXPRESSION *********************

pub fn parse_expression<'a>() -> impl Parser<'a, ast::Expr> {
    or_n(vec![
        BoxedParser::new(parse_fn_expr_as_expr()),
        BoxedParser::new(parse_bin_op(0)),
        BoxedParser::new(parse_let_in_expression),
        BoxedParser::new(parse_if_then_else),
    ])
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

            let (l_bp, r_bp) = match get_infix_binding_power(op.clone()) {
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

fn get_infix_binding_power(op: ast::BinOperator) -> (u8, u8) {
    use ast::BinOperator::*;
    match op {
        Or => (1, 2),
        And => (3, 4),
        AppendSet => (5, 6),
        EqualsEquals | NotEquals | Less | LessEquals | Greater | GreaterEquals => (7, 8),
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

pub fn parse_unary_op<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Expr> {
    pair(
        parse_unary_operator_from_token,
        parse_primary())
    .map(|(op, expr)| ast::Expr::UnaryOp(op, P(expr)))
    .parse(input)
}

pub fn parse_unary_operator_from_token<'a>(input: &'a [Token]) -> ParseResult<'a, ast::UnaryOperator> {
    match input.get(0) {
        Some(t) => match t {
            Token::Minus => Ok((&input[1..], ast::UnaryOperator::Negative)),
            Token::Bang => Ok((&input[1..], ast::UnaryOperator::Not)),
            _ => Err(ParserError::CouldNotMatchToken),
        },
        None => Err(ParserError::CouldNotMatchToken),
    }
}

pub fn parse_member_expr<'a>() -> impl Parser<'a, ast::Expr> {
    pair(
        parse_identifier,
        one_or_more(
            right(
            match_token(Token::Dot),
            parse_identifier)))
    .map(|(first, rest)| {
        // Start building from the leftmost identifier (foo) outward
        let mut iter = rest.into_iter();
        let mut base = ast::MemberExpr {
            identifier: iter.next().unwrap(),
            base: ast::MemberExprBase::Member(first),
        };

        // Fold the remaining identifiers correctly so that the rightmost is the outermost
        for identifier in iter {
            base = ast::MemberExpr {
                identifier,
                base: ast::MemberExprBase::MemberExpr(P(base)),
            };
        }

        ast::Expr::MemberExpr(base)
    })
}

pub fn parse_primary<'a>() -> impl Parser<'a, ast::Expr> {
    or_n(vec![
        BoxedParser::new(parse_parenthesized_expression()),
        BoxedParser::new(parse_set_literal()),
        BoxedParser::new(parse_set_array()),
        BoxedParser::new(parse_member_expr()),
        BoxedParser::new(parse_fn_call),
        BoxedParser::new(parse_literal.map(|l| ast::Expr::LitExpr(l))),
        BoxedParser::new(parse_identifier.map(|i| ast::Expr::IdentExpr(i))),
    ])
}
//
// *************** FUNCTION EXPRESSION ***********************

pub fn parse_fn_expr_as_expr<'a>() -> impl Parser<'a, ast::Expr> {
    parse_fn_expr().map(|f| ast::Expr::FnExpr(P(f)))
}

pub fn parse_fn_expr<'a>() -> impl Parser<'a, ast::FnExpr> { 
    or(parse_fn_expr_single,parse_fn_expr_case)
}

pub fn parse_fn_expr_single<'a>(input: &'a [Token]) -> ParseResult<'a, ast::FnExpr> {
    pair(
        left(
            parse_fn_expr_params(),
            match_token(Token::FatArrow)),
        parse_comma_seperated_expressions)
    .map(|(params, expr)| ast::FnExpr::FnExpr(params, expr))
    .parse(input)
}

pub fn parse_fn_expr_case<'a>(input: &'a [Token]) -> ParseResult<'a, ast::FnExpr> {
    right(
        match_token(Token::CaseKeyword),
        one_or_more(parse_fn_expr_case_branch))
    .map(|fn_exprs| ast::FnExpr::CaseFnExpr(fn_exprs))
    .parse(input)
}

pub fn parse_fn_expr_case_branch<'a>(input: &'a [Token]) -> ParseResult<'a, ast::FnExpr> {
    pair(
        left(
            parse_fn_expr_case_params(),
            match_token(Token::FatArrow)),
        parse_comma_seperated_expressions)
    .map(|(params, expr)| ast::FnExpr::FnExpr(params, expr))
    .parse(input)
}

pub fn parse_fn_expr_params<'a>() -> impl Parser<'a, Vec<ast::FnParam>> {
    pair(
        parse_fn_expr_param(),
        zero_or_more(
            right(
            match_token(Token::Comma),
            parse_fn_expr_param())))
    .map(|(first_param, params)| {
        let mut final_params = vec![first_param];
        final_params.extend(params);
        final_params
    })
}

pub fn parse_fn_expr_param<'a>() -> impl Parser<'a, ast::FnParam> {
    or_n(vec![
        parse_identifier.map(|i| ast::FnParam::IdentParam(i)),
        BoxedParser::new(parse_set_deconstruct().map(|idents| ast::FnParam::SetDeconstructParam(idents))),
        BoxedParser::new(parse_fn_param_set_selector()),
        BoxedParser::new(parse_empty_set.map(|_| ast::FnParam::SetDeconstructParam(vec![]))),
    ])
}

pub fn parse_fn_expr_case_params<'a>() -> impl Parser<'a, Vec<ast::FnParam>> {
    pair(
        parse_fn_expr_case_param(),
        zero_or_more(
            right(
            match_token(Token::Comma),
            parse_fn_expr_case_param())))
    .map(|(first_param, params)| {
        let mut final_params = vec![first_param];
        final_params.extend(params);
        final_params
    })
}

pub fn parse_fn_expr_case_param<'a>() -> impl Parser<'a, ast::FnParam> {
    or(
        parse_fn_expr_param(),
        parse_literal.map(|l| ast::FnParam::LiteralParam(l)))
}

pub fn parse_set_deconstruct<'a>() -> impl Parser<'a, Vec<ast::Identifier>> {
    right(
        match_token(Token::LeftBrace),
        left(
            parse_comma_seperated_identiers,
            match_token(Token::RightBrace)))
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
                    match_token(Token::RightBrace))))
    .map(|(first, rest)| ast::FnParam::SetSelectorParam(first, rest))                
}

// *************** SET LITERAL ***********************

pub fn parse_set_array<'a>() -> impl Parser<'a, ast::Expr> {
    or(
        parse_empty_set.map(|_| ast::Expr::ArrayExpr(vec![])),
        right(
            match_token(Token::LeftBrace),
            left(
                    parse_comma_seperated_expressions,
                    match_token(Token::RightBrace))) 
        .map(|exprs| ast::Expr::ArrayExpr(exprs)))
}

pub fn parse_empty_set<'a>(input: &'a [Token]) -> ParseResult<'a, ()> {
    pair(
        match_token(Token::LeftBrace),
        match_token(Token::RightBrace))
    .map(|_| ())
    .parse(input)
}

pub fn parse_set_literal<'a>() -> impl Parser<'a, ast::Expr> {
    or(
        parse_empty_set.map(|_| ast::Expr::ArrayExpr(vec![])),
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
        }))
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
    or_n(vec![
        BoxedParser::new(parse_inferred_assignment),
        BoxedParser::new(parse_type_assignment),
        BoxedParser::new(parse_inferred_multi_assign_statement()),
        BoxedParser::new(parse_typed_multi_assign_statement()),
        BoxedParser::new(parse_set_deconstruct_assignment),
        BoxedParser::new(parse_proc_call),
        parse_fn_call.map(|expr| match expr {
            ast::Expr::FnCallExpr(ident, args) => ast::Stmt::FnCallStmt(ident, args),
            // absolutely unreachable since fn_call returns ast::Expr::FnCall guaranteed.
            _ => unreachable!(),
        }),
    ])
}

pub fn parse_proc_call<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Stmt> {
    left(
        parse_identifier,
        match_token(Token::Bang))
    .map(|ident| ast::Stmt::ProcCallStmt(ident))
    .parse(input)
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
    .map(|((i, t), e)| ast::Stmt::AssignStmt(i, Some(t), e))
    .parse(input)
}

pub fn parse_inferred_assignment<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Stmt> {
    pair(
        parse_identifier,
        right(
            match_token(Token::InferredEquals),
            parse_expression()))
    .map(|(id, e)| ast::Stmt::AssignStmt(id, None, e))
    .parse(input)
}

pub fn parse_inferred_multi_assign_statement<'a>() -> impl Parser<'a, ast::Stmt> {
    pair(
        left(
            parse_comma_seperated_identiers,
            match_token(Token::InferredEquals)),
        parse_comma_seperated_expressions) 
    .map(|(idents, exprs)| ast::Stmt::MultiAssignStmt(idents, None, exprs))
}

pub fn parse_typed_multi_assign_statement<'a>() -> impl Parser<'a, ast::Stmt> {
    pair(
        pair(
            parse_comma_seperated_identiers,
            right(
                match_token(Token::Colon),    
                parse_comma_seperated_identiers)),
        right(
                match_token(Token::Equals),
            parse_comma_seperated_expressions))
    .map(|((idents, types), exprs)| ast::Stmt::MultiAssignStmt(idents, Some(types), exprs))
}

pub fn parse_comma_seperated_expressions<'a>(input: &'a [Token]) -> ParseResult<'a, Vec<ast::Expr>> {
    pair(
        parse_expression(),
        zero_or_more(
            right(
            match_token(Token::Comma),
            parse_expression())))
    .map(|(first_ident, idents)| {
        let mut final_idents = vec![first_ident];
        final_idents.extend(idents);
        final_idents
    })
    .parse(input)
}

pub fn parse_comma_seperated_identiers<'a>(input: &'a [Token]) -> ParseResult<'a, Vec<ast::Identifier>> {
    pair(
        parse_identifier,
        zero_or_more(
            right(
            match_token(Token::Comma),
            parse_identifier)))
    .map(|(first_ident, idents)| {
        let mut final_idents = vec![first_ident];
        final_idents.extend(idents);
        final_idents
    })
    .parse(input)
}

pub fn parse_set_deconstruct_assignment<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Stmt> {
    pair(
        parse_set_deconstruct(),
        right(
            or(
                match_token(Token::InferredEquals),
                match_token(Token::Equals)),
            parse_comma_seperated_expressions))
    .map(|(decon_set, exprs)| ast::Stmt::SetDeconstructAssignStmt(decon_set, exprs))
    .parse(input)
}

pub fn parse_if_then_else<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Expr> {
    pair(
    right(
        match_token(Token::IfKeyword),
        left(
            parse_expression(),
            match_token(Token::ThenKeyword))),
    pair(
        pair(
                parse_expression(),
                zero_or_more(parse_elseif_p_then_e)),
        right(
            match_token(Token::ElseKeyword),
            parse_expression())))
    .map(|(pred, ((expr, elseifs), else_expr))| {
        let mut final_result = vec![(pred, expr)];
        final_result.extend(elseifs);

        ast::Expr::IfThenElseIfExpr(final_result, P(else_expr))})
    .parse(input)
}

pub fn parse_elseif_p_then_e<'a>(input: &'a [Token]) -> ParseResult<'a, (ast::Expr, ast::Expr)> {
    pair(
        right(
            match_token(Token::ElseIfKeyword),
            left(
                parse_expression(),
                match_token(Token::ThenKeyword))),
        parse_expression())
    .parse(input)
}

// **************** FN_CALL ***************************

pub fn parse_fn_call<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Expr> {
    pair(
        parse_identifier,
        left(
            one_or_more(parse_fn_arg),
            match_token(Token::Bang)))
    .map(|(func_name, params)| {
        ast::Expr::FnCallExpr(func_name, params)
    })
    .parse(input)
}

pub fn parse_fn_arg<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Expr> {
    or_n(vec![
        BoxedParser::new(parse_parenthesized_expression()),
        BoxedParser::new(parse_set_literal()),
        BoxedParser::new(parse_set_array()),
        BoxedParser::new(parse_literal.map(|l| ast::Expr::LitExpr(l))),
        BoxedParser::new(parse_member_expr()),
        BoxedParser::new(parse_identifier.map(|i| ast::Expr::IdentExpr(i))),
    ])
    .parse(input)
}
