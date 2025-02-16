use crate::err::CombineSourceSpan;
use crate::lexer::prelude::{Token, TokenKind};
use crate::parser::ast::*;
use crate::parser::combinator::*;
use crate::parser::error::*;
use crate::parser::parser::*;
use crate::parser::ptr::*;
use crate::parser::stmt::*;
use crate::parser::tokenstream::{TokenResult, TokenStream};

pub fn parse_expression<'a>() -> impl Parser<'a, Expr> {
    or_n(vec![
        BoxedParser::new(parse_fn_expr_as_expr()),
        BoxedParser::new(parse_let_in_expr),
        BoxedParser::new(parse_if_then_else),
        BoxedParser::new(parse_case_expr),
        BoxedParser::new(parse_bin_op(0)),
    ])
}

// ****************** FN EXPR ***************************
pub fn parse_fn_expr_as_expr<'a>() -> impl Parser<'a, Expr> {
    parse_fn_expr().map(|f, s| Expr::FnExpr(P(f), s))
}

pub fn parse_fn_expr<'a>() -> impl Parser<'a, FnExpr> {
    or(
        parse_fn_expr_single,
        parse_fn_expr_case)
}

pub fn parse_fn_expr_single(input: TokenStream) -> Output<FnExpr> {
    pair(
        left(
            parse_fn_expr_params(),
            match_token(TokenKind::FatArrow)),
        or(
            parse_expression(),
            surrounded(
                match_token(TokenKind::LeftParen),
                parse_comma_seperated_expressions.map(|exprs, span| Expr::TupleExpr(exprs, span)),
                match_token(TokenKind::RightParen))))
        .map(|(params, expr), span| FnExpr::FnExpr(params, expr, span))
        .parse(input)
}

pub fn parse_fn_expr_case(input: TokenStream) -> Output<FnExpr> {
    right(
        match_token(TokenKind::CaseKeyword),
        one_or_more(parse_fn_expr_case_branch))
        .map(|fn_exprs, span| FnExpr::CaseFnExpr(fn_exprs, span))
        .parse(input)
}

pub fn parse_fn_expr_case_branch(input: TokenStream) -> Output<FnExpr> {
    pair(
        left(
            parse_fn_expr_case_params(),
            match_token(TokenKind::FatArrow)),
        left(
            or(
                parse_expression(),
                surrounded(
                    match_token(TokenKind::LeftParen),
                    parse_comma_seperated_expressions.map(|exprs, span| Expr::TupleExpr(exprs, span)),
                    match_token(TokenKind::RightParen))),
            match_token(TokenKind::SemiColon)))
        .map(|(params, expr), span| FnExpr::FnExpr(params, expr, span))
        .parse(input)
}

pub fn parse_fn_expr_params<'a>() -> impl Parser<'a, Vec<FnParam>> {
    pair(
        parse_fn_expr_param(),
        zero_or_more(
            right(
                match_token(TokenKind::Comma),
                parse_fn_expr_param())))
        .map(|(first_param, params), _| {
            let mut final_params = vec![first_param];
            final_params.extend(params);
            final_params
        })
}

pub fn parse_fn_expr_param<'a>() -> impl Parser<'a, FnParam> {
    or_n(vec![
        parse_identifier().map(|i, s| FnParam::IdentParam(i, s)),
        BoxedParser::new(parse_set_deconstruct().map(|idents, s| FnParam::SetDeconstructParam(idents, s))),
        BoxedParser::new(parse_fn_param_set_selector()),
        BoxedParser::new(parse_empty_set.map(|_, s| FnParam::SetDeconstructParam(vec![], s))),
    ])
}

pub fn parse_fn_expr_case_params<'a>() -> impl Parser<'a, Vec<FnParam>> {
    pair(
        parse_fn_expr_case_param(),
        zero_or_more(
            right(
                match_token(TokenKind::Comma),
                parse_fn_expr_case_param())))
        .map(|(first_param, params), _| {
            let mut final_params = vec![first_param];
            final_params.extend(params);
            final_params
        })
}

pub fn parse_fn_expr_case_param<'a>() -> impl Parser<'a, FnParam> {
    or_n(vec![
        BoxedParser::new(parse_fn_expr_case_deconstruct_union_type()),
        BoxedParser::new(parse_fn_expr_param()),
        parse_literal().map(|l, s| FnParam::LiteralParam(l, s))
    ])
}

// ***************** BIN EXPR ******************************

// PRATT parser for binary expressions
pub fn parse_bin_op<'a>(min_bp: u8) -> impl Parser<'a, Expr> {
    move |input| {
        let (mut input, mut lhs, mut span) = parse_primary().parse(input)?;

        loop {
            let (rest, op, op_span) = match parse_operator().parse(input) {
                Ok((i, o, sp)) => (i, o, sp),
                Err(_) => break,
            };
            span = span.start..op_span.end;

            let (_, r_bp) = match get_infix_binding_power(op.clone()) {
                bp if bp.0 >= min_bp => bp,
                _ => break,
            };

            let (rest, rhs, sp2) = parse_bin_op(r_bp).parse(rest)?;
            input = rest;
            span = span.start..sp2.end;

            lhs = Expr::BinOpExpr(op, P(lhs), P(rhs), span.clone())
        }

        Ok((input, lhs, span))
    }
}

fn get_infix_binding_power(op: BinOperator) -> (u8, u8) {
    use crate::parser::ast::BinOperator::*;
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

pub fn parse_parenthesized_expression<'a>() -> impl Parser<'a, Expr> {
    right(
        match_token(TokenKind::LeftParen),
        left(
            parse_expression(),
            match_token(TokenKind::RightParen)))
}

pub fn parse_unary_op(input: TokenStream) -> Output<Expr> {
    pair(
        parse_unary_operator_from_token,
        parse_primary())
        .map(|(op, expr), span| Expr::UnaryOp(op, P(expr), span))
        .parse(input)
}

pub fn parse_unary_operator_from_token(input: TokenStream) -> Output<UnaryOperator> {
    match input.next() {
        Some((res, next_input)) => {
            let op = match res.kind() {
                TokenKind::Minus => UnaryOperator::Negative,
                TokenKind::Bang => UnaryOperator::Not,
                _ => return Err(ParserError::new()
                    .message(format!("expected unary op, found {:?}", res.kind()))
                    .span(res.span())
                    .build())
            };
            Ok((next_input, op, res.span()))
        },
        None => Err(ParserError::new()
            .message("expected eof".to_string())
            .span(input.last_span())
            .build())
    }
}

pub fn parse_member_expr<'a>() -> impl Parser<'a, Expr> {
    pair(
        parse_identifier(),
        one_or_more(
            right(
                match_token(TokenKind::Dot),
                parse_identifier())))
        .map(|(first, rest), span| {
            // Start building from the leftmost identifier (foo) outward
            let mut iter = rest.into_iter();
            let mut base = MemberExpr {
                identifier: iter.next().unwrap(),
                base: MemberExprBase::Member(first),
            };

            // Fold the remaining identifiers correctly so that the rightmost is the outermost
            for identifier in iter {
                base = MemberExpr {
                    identifier,
                    base: MemberExprBase::MemberExpr(P(base)),
                };
            }

            Expr::MemberExpr(base, span)
        })
}

pub fn parse_primary<'a>() -> impl Parser<'a, Expr> {
    or_n(vec![
        BoxedParser::new(parse_parenthesized_expression()),
        BoxedParser::new(parse_set_literal()),
        BoxedParser::new(parse_set_array()),
        BoxedParser::new(parse_member_expr()),
        BoxedParser::new(parse_fn_call_expr),
        BoxedParser::new(parse_literal().map(|l, s| Expr::LitExpr(l, s))),
        BoxedParser::new(parse_identifier().map(|i, s| Expr::IdentExpr(i, s))),
    ])
}

// **************** FN_CALL ***************************

pub fn parse_fn_call_expr(input: TokenStream) -> Output<Expr> {
    pair(
        parse_identifier(),
        one_or_more(parse_fn_arg))
        .map(|(func_name, params), span| {
            Expr::FnCallExpr(func_name, params, span)
        })
        .parse(input)
}

pub fn parse_fn_arg(input: TokenStream) -> Output<Expr> {
    or_n(vec![
        BoxedParser::new(parse_parenthesized_expression()),
        BoxedParser::new(parse_set_literal()),
        BoxedParser::new(parse_set_array()),
        BoxedParser::new(parse_literal().map(|l, s| Expr::LitExpr(l, s))),
        BoxedParser::new(parse_member_expr()),
        BoxedParser::new(parse_fn_arg_identifier())])
        .parse(input)
}

pub fn parse_fn_arg_identifier<'a>() -> impl Parser<'a, Expr> {
    left(
        parse_identifier().map(|(i), span| Expr::IdentExpr(i, span)),
        not(
            or_n(vec![
                BoxedParser::new(match_token(TokenKind::Comma)),
                BoxedParser::new(match_token(TokenKind::InferredEquals)),
                BoxedParser::new(match_token(TokenKind::Colon)),
                BoxedParser::new(match_token(TokenKind::ColonColon)),
                BoxedParser::new(match_token(TokenKind::Equals)),
            ])
        ))
}

// *********** IF THEN ELSE *******************

pub fn parse_if_then_else(input: TokenStream) -> Output<Expr> {
    pair(
        right(
            match_token(TokenKind::IfKeyword),
            left(
                parse_expression(),
                match_token(TokenKind::ThenKeyword))),
        pair(
            pair(
                parse_expression(),
                zero_or_more(parse_elseif_p_then_e)),
            right(
                match_token(TokenKind::ElseKeyword),
                parse_expression())))
        .map(|(pred, ((expr, elseifs), else_expr)), span| {
            let mut final_result = vec![(pred, expr)];
            final_result.extend(elseifs);

            Expr::IfThenElseIfExpr(final_result, P(else_expr), span)
        })
        .parse(input)
}

pub fn parse_elseif_p_then_e(input: TokenStream) -> Output<(Expr, Expr)> {
    pair(
        right(
            match_token(TokenKind::ElseIfKeyword),
            left(
                parse_expression(),
                match_token(TokenKind::ThenKeyword))),
        parse_expression())
        .parse(input)
}

pub fn parse_comma_seperated_expressions(input: TokenStream) -> Output<Vec<Expr>> {
    pair(
        parse_expression(),
        zero_or_more(
            right(
                match_token(TokenKind::Comma),
                parse_expression())))
        .map(|(first_ident, idents), _| {
            let mut final_idents = vec![first_ident];
            final_idents.extend(idents);
            final_idents
        })
        .parse(input)
}

// **************** LET IN ******************
pub fn parse_let_in_expr(input: TokenStream) -> Output<Expr> {
    pair(
        surrounded(
            match_token(TokenKind::LetKeyword),
            zero_or_more(parse_statement()),
            match_token(TokenKind::InKeyword)),
        parse_expression())
        .map(|(v, e), span| Expr::LetInExpr(v, P(e), span))
        .parse(input)
}

// ***************** CASE EXPRESSION *****************

pub fn parse_case_expr(input: TokenStream) -> Output<Expr> {
    right(
        match_token(TokenKind::CaseKeyword),
        pair(
            parse_identifier(),
            one_or_more(parse_fn_expr_case_branch)))
        .map(|(first, rest), span| {
            let result: Vec<(Vec<FnParam>, Expr)> =
                rest.iter()
                    .map(|f| match f.clone() {
                        FnExpr::FnExpr(a, b, _) => (a, b),
                        _ => unreachable!()
                    }).collect();
            Expr::CaseExpr(first, result, span)
        })
        .parse(input)
}

// *************** SET LITERAL ***********************

pub fn parse_set_array<'a>() -> impl Parser<'a, Expr> {
    or(
        parse_empty_set.map(|_, s| Expr::ArrayExpr(vec![], s)),
        right(
            match_token(TokenKind::LeftBrace),
            left(
                parse_comma_seperated_expressions,
                match_token(TokenKind::RightBrace)))
            .map(|exprs, s| Expr::ArrayExpr(exprs, s)))
}

pub fn parse_empty_set(input: TokenStream) -> Output<()> {
    pair(
        match_token(TokenKind::LeftBrace),
        match_token(TokenKind::RightBrace))
        .map(|_, _| ())
        .parse(input)
}

pub fn parse_set_literal<'a>() -> impl Parser<'a, Expr> {
    or(
        parse_empty_set.map(|_, s| Expr::ArrayExpr(vec![], s)),
        right(
            match_token(TokenKind::LeftBrace),
            left(
                zero_or_one(parse_set_literal_comma_seperated_fields()),
                match_token(TokenKind::RightBrace)))
            .map(|v, span| {
                let mut final_fields: Vec<(Identifier, Expr)> = vec![];
                match v {
                    Some(v) => final_fields.extend(v),
                    None => {}
                }
                Expr::SetExpr(final_fields, span)
            }))
}

pub fn parse_set_literal_comma_seperated_fields<'a>() -> impl Parser<'a, Vec<(Identifier, Expr)>> {
    pair(
        parse_set_literal_field,
        zero_or_more(
            right(
                match_token(TokenKind::Comma),
                parse_set_literal_field)))
        .map(|(first, rest), _| {
            let mut fields = vec![first];
            fields.extend(rest);
            fields
        })
}

pub fn parse_set_literal_field(input: TokenStream) -> Output<(Identifier, Expr)> {
    pair(
        left(
            parse_identifier(),
            match_token(TokenKind::Equals)),
        parse_expression())
        .parse(input)
}

pub fn parse_set_deconstruct<'a>() -> impl Parser<'a, Vec<Identifier>> {
    right(
        match_token(TokenKind::LeftBrace),
        left(
            parse_comma_seperated_identifiers,
            match_token(TokenKind::RightBrace)))
}

pub fn parse_fn_param_set_selector<'a>() -> impl Parser<'a, FnParam> {
    pair(
        right(
            match_token(TokenKind::LeftBrace),
            parse_identifier()),
        right(
            match_token(TokenKind::Colon),
            left(
                parse_identifier(),
                match_token(TokenKind::RightBrace))))
        .map(|(first, rest), s| FnParam::SetSelectorParam(first, rest, s))
}

pub fn parse_fn_expr_case_deconstruct_union_type<'a>() -> impl Parser<'a, FnParam> {
    surrounded(
        match_token(TokenKind::LeftParen),
        pair(
            parse_identifier(),
            one_or_more(parse_identifier())),
        match_token(TokenKind::RightParen))
        .map(|(first, rest), s| FnParam::TypeUnionParam(first, rest, s))
}
