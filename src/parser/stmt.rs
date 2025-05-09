use crate::lexer::prelude::TokenKind;
use crate::parser::ast::{Expr, FnPatternParam, Identifier, Stmt};
use crate::parser::combinator::{cut, left, one_or_more, or, or_n, pair, right, surrounded, zero_or_more};
use crate::parser::expr::{parse_comma_seperated_expressions, parse_expression, parse_fn_call_expr, parse_fn_expr_case_params, parse_set_deconstruct};
use crate::parser::parser::{match_token, parse_identifier, BoxedParser, Output, Parser};
use crate::parser::tokenstream::TokenStream;

pub fn parse_statement(input: TokenStream) -> Output<Stmt> {
    or_n(vec![
        BoxedParser::new(parse_inferred_assignment),
        BoxedParser::new(parse_type_assignment),
        BoxedParser::new(parse_inferred_multi_assign_statement()),
        BoxedParser::new(parse_typed_multi_assign_statement()),
        BoxedParser::new(parse_set_deconstruct_assignment),
        BoxedParser::new(parse_proc_call),
        BoxedParser::new(parse_case_statement),
        parse_fn_call_stmt(),
    ])
        .parse_next(input)
}

pub fn parse_fn_call_stmt<'a>() -> BoxedParser<'a, Stmt> {
    parse_fn_call_expr
        .map(|expr, _| match expr {
            Expr::FnCallExpr(ident, args, span) => Stmt::FnCallStmt(ident, args, span),
            _ => unreachable!(),
        })
}

pub fn parse_proc_call(input: TokenStream) -> Output<Stmt> {
    left(
        parse_identifier(),
        match_token(TokenKind::Bang))
        .map(|ident, span| Stmt::ProcCallStmt(ident, span))
        .parse_next(input)
}


pub fn parse_type_assignment(input: TokenStream) -> Output<Stmt> {
    pair(
        pair(
            parse_identifier(),
            right(
                match_token(TokenKind::Colon),
                parse_identifier())),
        right(
            match_token(TokenKind::Equals),
            parse_expression))
        .map(|((i, t), e), span| Stmt::AssignStmt(i, Some(t), e, span))
        .parse_next(input)
}

pub fn parse_inferred_assignment(input: TokenStream) -> Output<Stmt> {
    pair(
        parse_identifier(),
        right(
            match_token(TokenKind::InferredEquals),
            cut(parse_expression)))
        .map(|(id, e), span| Stmt::AssignStmt(id, None, e, span))
        .parse_next(input)
}

pub fn parse_inferred_multi_assign_statement<'a>() -> impl Parser<'a, Stmt> {
    pair(
        left(
            surrounded(
                match_token(TokenKind::LeftParen),
                parse_comma_seperated_identifiers,
                match_token(TokenKind::RightParen)),
            match_token(TokenKind::InferredEquals)),
        surrounded(
            match_token(TokenKind::LeftParen),
            parse_comma_seperated_expressions.map(|exprs, span| Expr::TupleExpr(exprs, span)),
            match_token(TokenKind::RightParen)))
        .map(|(idents, exprs), span| Stmt::MultiAssignStmt(idents, None, exprs, span))
}

pub fn parse_typed_multi_assign_statement<'a>() -> impl Parser<'a, Stmt> {
    pair(
        pair(
            surrounded(
                match_token(TokenKind::LeftParen),
                parse_comma_seperated_identifiers,
                match_token(TokenKind::RightParen)),
            right(
                match_token(TokenKind::Colon),
                surrounded(
                    match_token(TokenKind::LeftParen),
                    parse_comma_seperated_identifiers,
                    match_token(TokenKind::RightParen)))),
        right(
            match_token(TokenKind::Equals),
            surrounded(
                match_token(TokenKind::LeftParen),
                parse_comma_seperated_expressions.map(|exprs, span| Expr::TupleExpr(exprs, span)),
                match_token(TokenKind::RightParen))))
        .map(|((idents, types), exprs), span| Stmt::MultiAssignStmt(idents, Some(types), exprs, span))
}
pub fn parse_comma_seperated_identifiers(input: TokenStream) -> Output<Vec<Identifier>> {
    pair(
        parse_identifier(),
        zero_or_more(
            right(
                match_token(TokenKind::Comma),
                parse_identifier())))
        .map(|(first_ident, idents), _| {
            let mut final_idents = vec![first_ident];
            final_idents.extend(idents);
            final_idents
        })
        .parse_next(input)
}

pub fn parse_set_deconstruct_assignment(input: TokenStream) -> Output<Stmt> {
    pair(
        parse_set_deconstruct(),
        right(
            or(
                match_token(TokenKind::InferredEquals),
                match_token(TokenKind::Equals)),
            parse_comma_seperated_expressions.map(|exprs, span| Expr::TupleExpr(exprs, span))))
        .map(|(decon_set, exprs), span| Stmt::SetDeconstructAssignStmt(decon_set, exprs, span))
        .parse_next(input)
}

pub fn parse_case_statement(input: TokenStream) -> Output<Stmt> {
    right(
        match_token(TokenKind::CaseKeyword),
        pair(
            parse_expression,
            one_or_more(parse_case_stmt_branch)))
        .map(|(expr, branches), span| Stmt::CaseStmt(expr, branches, span))
        .parse_next(input)
}

pub fn parse_case_stmt_branch(input: TokenStream) -> Output<(Vec<FnPatternParam>, Stmt)> {
    pair(
        left(
            parse_fn_expr_case_params(),
            match_token(TokenKind::FatArrow)),
        left(
            or_n(vec![
                pair(
                    match_token(TokenKind::LeftParen),
                    match_token(TokenKind::RightParen))
                    .map(|(_, _), span| Stmt::Empty(span)),
                parse_fn_call_stmt(),
                BoxedParser::new(parse_proc_call),
            ]),
            match_token(TokenKind::SemiColon)))
        .parse_next(input)
}
