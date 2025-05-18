use crate::lexer::prelude::TokenKind;
use crate::parser::ast::*;
use crate::parser::combinator::*;
use crate::parser::error::*;
use crate::parser::parser::*;
use crate::parser::ptr::*;
use crate::parser::stmt::*;
use crate::parser::tokenstream::{TokenResult, TokenStream};

pub fn parse_expression(input: TokenStream) -> Output<Expr> {
    or_n(vec![
        // BoxedParser::new(parse_fn_expr_as_expr),
        BoxedParser::new(parse_let_in_expr),
        BoxedParser::new(parse_if_then_else),
        BoxedParser::new(parse_case_expr),
        BoxedParser::new(parse_bin_op(0)),
    ])
        .parse_next(input)
}

pub fn parse_fn_expr_case_branch(input: TokenStream) -> Output<FnExpr> {
    pair(
        surrounded(
            match_token(TokenKind::Union),
            parse_fn_expr_case_params(),
            match_token(TokenKind::FatArrow),
        ),
        or(
            parse_expression,
            surrounded(
                match_token(TokenKind::LeftParen),
                parse_comma_seperated_expressions.map(|exprs, span| Expr::TupleExpr(exprs, span)),
                match_token(TokenKind::RightParen))))
    .map(|(params, expr), span| FnExpr::FnExpr(params, expr, span))
    .parse_next(input)
}

pub fn parse_fn_expr_param<'a>() -> impl Parser<'a, FnPatternParam> {
    or_n(vec![
        parse_identifier().map(|i, s| FnPatternParam::BindToIdentParam(i, s)),
        BoxedParser::new(parse_set_deconstruct().map(|idents, s| FnPatternParam::BindToSetDeconstructParam(idents, s))),
        BoxedParser::new(parse_fn_param_set_selector()),
        BoxedParser::new(parse_empty_set.map(|_, s| FnPatternParam::BindToSetDeconstructParam(vec![], s))),
    ])
}

pub fn parse_fn_expr_case_params<'a>() -> impl Parser<'a, Vec<FnPatternParam>> {
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

pub fn parse_fn_expr_case_param<'a>() -> impl Parser<'a, FnPatternParam> {
    or_n(vec![
        BoxedParser::new(parse_literal().map(|l, s| FnPatternParam::BindToLiteralParam(l, s))),
        BoxedParser::new(parse_fn_expr_param()),
    ])
}

// ***************** BIN EXPR ******************************

// PRATT parser for binary expressions
pub fn parse_bin_op<'a>(min_bp: u8) -> impl Parser<'a, Expr> {
    move |input: TokenStream<'a>| {
        let (mut input, mut lhs, mut span) = parse_primary().parse_next(input)?;

        loop {
            let (rest, op, op_span) = match parse_operator().parse_next(input) {
                Ok((i, o, sp)) => (i, o, sp),
                Err(_) => break,
            };
            span = span.start..op_span.end;

            let (_, r_bp) = match get_infix_binding_power(op.clone()) {
                bp if bp.0 >= min_bp => bp,
                _ => break,
            };

            let (rest, rhs, sp2) = parse_bin_op(r_bp).parse_next(rest)?;
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
            parse_expression,
            match_token(TokenKind::RightParen)))
}

pub fn parse_unary_op(input: TokenStream) -> Output<Expr> {
    pair(
        parse_unary_operator_from_token,
        parse_primary())
        .map(|(op, expr), span| Expr::UnaryOp(op, P(expr), span))
        .parse_next(input)
}

pub fn parse_unary_operator_from_token(input: TokenStream) -> Output<UnaryOperator> {
    match input.next() {
        Some((res, next_input)) => {
            let op = match res.kind() {
                TokenKind::Minus => UnaryOperator::Negative,
                TokenKind::Bang => UnaryOperator::Not,
                _ => return Err(
                    ErrorType::Recoverable(
                        ParserError::new()
                            .message(format!("expected unary op, found {:?}", res.kind()))
                            .span(res.span())
                            .build()))
            };
            Ok((next_input, op, res.span()))
        }
        None => Err(ErrorType::Incomplete),
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
            let mut iter = rest.into_iter();
            let mut base = MemberExpr {
                identifier: iter.next().unwrap(),
                base: MemberExprBase::Member(first.clone(), first.1),
            };

            for identifier in iter {
                let base2_span = match &base.base {
                                    MemberExprBase::Member(_, s) => s.clone(),
                                    MemberExprBase::MemberExpr(_, s) => s.clone(),
                                };
                
                base = MemberExpr {
                    identifier,
                    base: MemberExprBase::MemberExpr(P(base), base2_span),
                };
            }

            Expr::MemberExpr(base, span)
        })
}

pub fn parse_primary<'a>() -> impl Parser<'a, Expr> {
    or_n(vec![
        BoxedParser::new(parse_unary_op),
        BoxedParser::new(parse_parenthesized_expression()),
        BoxedParser::new(parse_set_literal()),
        BoxedParser::new(parse_set_array()),
        BoxedParser::new(parse_member_expr()),
        BoxedParser::new(parse_fn_call_expr),
        BoxedParser::new(parse_literal().map(|l, s| Expr::LitExpr(l, s))),
        BoxedParser::new(
            left(
                parse_identifier().map(|i, s| Expr::IdentExpr(i, s)),
                not(match_token(TokenKind::InferredEquals)))),
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
        .parse_next(input)
}

pub fn parse_fn_arg(input: TokenStream) -> Output<Expr> {
    or_n(vec![
        BoxedParser::new(parse_parenthesized_expression()),
        BoxedParser::new(parse_set_literal()),
        BoxedParser::new(parse_set_array()),
        BoxedParser::new(parse_literal().map(|l, s| Expr::LitExpr(l, s))),
        BoxedParser::new(parse_member_expr()),
        BoxedParser::new(parse_fn_arg_identifier())])
        .parse_next(input)
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
                BoxedParser::new(match_token(TokenKind::FatArrow)),
            ])
        ))
}

// *********** IF THEN ELSE *******************

pub fn parse_if_then_else(input: TokenStream) -> Output<Expr> {
    pair(
        right(
            match_token(TokenKind::IfKeyword),
            left(
                parse_expression,
                match_token(TokenKind::ThenKeyword))),
        pair(
            pair(
                parse_expression,
                zero_or_more(parse_elseif_p_then_e)),
            right(
                match_token(TokenKind::ElseKeyword),
                parse_expression)))
        .map(|(pred, ((expr, elseifs), else_expr)), span| {
            let mut final_result = vec![(pred, expr)];
            final_result.extend(elseifs);

            Expr::IfThenElseIfExpr(final_result, P(else_expr), span)
        })
        .parse_next(input)
}

pub fn parse_elseif_p_then_e(input: TokenStream) -> Output<(Expr, Expr)> {
    pair(
        right(
            match_token(TokenKind::ElseIfKeyword),
            left(
                parse_expression,
                match_token(TokenKind::ThenKeyword))),
        parse_expression)
        .parse_next(input)
}

pub fn parse_comma_seperated_expressions(input: TokenStream) -> Output<Vec<Expr>> {
    pair(
        parse_expression,
        zero_or_more(
            right(
                match_token(TokenKind::Comma),
                parse_expression)))
        .map(|(first_ident, idents), _| {
            let mut final_idents = vec![first_ident];
            final_idents.extend(idents);
            final_idents
        })
        .parse_next(input)
}

// **************** LET IN ******************
pub fn parse_let_in_expr(input: TokenStream) -> Output<Expr> {
    pair(
        surrounded(
            match_token(TokenKind::LetKeyword),
            zero_or_more(parse_statement),
            match_token(TokenKind::InKeyword)),
        parse_expression)
        .map(|(v, e), span| Expr::LetInExpr(v, P(e), span))
        .parse_next(input)
}

// ***************** CASE EXPRESSION *****************

pub fn parse_case_expr(input: TokenStream) -> Output<Expr> {
    right(
        match_token(TokenKind::CaseKeyword),
        pair(
            parse_identifier(),
            one_or_more(parse_fn_expr_case_branch)))
        .map(|(first, rest), span| {
            let result: Vec<(Vec<FnPatternParam>, Expr)> =
                rest.iter()
                    .map(|f| match f.clone() {
                        FnExpr::FnExpr(a, b, _) => (a, b),
                        _ => unreachable!()
                    }).collect();
            Expr::CaseExpr(first, result, span)
        })
        .parse_next(input)
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
        .parse_next(input)
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
        parse_expression)
        .parse_next(input)
}

pub fn parse_set_deconstruct<'a>() -> impl Parser<'a, Vec<Identifier>> {
    right(
        match_token(TokenKind::LeftBrace),
        left(
            parse_comma_seperated_identifiers,
            match_token(TokenKind::RightBrace)))
}

pub fn parse_fn_param_set_selector<'a>() -> impl Parser<'a, FnPatternParam> {
    pair(
        right(
            match_token(TokenKind::LeftBrace),
            parse_identifier()),
        right(
            match_token(TokenKind::Colon),
            left(
                parse_identifier(),
                match_token(TokenKind::RightBrace))))
        .map(|(first, rest), s| FnPatternParam::BindToSetSelectorParam(first, rest, s))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::prelude::{Token, TokenKind};
    use crate::parser::tokenstream::TokenStream;

    fn mock_token_stream(tokens: &[Token]) -> TokenStream {
        // Mock implementation to create a TokenStream from a vector of Token
        TokenStream::new(tokens)
    }

    fn create_token(kind: TokenKind, start: usize, len: usize) -> Token {
        Token::new(kind, start, len)
    }

    #[test]
    fn test_parse_expression() {
        let tokens = vec![
            create_token(TokenKind::LetKeyword, 0, 3),
            create_token(TokenKind::Identifier("x".into()), 4, 5),
            create_token(TokenKind::InferredEquals, 6, 7),
            create_token(TokenKind::Integer(42), 8, 10),
            create_token(TokenKind::InKeyword, 11, 2),
            create_token(TokenKind::Identifier("x".into()), 13, 5),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_expression(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_fn_expr_case_branch() {
        let tokens = vec![
            create_token(TokenKind::Union, 0, 1),
            create_token(TokenKind::Identifier("x".into()), 2, 1),
            create_token(TokenKind::FatArrow, 4, 2),
            create_token(TokenKind::Integer(42), 9, 2),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_fn_expr_case_branch(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_fn_expr_param() {
        let tokens = vec![create_token(TokenKind::Identifier("x".into()), 0, 1)];
        let input = mock_token_stream(&tokens);
        let result = parse_fn_expr_param().parse_next(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_fn_expr_case_params() {
        let tokens = vec![
            create_token(TokenKind::Identifier("x".into()), 0, 1),
            create_token(TokenKind::Comma, 2, 3),
            create_token(TokenKind::Identifier("y".into()), 4, 5),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_fn_expr_case_params().parse_next(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_fn_expr_case_param() {
        let tokens = vec![create_token(TokenKind::Identifier("x".into()), 0, 1)];
        let input = mock_token_stream(&tokens);
        let result = parse_fn_expr_case_param().parse_next(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_bin_op() {
        let tokens = vec![
            create_token(TokenKind::Integer(1), 0, 1),
            create_token(TokenKind::Plus, 2, 3),
            create_token(TokenKind::Integer(2), 4, 5),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_bin_op(0).parse_next(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_parenthesized_expression() {
        let tokens = vec![
            create_token(TokenKind::LeftParen, 0, 1),
            create_token(TokenKind::Integer(42), 2, 4),
            create_token(TokenKind::RightParen, 5, 6),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_parenthesized_expression().parse_next(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_unary_op() {
        let tokens = vec![
            create_token(TokenKind::Minus, 0, 1),
            create_token(TokenKind::Integer(42), 2, 4),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_unary_op(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_member_expr() {
        let tokens = vec![
            create_token(TokenKind::Identifier("x".into()), 0, 1),
            create_token(TokenKind::Dot, 2, 3),
            create_token(TokenKind::Identifier("y".into()), 4, 5),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_member_expr().parse_next(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_primary() {
        let tokens = vec![create_token(TokenKind::Integer(42), 0, 2)];
        let input = mock_token_stream(&tokens);
        let result = parse_primary().parse_next(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_fn_call_expr() {
        let tokens = vec![
            create_token(TokenKind::Identifier("func".into()), 0, 4),
            create_token(TokenKind::LeftParen, 5, 6),
            create_token(TokenKind::Integer(42), 7, 9),
            create_token(TokenKind::RightParen, 10, 11),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_fn_call_expr(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_fn_arg() {
        let tokens = vec![create_token(TokenKind::Integer(42), 0, 2)];
        let input = mock_token_stream(&tokens);
        let result = parse_fn_arg(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_fn_arg_identifier() {
        let tokens = vec![create_token(TokenKind::Identifier("x".into()), 0, 1)];
        let input = mock_token_stream(&tokens);
        let result = parse_fn_arg_identifier().parse_next(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_if_then_else() {
        let tokens = vec![
            create_token(TokenKind::IfKeyword, 0, 2),
            create_token(TokenKind::Bool(true), 3, 7),
            create_token(TokenKind::ThenKeyword, 8, 12),
            create_token(TokenKind::Integer(42), 13, 15),
            create_token(TokenKind::ElseKeyword, 16, 20),
            create_token(TokenKind::Integer(0), 21, 22),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_if_then_else(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_comma_seperated_expressions() {
        let tokens = vec![
            create_token(TokenKind::Integer(1), 0, 1),
            create_token(TokenKind::Comma, 2, 3),
            create_token(TokenKind::Integer(2), 4, 5),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_comma_seperated_expressions(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_let_in_expr() {
        let tokens = vec![
            create_token(TokenKind::LetKeyword, 0, 3),
            create_token(TokenKind::Identifier("x".into()), 4, 5),
            create_token(TokenKind::InferredEquals, 6, 7),
            create_token(TokenKind::Integer(42), 8, 10),
            create_token(TokenKind::InKeyword, 11, 13),
            create_token(TokenKind::Identifier("x".into()), 14, 15),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_let_in_expr(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_case_expr() {
        let tokens = vec![
            create_token(TokenKind::CaseKeyword, 0, 4),
            create_token(TokenKind::Identifier("x".into()), 5, 6),
            create_token(TokenKind::Union, 7, 8),
            create_token(TokenKind::Identifier("_".into()), 9, 10),
            create_token(TokenKind::FatArrow, 11, 13),
            create_token(TokenKind::Integer(42), 14, 16),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_case_expr(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_set_array() {
        let tokens = vec![
            create_token(TokenKind::LeftBrace, 0, 1),
            create_token(TokenKind::Integer(1), 2, 3),
            create_token(TokenKind::Comma, 4, 5),
            create_token(TokenKind::Integer(2), 6, 7),
            create_token(TokenKind::RightBrace, 8, 9),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_set_array().parse_next(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_empty_set() {
        let tokens = vec![
            create_token(TokenKind::LeftBrace, 0, 1),
            create_token(TokenKind::RightBrace, 2, 3),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_empty_set(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_set_literal() {
        let tokens = vec![
            create_token(TokenKind::LeftBrace, 0, 1),
            create_token(TokenKind::Identifier("x".into()), 2, 3),
            create_token(TokenKind::Equals, 4, 5),
            create_token(TokenKind::Integer(42), 6, 8),
            create_token(TokenKind::RightBrace, 9, 10),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_set_literal().parse_next(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_set_literal_comma_seperated_fields() {
        let tokens = vec![
            create_token(TokenKind::Identifier("x".into()), 0, 1),
            create_token(TokenKind::Equals, 2, 3),
            create_token(TokenKind::Integer(42), 4, 6),
            create_token(TokenKind::Comma, 7, 8),
            create_token(TokenKind::Identifier("y".into()), 9, 10),
            create_token(TokenKind::Equals, 11, 12),
            create_token(TokenKind::Integer(24), 13, 15),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_set_literal_comma_seperated_fields().parse_next(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_set_literal_field() {
        let tokens = vec![
            create_token(TokenKind::Identifier("x".into()), 0, 1),
            create_token(TokenKind::Equals, 2, 3),
            create_token(TokenKind::Integer(42), 4, 6),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_set_literal_field(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_set_deconstruct() {
        let tokens = vec![
            create_token(TokenKind::LeftBrace, 0, 1),
            create_token(TokenKind::Identifier("x".into()), 2, 3),
            create_token(TokenKind::Comma, 4, 5),
            create_token(TokenKind::Identifier("y".into()), 6, 7),
            create_token(TokenKind::RightBrace, 8, 9),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_set_deconstruct().parse_next(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_fn_param_set_selector() {
        let tokens = vec![
            create_token(TokenKind::LeftBrace, 0, 1),
            create_token(TokenKind::Identifier("x".into()), 2, 3),
            create_token(TokenKind::Colon, 4, 5),
            create_token(TokenKind::Identifier("y".into()), 6, 7),
            create_token(TokenKind::RightBrace, 8, 9),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_fn_param_set_selector().parse_next(input);
        assert!(result.is_ok());
    }
}