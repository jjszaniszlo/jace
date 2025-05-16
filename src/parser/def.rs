// *************** DEFINITIONS **************************

use crate::lexer::prelude::*;
use crate::parser::ast::FnPatternParam::{BindToIdentParam, BindToSetSelectorParam, BindToTypeConstructorParam};
use crate::parser::ast::*;
use crate::parser::combinator::*;
use crate::parser::expr::*;
use crate::parser::parser::*;
use crate::parser::prelude::FnPatternParam::{BindToLiteralParam, BindToSetDeconstructParam};
use crate::parser::stmt::*;
use crate::parser::tokenstream::TokenStream;
use crate::parser::typeparam::parse_type_param;

pub fn parse_module<'a>() -> impl Parser<'a, Module> {
    zero_or_more(parse_def())
        .map(|(defs), span| Module(defs, span))
}

pub fn parse_def<'a>() -> impl Parser<'a, Def> {
    or_n(vec![
        BoxedParser::new(parse_fn_def),
        BoxedParser::new(parse_proc_def),
        BoxedParser::new(parse_type_def),
        // BoxedParser::new(parse_instance_def),
        // BoxedParser::new(parse_type_def()),
        // BoxedParser::new(parse_type_union_def()),
        // BoxedParser::new(parse_class_def()),
        // BoxedParser::new(parse_proc_def()),
        // BoxedParser::new(parse_const_def()),
    ])
}

pub fn parse_def_header(input: TokenStream) -> Output<Identifier> {
    surrounded(
        match_token(TokenKind::DefKeyword),
        parse_identifier(),
        match_token(TokenKind::ColonColon),
    )
        .parse_next(input)
}

pub fn parse_fn_def(input: TokenStream) -> Output<Def> {
    pair(
        pair(
            parse_def_header,
            parse_fn_type_sig),
        pair(
            parse_fn_type_constraints,
            parse_fn_body))
        .map(|((ident, (in_types, ret_type)), (constraints, expr)), s|
            Def::FnDef(ident, in_types, ret_type, constraints, expr, s))
        .parse_next(input)
}

pub fn parse_fn_type_sig(input: TokenStream) -> Output<(Vec<TypeParam>, TypeParam)> {
    pair(
        pair(
            parse_type_param,
            zero_or_more(
                right(
                    match_token(TokenKind::Comma),
                    parse_type_param))),
        right(
            match_token(TokenKind::FatArrow),
            parse_type_param))
        .map(|((first, rest), ret), _| {
            let mut v = vec![first];
            v.extend(rest);
            (v, ret)
        })
        .parse_next(input)
}

pub fn parse_fn_type_constraints(input: TokenStream) -> Output<Option<Vec<TypeConstraint>>> {
    zero_or_one(
        surrounded(
            match_token(TokenKind::WhereKeyword),
            zero_or_more(parse_fn_type_constraint),
            match_token(TokenKind::InKeyword),
        ))
        .parse_next(input)
}

pub fn parse_fn_type_constraint(input: TokenStream) -> Output<TypeConstraint> {
    pair(
        parse_identifier(),
        right(
            match_token(TokenKind::Colon),
            pair(
                parse_identifier(),
                zero_or_more(
                    right(
                        match_token(TokenKind::Plus),
                        parse_identifier())))))
        .map(|(ident, (first, rest)), s| {
            let mut final_vec = vec![first];
            final_vec.extend(rest);
            TypeConstraint(ident, final_vec, s)
        })
        .parse_next(input)
}

pub fn parse_fn_body(input: TokenStream) -> Output<FnExpr> {
    or(
        right(
            match_token(TokenKind::DoKeyword),
            parse_fn_body_single),
        parse_fn_body_case,
    )
        .parse_next(input)
}

pub fn parse_fn_body_single(input: TokenStream) -> Output<FnExpr> {
    pair(
        parse_fn_body_params,
        right(
            match_token(TokenKind::FatArrow),
            parse_expression))
        .map(|(params, expr), s| FnExpr::FnExpr(params, expr, s))
        .parse_next(input)
}

pub fn parse_fn_body_case(input: TokenStream) -> Output<FnExpr> {
    right(
        match_token(TokenKind::CaseKeyword),
        one_or_more(
            right(
                match_token(TokenKind::Union),
                parse_fn_body_single)))
        .map(|exprs, s| FnExpr::CaseFnExpr(exprs, s))
        .parse_next(input)
}

pub fn parse_fn_body_params(input: TokenStream) -> Output<Vec<FnPatternParam>> {
    pair(
        parse_fn_pattern_param,
        zero_or_more(
            right(
                match_token(TokenKind::Comma),
                parse_fn_pattern_param)))
        .map(|(first, rest), _| {
            let mut final_vec = vec![first];
            final_vec.extend(rest);
            final_vec
        })
        .parse_next(input)
}

pub fn parse_fn_pattern_param(input: TokenStream) -> Output<FnPatternParam> {
    or_n(vec![
        BoxedParser::new(parse_fn_pattern_param_type_constructor),
        BoxedParser::new(parse_fn_pattern_param_bind_literal),
        BoxedParser::new(parse_fn_pattern_param_bind_ident),
        BoxedParser::new(parse_fn_pattern_param_bind_set_deconstruct),
        BoxedParser::new(parse_fn_pattern_param_set_selector),
    ])
        .parse_next(input)
}

pub fn parse_fn_pattern_param_bind_literal(input: TokenStream) -> Output<FnPatternParam> {
    parse_literal()
        .map(|l, s| BindToLiteralParam(l, s))
        .parse_next(input)
}

pub fn parse_fn_pattern_param_bind_ident(input: TokenStream) -> Output<FnPatternParam> {
    parse_identifier()
        .map(|i, s| BindToIdentParam(i, s))
        .parse_next(input)
}

pub fn parse_fn_pattern_param_bind_set_deconstruct(input: TokenStream) -> Output<FnPatternParam> {
    surrounded(
        match_token(TokenKind::LeftBrace),
        zero_or_more(parse_identifier()),
        match_token(TokenKind::RightBrace))
        .map(|is, s| BindToSetDeconstructParam(is, s))
        .parse_next(input)
}

pub fn parse_fn_pattern_param_set_selector(input: TokenStream) -> Output<FnPatternParam> {
    surrounded(
        match_token(TokenKind::LeftBrace),
        pair(
            parse_identifier(),
            right(
                match_token(TokenKind::Colon),
                parse_identifier())),
        match_token(TokenKind::RightBrace))
        .map(|(first, rest), s| BindToSetSelectorParam(first, rest, s))
        .parse_next(input)
}

pub fn parse_fn_pattern_param_type_constructor(input: TokenStream) -> Output<FnPatternParam> {
    or(
        surrounded(
            match_token(TokenKind::LeftParen),
            pair(
                parse_identifier(),
                one_or_more(parse_fn_pattern_param_type_constructor_lit_or_ident)),
            match_token(TokenKind::RightParen)),
        pair(
            parse_identifier(),
            one_or_more(parse_fn_pattern_param_type_constructor_lit_or_ident)))
        .map(|(ident, cons_params), s|
            BindToTypeConstructorParam(ident, cons_params, s))
        .parse_next(input)
}
fn parse_fn_pattern_param_type_constructor_lit_or_ident(input: TokenStream) -> Output<FnPatternParam> {
    or(
        parse_identifier().map(|i, s| BindToIdentParam(i, s)),
        parse_literal().map(|l, s| BindToLiteralParam(l, s)))
        .parse_next(input)
}

pub fn parse_proc_def(input: TokenStream) -> Output<Def> {
    pair(
        left(
            parse_def_header,
            pair(
                match_token(TokenKind::LeftParen),
                match_token(TokenKind::RightParen))),
        zero_or_more(
            parse_statement))
        .map(|(i, stmts), s| Def::ProcDef(i, stmts, s))
        .parse_next(input)
}

pub fn parse_type_def(input: TokenStream) -> Output<Def> {
    pair(
        parse_type_def_header,
        pair(
            parse_type_def_member,
            zero_or_more(
                right(
                    match_token(TokenKind::Union),
                    parse_type_def_member))))
        .map(|((ty_name, para_names), (f_ty, rest_ty)), s| {
            let mut final_types = vec![f_ty];
            final_types.extend(rest_ty);
            Def::TypeDef(ty_name, para_names, final_types, s)
        })
        .parse_next(input)
}

pub fn parse_type_def_member(input: TokenStream) -> Output<TypeParam> {
    parse_type_param
        .map(|t, s |
            match t {
                TypeParam::Type(i, s) => TypeParam::TypeConstructorType(i, vec![], s),
                _ => t,
            })
        .parse_next(input)

}

pub fn parse_type_def_header(input: TokenStream) -> Output<(Identifier, Vec<Identifier>)> {
    surrounded(
        match_token(TokenKind::TypeKeyword),
        pair(
            parse_identifier(),
            zero_or_more(parse_identifier())),
        match_token(TokenKind::ColonColon))
        .parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::prelude::{Token, TokenKind};
    use crate::parser::tokenstream::TokenStream;

    fn mock_token_stream(tokens: &[Token]) -> TokenStream {
        TokenStream::new(tokens)
    }

    fn create_token(kind: TokenKind, start: usize, length: usize) -> Token {
        Token::new(kind, start, length)
    }

    #[test]
    fn test_parse_module() {
        let tokens = vec![
            create_token(TokenKind::DefKeyword, 0, 3),
            create_token(TokenKind::Identifier("main".into()), 4, 4),
            create_token(TokenKind::ColonColon, 8, 2),
            create_token(TokenKind::LeftParen, 10, 1),
            create_token(TokenKind::RightParen, 11, 1),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_module().parse_next(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_def() {
        let tokens = vec![
            create_token(TokenKind::DefKeyword, 0, 3),
            create_token(TokenKind::Identifier("main".into()), 4, 4),
            create_token(TokenKind::ColonColon, 8, 2),
            create_token(TokenKind::LeftParen, 10, 1),
            create_token(TokenKind::RightParen, 11, 1),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_def().parse_next(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_def_header() {
        let tokens = vec![
            create_token(TokenKind::DefKeyword, 0, 3),
            create_token(TokenKind::Identifier("main".into()), 4, 4),
            create_token(TokenKind::ColonColon, 8, 2),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_def_header(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_fn_def() {
        let tokens = vec![
            create_token(TokenKind::DefKeyword, 0, 3),
            create_token(TokenKind::Identifier("sum".into()), 4, 3),
            create_token(TokenKind::ColonColon, 7, 2),
            create_token(TokenKind::Identifier("Integer".into()), 9, 7),
            create_token(TokenKind::Comma, 16, 1),
            create_token(TokenKind::Identifier("Integer".into()), 17, 7),
            create_token(TokenKind::FatArrow, 24, 2),
            create_token(TokenKind::Identifier("Integer".into()), 26, 7),
            create_token(TokenKind::DoKeyword, 33, 2),
            create_token(TokenKind::Identifier("a".into()), 35, 1),
            create_token(TokenKind::Comma, 36, 1),
            create_token(TokenKind::Identifier("b".into()), 37, 1),
            create_token(TokenKind::FatArrow, 38, 2),
            create_token(TokenKind::Identifier("a".into()), 40, 1),
            create_token(TokenKind::Multiply, 41, 1),
            create_token(TokenKind::Identifier("b".into()), 42, 1),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_fn_def(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_fn_type_sig() {
        let tokens = vec![
            create_token(TokenKind::Identifier("x".into()), 0, 1),
            create_token(TokenKind::Comma, 1, 1),
            create_token(TokenKind::Identifier("y".into()), 2, 1),
            create_token(TokenKind::FatArrow, 3, 2),
            create_token(TokenKind::Identifier("z".into()), 5, 1),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_fn_type_sig(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_fn_type_constraints() {
        let tokens = vec![
            create_token(TokenKind::WhereKeyword, 0, 5),
            create_token(TokenKind::Identifier("T".into()), 5, 1),
            create_token(TokenKind::Colon, 6, 1),
            create_token(TokenKind::Identifier("Clone".into()), 7, 5),
            create_token(TokenKind::InKeyword, 12, 2),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_fn_type_constraints(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_fn_type_constraint() {
        let tokens = vec![
            create_token(TokenKind::Identifier("T".into()), 0, 1),
            create_token(TokenKind::Colon, 1, 1),
            create_token(TokenKind::Identifier("Clone".into()), 2, 5),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_fn_type_constraint(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_fn_body() {
        let tokens = vec![
            create_token(TokenKind::DoKeyword, 0, 2),
            create_token(TokenKind::Identifier("x".into()), 2, 1),
            create_token(TokenKind::FatArrow, 3, 2),
            create_token(TokenKind::Integer(42), 5, 2),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_fn_body(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_fn_body_single() {
        let tokens = vec![
            create_token(TokenKind::Identifier("x".into()), 0, 1),
            create_token(TokenKind::FatArrow, 1, 2),
            create_token(TokenKind::Integer(42), 3, 2),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_fn_body_single(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_fn_body_case() {
        let tokens = vec![
            create_token(TokenKind::CaseKeyword, 0, 4),
            create_token(TokenKind::Union, 1, 1),
            create_token(TokenKind::Identifier("x".into()), 3, 1),
            create_token(TokenKind::FatArrow, 4, 2),
            create_token(TokenKind::Integer(42), 6, 2),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_fn_body_case(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_fn_body_params() {
        let tokens = vec![
            create_token(TokenKind::Identifier("x".into()), 0, 1),
            create_token(TokenKind::Comma, 1, 1),
            create_token(TokenKind::Identifier("y".into()), 2, 1),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_fn_body_params(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_fn_pattern_param() {
        let tokens = vec![create_token(TokenKind::Identifier("x".into()), 0, 1)];
        let input = mock_token_stream(&tokens);
        let result = parse_fn_pattern_param(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_proc_def() {
        let tokens = vec![
            create_token(TokenKind::DefKeyword, 0, 3),
            create_token(TokenKind::Identifier("proc".into()), 4, 4),
            create_token(TokenKind::ColonColon, 8, 2),
            create_token(TokenKind::LeftParen, 10, 1),
            create_token(TokenKind::RightParen, 11, 1),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_proc_def(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_type_def() {
        let tokens = vec![
            create_token(TokenKind::TypeKeyword, 0, 4),
            create_token(TokenKind::Identifier("Option".into()), 5, 6),
            create_token(TokenKind::ColonColon, 11, 2),
            create_token(TokenKind::Identifier("Some".into()), 13, 4),
            create_token(TokenKind::Union, 17, 1),
            create_token(TokenKind::Identifier("None".into()), 18, 4),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_type_def(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_type_def_member() {
        let tokens = vec![create_token(TokenKind::Identifier("Some".into()), 0, 4)];
        let input = mock_token_stream(&tokens);
        let result = parse_type_def_member(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_type_def_header() {
        let tokens = vec![
            create_token(TokenKind::TypeKeyword, 0, 4),
            create_token(TokenKind::Identifier("Option".into()), 5, 6),
            create_token(TokenKind::ColonColon, 11, 2),
        ];
        let input = mock_token_stream(&tokens);
        let result = parse_type_def_header(input);
        assert!(result.is_ok());
    }
}