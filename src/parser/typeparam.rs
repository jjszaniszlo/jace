use crate::lexer::token::TokenKind;
use crate::parser::ast::TypeParam;
use crate::parser::combinator::{one_or_more, or, or_n, pair, right, surrounded, zero_or_one};
use crate::parser::parser::{parse_identifier, parse_literal_integer, BoxedParser, Output, Parser};
use crate::parser::prelude::match_token;
use crate::parser::ptr::P;
use crate::parser::tokenstream::TokenStream;

pub fn parse_type_param(input: TokenStream) -> Output<TypeParam> {
    or_n(vec![
        BoxedParser::new(parse_type_param_empty),
        BoxedParser::new(parse_type_param_tuple),
        BoxedParser::new(parse_type_param_array),
        BoxedParser::new(parse_type_param_func),
        BoxedParser::new(parse_product_type_param),
        BoxedParser::new(parse_type_param_ident),
    ])
        .parse_next(input)
}

pub fn parse_type_param_empty(input: TokenStream) -> Output<TypeParam> {
    pair(
        match_token(TokenKind::LeftParen),
        match_token(TokenKind::RightParen))
        .map(|(_, _), s| TypeParam::Empty(s))
        .parse_next(input)
}

pub fn parse_type_param_tuple(input: TokenStream) -> Output<TypeParam> {
    surrounded(
        match_token(TokenKind::LeftParen),
        pair(
            parse_type_param,
            one_or_more(
                right(
                    match_token(TokenKind::Comma),
                    parse_type_param))),
        match_token(TokenKind::RightParen))
        .map(|(p, prest), s| {
            let mut out = vec![p];
            out.extend(prest);
            TypeParam::TupleType(out, s)
        })
        .parse_next(input)
}

pub fn parse_type_param_array(input: TokenStream) -> Output<TypeParam> {
    surrounded(
        match_token(TokenKind::LeftBracket),
        pair(
            parse_type_param,
            zero_or_one(
                parse_literal_integer())),
        match_token(TokenKind::RightBracket))
        .map(|(ty, size), s| TypeParam::ArrayType(P(ty), size, s))
        .parse_next(input)
}

pub fn parse_type_param_func(input: TokenStream) -> Output<TypeParam> {
    surrounded(
        match_token(TokenKind::LeftParen),
        pair(
            pair(
                parse_type_param,
                zero_or_one(
                    right(
                        match_token(TokenKind::Comma),
                        parse_type_param))),
            right(
                match_token(TokenKind::FatArrow),
                parse_type_param)),
        match_token(TokenKind::RightParen))
        .map(|((p, prest), ret), s| {
            let mut v = vec![p];
            v.extend(prest);
            TypeParam::FuncType(v, P(ret), s)
        })
        .parse_next(input)
}

pub fn parse_type_param_ident(input: TokenStream) -> Output<TypeParam> {
    parse_identifier()
        .map(|i, s| TypeParam::Type(i, s))
        .parse_next(input)
}

pub fn parse_product_type_param(input: TokenStream) -> Output<TypeParam> {
    or(
        surrounded(
            match_token(TokenKind::LeftParen),
            pair(
                parse_identifier(),
                one_or_more(parse_type_param)),
            match_token(TokenKind::RightParen),
        ),
        pair(
            parse_identifier(),
            one_or_more(parse_type_param)))
        .map(|(i, p), s| TypeParam::ProductType(i, p, s))
        .parse_next(input)
}