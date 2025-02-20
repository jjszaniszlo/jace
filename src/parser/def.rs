// *************** DEFINITIONS **************************

use crate::lexer::prelude::*;
use crate::parser::ast::*;
use crate::parser::combinator::*;
use crate::parser::error::*;
use crate::parser::expr::*;
use crate::parser::parser::*;
use crate::parser::ptr::*;
use crate::parser::stmt::*;
use crate::parser::tokenstream::{TokenResult, TokenStream};

pub fn parse_module<'a>(input: &mut TokenStream<'a>) -> Output<'a, Module> {
    zero_or_more(parse_definition)
        .map(|(defs), span| Module(defs, span))
        .parse_next(input)
}

pub fn parse_definition<'a>(input: &mut TokenStream<'a>) -> Output<'a, Def> {
    or_n(vec![
        BoxedParser::new(parse_instance_def),
        BoxedParser::new(parse_type_def),
        BoxedParser::new(parse_type_union_def),
        BoxedParser::new(parse_class_def),
        BoxedParser::new(parse_fn_def),
        BoxedParser::new(parse_proc_def),
        BoxedParser::new(parse_const_def),
    ])
        .parse_next(input)
}

pub fn parse_fn_def<'a>(input: &mut TokenStream<'a>) -> Output<'a, Def> {
    pair(
        pair(
            parse_fn_def_header,
            parse_fn_def_types),
        pair(
            zero_or_one(parse_fn_type_constraints),
            parse_fn_expr,
        ))
        .map(|((i, (tp, rt)), (cons, exp)), span| Def::FnDef(i, tp, rt, cons, exp, span))
        .parse_next(input)
}

pub fn parse_fn_def_header<'a>(input: &mut TokenStream<'a>) -> Output<'a, Identifier> {
    left(
        right(
            match_token(TokenKind::DefKeyword),
            parse_identifier()),
        match_token(TokenKind::ColonColon))
        .parse_next(input)
}

pub fn parse_fn_def_types<'a>(input: &mut TokenStream<'a>) -> Output<'a, (Vec<TypeParam>, TypeParam)> {
    pair(
        parse_fn_type_params,
        right(
            match_token(TokenKind::FatArrow),
            or_n(vec![
                BoxedParser::new(parse_fn_type_param),
                pair(
                    match_token(TokenKind::LeftParen),
                    match_token(TokenKind::RightParen))
                    .map(|(_, _), span| TypeParam::Empty(span)),
                BoxedParser::new(surrounded(
                    match_token(TokenKind::LeftParen),
                    parse_fn_type_params.map(|(params), span| TypeParam::Tuple(params, span)),
                    match_token(TokenKind::RightParen))),
            ])))
        .map(|(type_params, return_type), _| (type_params, return_type))
        .parse_next(input)
}

pub fn parse_fn_type_params<'a>(input: &mut TokenStream<'a>) -> Output<'a, Vec<TypeParam>> {
    pair(
        parse_fn_type_param,
        zero_or_more(
            right(
                match_token(TokenKind::Comma),
                parse_fn_type_param)))
        .map(|(first_type, types), _| {
            let mut final_types = vec![first_type];
            final_types.extend(types);
            final_types
        })
            .parse_next(input)
}

pub fn parse_fn_type_param<'a>(input: &mut TokenStream<'a>) -> Output<'a, TypeParam> {
    or_n(vec![
        parse_identifier().map(|i, span| TypeParam::Type(i, span)),
        BoxedParser::new(parse_array_type_param),
        BoxedParser::new(parse_fn_func_type_param),
        BoxedParser::new(parse_type_union_payload_type)
    ])
        .parse_next(input)
}

pub fn parse_array_type_param<'a>(input: &mut TokenStream<'a>) -> Output<'a, TypeParam> {
    right(
        match_token(TokenKind::LeftBracket),
        left(
            pair(
                parse_identifier(),
                zero_or_one(parse_literal_integer())),
            match_token(TokenKind::RightBracket)))
        .map(|(i, l), span| TypeParam::ArrayType(i, l, span))
        .parse_next(input)
}

pub fn parse_fn_func_type_param<'a>(input: &mut TokenStream<'a>) -> Output<'a, TypeParam> {
    pair(
        right(
            match_token(TokenKind::LeftParen),
            parse_fn_type_params),
        right(
            match_token(TokenKind::FatArrow),
            left(
                parse_fn_type_param,
                match_token(TokenKind::RightParen))))
        .map(|(params, ret), span| TypeParam::FuncType(params, P(ret), span))
        .parse_next(input)
}

pub fn parse_type_union_payload_type<'a>(input: &mut TokenStream<'a>) -> Output<'a, TypeParam> {
    surrounded(
        match_token(TokenKind::LeftParen),
        pair(
            parse_identifier(),
            one_or_more(parse_fn_type_param)),
        match_token(TokenKind::RightParen),
    )
        .map(|(i, params), span| TypeParam::PayloadType(i, params, span))
        .parse_next(input)
}

pub fn parse_fn_type_constraints<'a>(input: &mut TokenStream<'a>) -> Output<'a, Vec<TypeConstraint>> {
    left(
        right(
            match_token(TokenKind::WhereKeyword),
            zero_or_more(
                parse_fn_type_constraint)),
        match_token(TokenKind::InKeyword))
        .parse_next(input)
}

pub fn parse_fn_type_constraint<'a>(input: &mut TokenStream<'a>) -> Output<'a, TypeConstraint> {
    pair(
        left(
            parse_identifier(),
            match_token(TokenKind::Colon)),
        pair(
            parse_identifier(),
            zero_or_more(
                right(
                    match_token(TokenKind::Plus),
                    parse_identifier()))))
        .map(|(type_name, (first, rest)), span| {
            let mut out = vec![first];
            out.extend(rest);
            TypeConstraint(type_name, out, span)
        })
            .parse_next(input)
}

pub fn parse_proc_def<'a>(input: &mut TokenStream) -> Output<'a, Def> {
    pair(
        left(
            parse_fn_def_header,
            pair(
                match_token(TokenKind::LeftParen),
                match_token(TokenKind::RightParen))),
        zero_or_more(parse_statement()))
        .map(|(ident, stmts), span| Def::ProcDef(ident, stmts, span))
        .parse_next(input)
}

pub fn parse_const_def<'a>(input: &mut TokenStream<'a>) -> Output<'a, Def> {
    pair(
        right(
            match_token(TokenKind::ConstKeyword),
            parse_identifier()),
        right(
            match_token(TokenKind::ColonColon),
            parse_literal()))
        .map(|(i, l), span| Def::ConstDef(i, l, span))
        .parse_next(input)
}

pub fn parse_type_def<'a>(input: &mut TokenStream<'a>) -> Output<'a, Def> {
    pair(
        right(
            match_token(TokenKind::TypeKeyword),
            left(
                parse_identifier(),
                match_token(TokenKind::ColonColon))),
        one_or_more(
            pair(
                parse_identifier(),
                right(
                    match_token(TokenKind::Colon),
                    parse_fn_type_param))))
        .map(|(i, v), span| Def::TypeDef(i, v, span))
        .parse_next(input)
}

pub fn parse_type_union_def<'a>(input: &mut TokenStream<'a>) -> Output<'a, Def> {
    pair(
        right(
            match_token(TokenKind::TypeKeyword),
            left(
                pair(
                    parse_identifier(),
                    zero_or_more(parse_identifier())),
                match_token(TokenKind::ColonColon))),
        parse_type_union)
        .map(|((type_name, poly_types), members), span| Def::TypeUnion(type_name, poly_types, members, span))
        .parse_next(input)
}

pub fn parse_type_union<'a>(input: &mut TokenStream<'a>) -> Output<'a, Vec<TypeParam>> {
    pair(
        parse_fn_type_param,
        zero_or_more(
            right(
                match_token(TokenKind::Union),
                parse_fn_type_param)))
        .map(|(first, rest), _| {
            let mut types = vec![first];
            types.extend(rest);
            types
        })
            .parse_next(input)
}

pub fn parse_type_union_member<'a>(input: &mut TokenStream<'a>) -> Output<'a, (Identifier, Option<Identifier>)> {
    pair(
        parse_identifier(),
        zero_or_one(
            surrounded(
                match_token(TokenKind::LeftParen),
                parse_identifier(),
                match_token(TokenKind::RightParen))))
        .parse_next(input)
}

pub fn parse_class_def<'a>(input: &mut TokenStream<'a>) -> Output<'a, Def> {
    pair(
        right(
            match_token(TokenKind::ClassKeyword),
            pair(
                parse_identifier(),
                left(
                    parse_class_generic_types,
                    match_token(TokenKind::ColonColon)))),
        parse_class_method_defs)
        .map(|((class_name, params), methods), span|
            Def::ClassDef(class_name, params, methods, span))
            .parse_next(input)
}

pub fn parse_class_generic_types<'a>(input: &mut TokenStream<'a>) -> Output<'a, Vec<Identifier>> {
    one_or_more(parse_identifier()).parse_next(input)
}

pub fn parse_class_method_defs<'a>(input: &mut TokenStream<'a>) -> Output<'a, Vec<MethodDef>> {
    one_or_more(parse_class_method_def).parse_next(input)
}

pub fn parse_class_method_def<'a>(input: &mut TokenStream<'a>) -> Output<'a, MethodDef> {
    or(
        parse_class_method_def_operator,
        parse_class_method_def_named)
        .parse_next(input)
}

pub fn parse_class_method_def_operator<'a>(input: &mut TokenStream<'a>) -> Output<'a, MethodDef> {
    pair(
        left(
            parse_method_operator,
            match_token(TokenKind::ColonColon)),
        pair(
            parse_fn_type_params,
            right(
                match_token(TokenKind::FatArrow),
                or(
                    parse_fn_type_param,
                    surrounded(
                        match_token(TokenKind::LeftParen),
                        parse_fn_type_params.map(|params, span| TypeParam::Tuple(params, span)),
                        match_token(TokenKind::RightParen))))))
        .map(|(op, (params, ret)), span|
            MethodDef::Operator(op, params, ret, span))
            .parse_next(input)
}

pub fn parse_class_method_type_params<'a>(input: &mut TokenStream<'a>) -> Output<'a, Vec<Identifier>> {
    pair(
        parse_identifier(),
        zero_or_more(
            right(
                match_token(TokenKind::Comma),
                parse_identifier())))
        .map(|(first_type, types), _| {
            let mut final_type_params = vec![first_type];

            final_type_params.extend(types);

            final_type_params
        })
            .parse_next(input)
}

pub fn parse_method_operator<'a>(input: &mut TokenStream<'a>) -> Output<'a, MethodOperator> {
    let start = input.checkpoint();
    let final_res = match input.next() {
        Some(res) => {
            let op = match res.kind() {
                TokenKind::WrappedEqualsEquals => Ok(MethodOperator::EqualsEquals),
                TokenKind::WrappedNotEquals => Ok(MethodOperator::NotEquals),
                TokenKind::WrappedLessEquals => Ok(MethodOperator::LessEquals),
                TokenKind::WrappedGreaterEquals => Ok(MethodOperator::GreaterEquals),
                TokenKind::WrappedGreater => Ok(MethodOperator::Greater),
                TokenKind::WrappedLess => Ok(MethodOperator::Less),
                TokenKind::WrappedPlus => Ok(MethodOperator::Plus),
                TokenKind::WrappedMinus => Ok(MethodOperator::Minus),
                TokenKind::WrappedDivide => Ok(MethodOperator::Divide),
                TokenKind::WrappedMultiply => Ok(MethodOperator::Multiply),
                _ => Err(
                    ErrorType::Recoverable(
                        ParserError::new()
                        .message(format!("expected method operator, got {:?}", res.kind()))
                        .span(res.span())
                        .build())
                ),
            };
            match op {
                Ok(op) => Ok((op, res.span())),
                Err(err) => Err(err),
            }
        },
        None => Err(ErrorType::Incomplete)
    };
    
    match final_res {
        ok @ Ok(_) => ok,
        Err(e) => {
            input.restore_checkpoint(start);
            Err(e)
        },
    }
}

pub fn parse_class_method_def_named<'a>(input: &mut TokenStream<'a>) -> Output<'a, MethodDef> {
    pair(
        left(
            parse_identifier(),
            match_token(TokenKind::ColonColon)),
        pair(
            parse_fn_type_params,
            right(
                match_token(TokenKind::FatArrow),
                parse_fn_type_param)))
        .map(|(ident, (params, ret)), span|
            MethodDef::Named(ident, params, ret, span))
            .parse_next(input)
}

pub fn parse_instance_def<'a>(input: &mut TokenStream<'a>) -> Output<'a, Def> {
    pair(
        left(
            parse_instance_header,
            match_token(TokenKind::ColonColon)),
        parse_instance_method_impls)
        .map(|((cls, typ), impls), span|
            Def::InstanceDef(cls, typ, impls, span))
        .parse_next(input)
}

pub fn parse_instance_header<'a>(input: &mut TokenStream<'a>) -> Output<'a, (Identifier, Identifier)> {
    right(
        match_token(TokenKind::InstanceKeyword),
        pair(
            parse_identifier(),
            parse_identifier()))
        .map(|(class, ty), _| (class, ty))
        .parse_next(input)
}

pub fn parse_instance_method_impls<'a>(input: &mut TokenStream<'a>) -> Output<'a, Vec<MethodImpl>> {
    one_or_more(parse_instance_method_impl).parse_next(input)
}

pub fn parse_instance_method_impl<'a>(input: &mut TokenStream<'a>) -> Output<'a, MethodImpl> {
    or(
        parse_instance_method_impl_op,
        parse_instance_method_impl_named)
        .parse_next(input)
}

pub fn parse_instance_method_impl_op<'a>(input: &mut TokenStream<'a>) -> Output<'a, MethodImpl> {
    pair(
        left(
            parse_method_operator,
            match_token(TokenKind::ColonColon)),
        parse_fn_expr)
        .map(|(op, expr), span| MethodImpl::Operator(op, expr, span))
        .parse_next(input)
}

pub fn parse_instance_method_impl_named<'a>(input: &mut TokenStream<'a>) -> Output<'a, MethodImpl> {
    pair(
        left(
            parse_identifier(),
            match_token(TokenKind::ColonColon)),
        parse_fn_expr)
        .map(|(name, expr), span| MethodImpl::Named(name, expr, span))
        .parse_next(input)
}
