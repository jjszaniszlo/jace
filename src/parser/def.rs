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
            left(
                parse_fn_body_single,
                match_token(TokenKind::SemiColon))))
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

// pub fn parse_fn_def(input: TokenStream) -> Output<Def> {
//     pair(
//         pair(
//             parse_fn_def_header,
//             parse_fn_def_types()),
//         pair(
//             zero_or_one(parse_fn_type_constraints),
//             parse_fn_expr,
//         ))
//         .map(|((i, (tp, rt)), (cons, exp)), span| Def::FnDef(i, tp, rt, cons, exp, span))
//         .parse_next(input)
// }
//
// pub fn parse_fn_def_header(input: TokenStream) -> Output<Identifier> {
//     left(
//         right(
//             match_token(TokenKind::DefKeyword),
//             parse_identifier()),
//         match_token(TokenKind::ColonColon))
//         .parse_next(input)
// }
//
// pub fn parse_fn_def_types<'a>() -> impl Parser<'a, (Vec<TypeParam>, TypeParam)> {
//     pair(
//         parse_fn_type_params(),
//         right(
//             match_token(TokenKind::FatArrow),
//             or_n(vec![
//                 BoxedParser::new(parse_fn_type_param),
//                 pair(
//                     match_token(TokenKind::LeftParen),
//                     match_token(TokenKind::RightParen))
//                     .map(|(_, _), span| TypeParam::Empty(span)),
//                 BoxedParser::new(surrounded(
//                     match_token(TokenKind::LeftParen),
//                     parse_fn_type_params().map(|(params), span| TypeParam::TupleType(params, span)),
//                     match_token(TokenKind::RightParen))),
//             ])))
//         .map(|(type_params, return_type), _| (type_params, return_type))
// }
//
// pub fn parse_fn_type_params<'a>() -> impl Parser<'a, Vec<TypeParam>> {
//     pair(
//         parse_fn_type_param,
//         zero_or_more(
//             right(
//                 match_token(TokenKind::Comma),
//                 parse_fn_type_param)))
//         .map(|(first_type, types), _| {
//             let mut final_types = vec![first_type];
//             final_types.extend(types);
//             final_types
//         })
// }
//
// pub fn parse_fn_type_param(input: TokenStream) -> Output<TypeParam> {
//     or_n(vec![
//         parse_identifier().map(|i, span| TypeParam::Type(i, span)),
//         BoxedParser::new(parse_array_type_param),
//         BoxedParser::new(parse_fn_func_type_param),
//         BoxedParser::new(parse_type_union_payload_type)
//     ])
//         .parse_next(input)
// }
//
// pub fn parse_array_type_param(input: TokenStream) -> Output<TypeParam> {
//     right(
//         match_token(TokenKind::LeftBracket),
//         left(
//             pair(
//                 parse_fn_type_param,
//                 zero_or_one(parse_literal_integer())),
//             match_token(TokenKind::RightBracket)))
//         .map(|(i, l), span| TypeParam::ArrayType(P(i), l, span))
//         .parse_next(input)
// }
//
// pub fn parse_fn_func_type_param(input: TokenStream) -> Output<TypeParam> {
//     pair(
//         right(
//             match_token(TokenKind::LeftParen),
//             parse_fn_type_params()),
//         right(
//             match_token(TokenKind::FatArrow),
//             left(
//                 parse_fn_type_param,
//                 match_token(TokenKind::RightParen))))
//         .map(|(params, ret), span| TypeParam::FuncType(params, P(ret), span))
//         .parse_next(input)
// }
//
// pub fn parse_type_union_payload_type(input: TokenStream) -> Output<TypeParam> {
//     surrounded(
//         match_token(TokenKind::LeftParen),
//         pair(
//             parse_identifier(),
//             one_or_more(parse_fn_type_param)),
//         match_token(TokenKind::RightParen),
//     )
//         .map(|(i, params), span| TypeParam::TaggedUnionType(i, params, span))
//         .parse_next(input)
// }
//
// pub fn parse_fn_type_constraints(input: TokenStream) -> Output<Vec<TypeConstraint>> {
//     left(
//         right(
//             match_token(TokenKind::WhereKeyword),
//             zero_or_more(
//                 parse_fn_type_constraint())),
//         match_token(TokenKind::InKeyword))
//         .parse_next(input)
// }
//
// pub fn parse_fn_type_constraint<'a>() -> impl Parser<'a, TypeConstraint> {
//     pair(
//         left(
//             parse_identifier(),
//             match_token(TokenKind::Colon)),
//         pair(
//             parse_identifier(),
//             zero_or_more(
//                 right(
//                     match_token(TokenKind::Plus),
//                     parse_identifier()))))
//         .map(|(type_name, (first, rest)), span| {
//             let mut out = vec![first];
//             out.extend(rest);
//             TypeConstraint(type_name, out, span)
//         })
// }
//
// pub fn parse_proc_def<'a>() -> impl Parser<'a, Def> {
//     pair(
//         left(
//             parse_fn_def_header,
//             pair(
//                 match_token(TokenKind::LeftParen),
//                 match_token(TokenKind::RightParen))),
//         zero_or_more(parse_statement()))
//         .map(|(ident, stmts), span| Def::ProcDef(ident, stmts, span))
// }
//
// pub fn parse_const_def<'a>() -> impl Parser<'a, Def> {
//     pair(
//         right(
//             match_token(TokenKind::ConstKeyword),
//             parse_identifier()),
//         right(
//             match_token(TokenKind::ColonColon),
//             parse_literal()))
//         .map(|(i, l), span| Def::ConstDef(i, l, span))
// }
//
// // pub fn parse_type_def<'a>() -> impl Parser<'a, Def> {
// //     pair(
// //         right(
// //             match_token(TokenKind::TypeKeyword),
// //             left(
// //                 parse_identifier(),
// //                 match_token(TokenKind::ColonColon))),
// //         one_or_more(
// //             pair(
// //                 parse_identifier(),
// //                 right(
// //                     match_token(TokenKind::Colon),
// //                     parse_fn_type_param))))
// //         .map(|(i, v), span| Def::TypeDef(i, v, span))
// // }
//
// // pub fn parse_type_union_def<'a>() -> impl Parser<'a, Def> {
// //     pair(
// //         right(
// //             match_token(TokenKind::TypeKeyword),
// //             left(
// //                 pair(
// //                     parse_identifier(),
// //                     zero_or_more(parse_identifier())),
// //                 match_token(TokenKind::ColonColon))),
// //         parse_type_union())
// //         .map(|((type_name, poly_types), members), span| Def::TypeUnion(type_name, poly_types, members, span))
// // }
//
// pub fn parse_type_union<'a>() -> impl Parser<'a, Vec<TypeParam>> {
//     pair(
//         parse_fn_type_param,
//         zero_or_more(
//             right(
//                 match_token(TokenKind::Union),
//                 parse_fn_type_param)))
//         .map(|(first, rest), _| {
//             let mut types = vec![first];
//             types.extend(rest);
//             types
//         })
// }
//
// pub fn parse_type_union_member<'a>() -> impl Parser<'a, (Identifier, Option<Identifier>)> {
//     pair(
//         parse_identifier(),
//         zero_or_one(
//             surrounded(
//                 match_token(TokenKind::LeftParen),
//                 parse_identifier(),
//                 match_token(TokenKind::RightParen))))
// }
//
// pub fn parse_class_def<'a>() -> impl Parser<'a, Def> {
//     pair(
//         right(
//             match_token(TokenKind::ClassKeyword),
//             pair(
//                 parse_identifier(),
//                 left(
//                     parse_class_generic_types(),
//                     match_token(TokenKind::ColonColon)))),
//         parse_class_method_defs())
//         .map(|((class_name, params), methods), span|
//             Def::ClassDef(class_name, params, methods, span))
// }
//
// pub fn parse_class_generic_types<'a>() -> impl Parser<'a, Vec<Identifier>> {
//     one_or_more(parse_identifier())
// }
//
// pub fn parse_class_method_defs<'a>() -> impl Parser<'a, Vec<MethodDef>> {
//     one_or_more(parse_class_method_def())
// }
//
// pub fn parse_class_method_def<'a>() -> impl Parser<'a, MethodDef> {
//     or(
//         parse_class_method_def_operator(),
//         parse_class_method_def_named())
// }
//
// pub fn parse_class_method_def_operator<'a>() -> impl Parser<'a, MethodDef> {
//     pair(
//         left(
//             parse_method_operator,
//             match_token(TokenKind::ColonColon)),
//         pair(
//             parse_fn_type_params(),
//             right(
//                 match_token(TokenKind::FatArrow),
//                 or(
//                     parse_fn_type_param,
//                     surrounded(
//                         match_token(TokenKind::LeftParen),
//                         parse_fn_type_params().map(|params, span| TypeParam::TupleType(params, span)),
//                         match_token(TokenKind::RightParen))))))
//         .map(|(op, (params, ret)), span|
//             MethodDef::Operator(op, params, ret, span))
// }
//
// pub fn parse_class_method_type_params<'a>() -> impl Parser<'a, Vec<Identifier>> {
//     pair(
//         parse_identifier(),
//         zero_or_more(
//             right(
//                 match_token(TokenKind::Comma),
//                 parse_identifier())))
//         .map(|(first_type, types), _| {
//             let mut final_type_params = vec![first_type];
//
//             final_type_params.extend(types);
//
//             final_type_params
//         })
// }
//
// pub fn parse_method_operator(input: TokenStream) -> Output<MethodOperator> {
//     match input.next() {
//         Some((res, next_input)) => {
//             let op = match res.kind() {
//                 TokenKind::WrappedEqualsEquals => MethodOperator::EqualsEquals,
//                 TokenKind::WrappedNotEquals => MethodOperator::NotEquals,
//                 TokenKind::WrappedLessEquals => MethodOperator::LessEquals,
//                 TokenKind::WrappedGreaterEquals => MethodOperator::GreaterEquals,
//                 TokenKind::WrappedGreater => MethodOperator::Greater,
//                 TokenKind::WrappedLess => MethodOperator::Less,
//                 TokenKind::WrappedPlus => MethodOperator::Plus,
//                 TokenKind::WrappedMinus => MethodOperator::Minus,
//                 TokenKind::WrappedDivide => MethodOperator::Divide,
//                 TokenKind::WrappedMultiply => MethodOperator::Multiply,
//                 _ => return Err(
//                     ErrorType::Recoverable(
//                         ParserError::new()
//                         .message(format!("expected method operator, got {:?}", res.kind()))
//                         .span(res.span())
//                         .build())
//                 ),
//             };
//             Ok((next_input, op, res.span()))
//         },
//         None => Err(ErrorType::Incomplete)
//     }
// }
//
// pub fn parse_class_method_def_named<'a>() -> impl Parser<'a, MethodDef> {
//     pair(
//         left(
//             parse_identifier(),
//             match_token(TokenKind::ColonColon)),
//         pair(
//             parse_fn_type_params(),
//             right(
//                 match_token(TokenKind::FatArrow),
//                 parse_fn_type_param)))
//         .map(|(ident, (params, ret)), span|
//             MethodDef::Named(ident, params, ret, span))
// }
//
// pub fn parse_instance_def(input: TokenStream) -> Output<Def> {
//     pair(
//         left(
//             parse_instance_header(),
//             match_token(TokenKind::ColonColon)),
//         parse_instance_method_impls())
//         .map(|((cls, typ), impls), span|
//             Def::InstanceDef(cls, typ, impls, span))
//         .parse_next(input)
// }
//
// pub fn parse_instance_header<'a>() -> impl Parser<'a, (Identifier, Identifier)> {
//     right(
//         match_token(TokenKind::InstanceKeyword),
//         pair(
//             parse_identifier(),
//             parse_identifier()))
//         .map(|(class, ty), _| (class, ty))
// }
//
// pub fn parse_instance_method_impls<'a>() -> impl Parser<'a, Vec<MethodImpl>> {
//     one_or_more(parse_instance_method_impl())
// }
//
// pub fn parse_instance_method_impl<'a>() -> impl Parser<'a, MethodImpl> {
//     or(
//         parse_instance_method_impl_op(),
//         parse_instance_method_impl_named())
// }
//
// pub fn parse_instance_method_impl_op<'a>() -> impl Parser<'a, MethodImpl> {
//     pair(
//         left(
//             parse_method_operator,
//             match_token(TokenKind::ColonColon)),
//         parse_fn_expr)
//         .map(|(op, expr), span| MethodImpl::Operator(op, expr, span))
// }
//
// pub fn parse_instance_method_impl_named<'a>() -> impl Parser<'a, MethodImpl> {
//     pair(
//         left(
//             parse_identifier(),
//             match_token(TokenKind::ColonColon)),
//         parse_fn_expr)
//         .map(|(name, expr), span| MethodImpl::Named(name, expr, span))
// }
