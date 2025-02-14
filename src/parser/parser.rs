use super::combinator::*;
use crate::err::Span;
use crate::parser::ast::*;
use crate::{lexer::token::{Token, TokenKind}, parser::{ast, error::*, ptr::*}};

pub type Output<'a, Out> = miette::Result<(&'a [Token], Out, Span)>;

pub trait POut<'a, Out> {
    fn err(err: ParserError) -> Self;
    fn input(self) -> Option<&'a [Token]>;
    fn out(self) -> Option<Out>;
    fn span(self) -> Option<Span>;
}

impl<'a, Out> POut<'a, Out> for Output<'a, Out> {
    fn err(err: ParserError) -> Self {
        Err(err.into())
    }

    fn input(self) -> Option<&'a [Token]> {
        self.map(|(i, _, _)| i).ok()
    }

    fn out(self) -> Option<Out> {
        self.map(|(_, o, _)| o).ok()
    }

    fn span(self) -> Option<Span> {
        self.map(|(_, _, s)| s).ok()
    }
}

pub trait Parser<'a, Out> {
    fn parse(&self, input: &'a [Token]) -> Output<'a, Out>;

    fn map<Func, NewOut>(self, map_fn: Func) -> BoxedParser<'a, NewOut>
    where
        Self: Sized + 'a,
        Out: 'a,
        NewOut: 'a,
        Func: Fn(Out, Span) -> NewOut + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    fn and_then<Func, NewParser, NewOut>(self, f: Func) -> BoxedParser<'a, NewOut>
    where
        Self: Sized + 'a,
        Out: 'a,
        NewOut: 'a,
        NewParser: Parser<'a, NewOut> + 'a,
        Func: Fn(Out) -> NewParser + 'a,
    {
        BoxedParser::new(and_then(self, f))
    }
}

impl<'a, F, Out> Parser<'a, Out> for F
where
    F: Fn(&'a [Token]) -> Output<'a, Out>,
{
    fn parse(&self, input: &'a [Token]) -> Output<'a, Out> {
        self(input)
    }
}

pub struct BoxedParser<'a, Out> {
    parser: Box<dyn Parser<'a, Out> + 'a>,
}

impl<'a, Out> BoxedParser<'a, Out> {
    pub fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Out> + 'a,
    {
        Self {
            parser: Box::new(parser)
        }
    }
}

impl<'a, Out> Parser<'a, Out> for BoxedParser<'a, Out> {
    fn parse(&self, input: &'a [Token]) -> Output<'a, Out> {
        self.parser.parse(input)
    }
}

pub fn match_token<'a>(expected: TokenKind) -> impl Parser<'a, ()> {
    move |input: &'a [Token]| {
        match input.get(0) {
            Some(tok)
            if tok.kind() == expected =>
                Ok((&input[1..], (), tok.span())),
            Some(tok) => POut::err(ParserError::unexpected_tok(tok).into()),
            None => POut::err(ParserError::UnexpectedEOF),
        }
    }
}

pub fn parse_identifier<'a>() -> impl Parser<'a, Identifier> {
    move |input: &'a [Token]| {
        match input.get(0) {
            Some(tok) =>
                match &tok.kind() {
                    TokenKind::Identifier(i) =>
                        Ok((&input[1..], Identifier(i.clone()), tok.span())),
                    _ => POut::err(ParserError::expected_got("Identifier", tok))
                },
            None => POut::err(ParserError::UnexpectedEOF),
        }
    }
}

pub fn parse_literal<'a>() -> impl Parser<'a, Literal> {
    move |input: &'a [Token]| {
        match input.get(0) {
            Some(tok) =>
                match &tok.kind() {
                    TokenKind::Bool(b) => Ok((&input[1..], ast::Literal::from(*b), tok.span())),
                    TokenKind::Integer(i) => Ok((&input[1..], ast::Literal::from(*i), tok.span())),
                    TokenKind::Float(f) => Ok((&input[1..], ast::Literal::from(*f), tok.span())),
                    TokenKind::String(s) => Ok((&input[1..], ast::Literal::from(s.clone()), tok.span())),
                    _ => POut::err(ParserError::expected_got("literal", tok)),
                },
            None => POut::err(ParserError::UnexpectedEOF),
        }
    }
}

pub fn parse_literal_integer<'a>() -> impl Parser<'a, usize> {
    move |input: &'a [Token]| {
        match input.get(0) {
            Some(tok) => match &tok.kind() {
                TokenKind::Integer(i) => Ok((&input[1..], *i, tok.span())),
                _ => POut::err(ParserError::expected_got("literal_integer", tok)),
            },
            None => POut::err(ParserError::UnexpectedEOF),
        }
    }
}

pub fn parse_operator<'a>() -> impl Parser<'a, BinOperator> {
    use ast::BinOperator;
    move |input: &'a [Token]| {
        match input.get(0) {
            Some(tok) => match &tok.kind() {
                TokenKind::Plus => Ok((&input[1..], BinOperator::Plus, tok.span())),
                TokenKind::Minus => Ok((&input[1..], BinOperator::Minus, tok.span())),
                TokenKind::Multiply => Ok((&input[1..], BinOperator::Multiply, tok.span())),
                TokenKind::Divide => Ok((&input[1..], BinOperator::Divide, tok.span())),
                TokenKind::Exp => Ok((&input[1..], BinOperator::Exp, tok.span())),
                TokenKind::And => Ok((&input[1..], BinOperator::And, tok.span())),
                TokenKind::Or => Ok((&input[1..], BinOperator::Or, tok.span())),
                TokenKind::EqualsEquals => Ok((&input[1..], BinOperator::EqualsEquals, tok.span())),
                TokenKind::NotEquals => Ok((&input[1..], BinOperator::NotEquals, tok.span())),
                TokenKind::Less => Ok((&input[1..], BinOperator::Less, tok.span())),
                TokenKind::LessEquals => Ok((&input[1..], BinOperator::LessEquals, tok.span())),
                TokenKind::Greater => Ok((&input[1..], BinOperator::Greater, tok.span())),
                TokenKind::GreaterEquals => Ok((&input[1..], BinOperator::GreaterEquals, tok.span())),
                TokenKind::Colon => Ok((&input[1..], BinOperator::AppendSet, tok.span())),
                _ => POut::err(ParserError::expected_got("operator", tok)),
            },
            None => POut::err(ParserError::UnexpectedEOF),
        }
    }
}

// *************** DEFINITIONS **************************

pub fn parse_module<'a>() -> impl Parser<'a, Module> {
    zero_or_more(parse_definition())
        .map(|(defs), span| Module(defs, span))
}

pub fn parse_definition<'a>() -> impl Parser<'a, Def> {
    or_n(vec![
        BoxedParser::new(parse_instance_def),
        BoxedParser::new(parse_type_def()),
        BoxedParser::new(parse_type_union_def()),
        BoxedParser::new(parse_class_def()),
        BoxedParser::new(parse_fn_def),
        BoxedParser::new(parse_proc_def()),
        BoxedParser::new(parse_const_def()),
    ])
}

pub fn parse_fn_def(input: &[Token]) -> Output<Def> {
    pair(
        pair(
            parse_fn_def_header,
            parse_fn_def_types()),
        pair(
            zero_or_one(parse_fn_type_constraints()),
            parse_fn_expr(),
        ))
        .map(|((i, (tp, rt)), (cons, exp)), span| Def::FnDef(i, tp, rt, cons, exp, span))
        .parse(input)
}

pub fn parse_fn_def_header(input: &[Token]) -> Output<Identifier> {
    left(
        right(
            match_token(TokenKind::DefKeyword),
            parse_identifier()),
        match_token(TokenKind::ColonColon))
        .parse(input)
}

pub fn parse_fn_def_types<'a>() -> impl Parser<'a, (Vec<TypeParam>, TypeParam)> {
    pair(
        parse_fn_type_params(),
        right(
            match_token(TokenKind::FatArrow),
            or_n(vec![
                BoxedParser::new(parse_fn_type_param()),
                pair(
                    match_token(TokenKind::LeftParen),
                    match_token(TokenKind::RightParen))
                    .map(|(_, _), span| TypeParam::Empty(span)),
                BoxedParser::new(surrounded(
                    match_token(TokenKind::LeftParen),
                    parse_fn_type_params().map(|(params), span| TypeParam::Tuple(params, span)),
                    match_token(TokenKind::RightParen))),
            ])))
        .map(|(type_params, return_type), _| (type_params, return_type))
}

pub fn parse_fn_type_params<'a>() -> impl Parser<'a, Vec<TypeParam>> {
    pair(
        parse_fn_type_param(),
        zero_or_more(
            right(
                match_token(TokenKind::Comma),
                parse_fn_type_param())))
        .map(|(first_type, types), _| {
            let mut final_types = vec![first_type];
            final_types.extend(types);
            final_types
        })
}

pub fn parse_fn_type_param<'a>() -> impl Parser<'a, TypeParam> {
    or_n(vec![
        parse_identifier().map(|i, span| TypeParam::Type(i, span)),
        BoxedParser::new(parse_array_type_param()),
        BoxedParser::new(parse_fn_func_type_param),
        BoxedParser::new(parse_type_union_payload_type)
    ])
}

pub fn parse_array_type_param<'a>() -> impl Parser<'a, TypeParam> {
    right(
        match_token(TokenKind::LeftBracket),
        left(
            pair(
                parse_identifier(),
                zero_or_one(parse_literal_integer())),
            match_token(TokenKind::RightBracket)))
        .map(|(i, l), span| TypeParam::ArrayType(i, l, span))
}

pub fn parse_fn_func_type_param(input: &[Token]) -> Output<TypeParam> {
    pair(
        right(
            match_token(TokenKind::LeftParen),
            parse_fn_type_params()),
        right(
            match_token(TokenKind::FatArrow),
            left(
                parse_fn_type_param(),
                match_token(TokenKind::RightParen))))
        .map(|(params, ret), span| TypeParam::FuncType(params, P(ret), span))
        .parse(input)
}

pub fn parse_type_union_payload_type(input: &[Token]) -> Output<TypeParam> {
    surrounded(
        match_token(TokenKind::LeftParen),
        pair(
            parse_identifier(),
            one_or_more(parse_fn_type_param())),
        match_token(TokenKind::RightParen),
    )
        .map(|(i, params), span| TypeParam::PayloadType(i, params, span))
        .parse(input)
}

pub fn parse_fn_type_constraints<'a>() -> impl Parser<'a, Vec<TypeConstraint>> {
    left(
        right(
            match_token(TokenKind::WhereKeyword),
            zero_or_more(
                parse_fn_type_constraint())),
        match_token(TokenKind::InKeyword))
}

pub fn parse_fn_type_constraint<'a>() -> impl Parser<'a, TypeConstraint> {
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
}

pub fn parse_proc_def<'a>() -> impl Parser<'a, Def> {
    pair(
        left(
            parse_fn_def_header,
            pair(
                match_token(TokenKind::LeftParen),
                match_token(TokenKind::RightParen))),
        zero_or_more(parse_statement()))
        .map(|(ident, stmts), span| Def::ProcDef(ident, stmts, span))
}

pub fn parse_const_def<'a>() -> impl Parser<'a, Def> {
    pair(
        right(
            match_token(TokenKind::ConstKeyword),
            parse_identifier()),
        right(
            match_token(TokenKind::ColonColon),
            parse_literal()))
        .map(|(i, l), span| Def::ConstDef(i, l, span))
}

pub fn parse_type_def<'a>() -> impl Parser<'a, Def> {
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
                    parse_fn_type_param()))))
        .map(|(i, v), span| Def::TypeDef(i, v, span))
}

pub fn parse_type_union_def<'a>() -> impl Parser<'a, Def> {
    pair(
        right(
            match_token(TokenKind::TypeKeyword),
            left(
                pair(
                    parse_identifier(),
                    zero_or_more(parse_identifier())),
                match_token(TokenKind::ColonColon))),
        parse_type_union())
        .map(|((type_name, poly_types), members), span| Def::TypeUnion(type_name, poly_types, members, span))
}

pub fn parse_type_union<'a>() -> impl Parser<'a, Vec<TypeParam>> {
    pair(
        parse_fn_type_param(),
        zero_or_more(
            right(
                match_token(TokenKind::Union),
                parse_fn_type_param())))
        .map(|(first, rest), _| {
            let mut types = vec![first];
            types.extend(rest);
            types
        })
}

pub fn parse_type_union_member<'a>() -> impl Parser<'a, (Identifier, Option<Identifier>)> {
    pair(
        parse_identifier(),
        zero_or_one(
            surrounded(
                match_token(TokenKind::LeftParen),
                parse_identifier(),
                match_token(TokenKind::RightParen))))
}

pub fn parse_class_def<'a>() -> impl Parser<'a, Def> {
    pair(
        right(
            match_token(TokenKind::ClassKeyword),
            pair(
                parse_identifier(),
                left(
                    parse_class_generic_types(),
                    match_token(TokenKind::ColonColon)))),
        parse_class_method_defs())
        .map(|((class_name, params), methods), span|
            Def::ClassDef(class_name, params, methods, span))
}

pub fn parse_class_generic_types<'a>() -> impl Parser<'a, Vec<Identifier>> {
    one_or_more(parse_identifier())
}

pub fn parse_class_method_defs<'a>() -> impl Parser<'a, Vec<MethodDef>> {
    one_or_more(parse_class_method_def())
}

pub fn parse_class_method_def<'a>() -> impl Parser<'a, MethodDef> {
    or(
        parse_class_method_def_operator(),
        parse_class_method_def_named())
}

pub fn parse_class_method_def_operator<'a>() -> impl Parser<'a, MethodDef> {
    pair(
        left(
            parse_method_operator,
            match_token(TokenKind::ColonColon)),
        pair(
            parse_fn_type_params(),
            right(
                match_token(TokenKind::FatArrow),
                or(
                    parse_fn_type_param(),
                    surrounded(
                        match_token(TokenKind::LeftParen),
                        parse_fn_type_params().map(|params, span| TypeParam::Tuple(params, span)),
                        match_token(TokenKind::RightParen))))))
        .map(|(op, (params, ret)), span|
            MethodDef::Operator(op, params, ret, span))
}

pub fn parse_class_method_type_params<'a>() -> impl Parser<'a, Vec<Identifier>> {
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
}

pub fn parse_method_operator(input: &[Token]) -> Output<MethodOperator> {
    match input.get(0) {
        Some(tok) => match &tok.kind() {
            TokenKind::WrappedEqualsEquals => Ok((&input[1..], MethodOperator::EqualsEquals, tok.span())),
            TokenKind::WrappedNotEquals => Ok((&input[1..], MethodOperator::NotEquals, tok.span())),
            TokenKind::WrappedLessEquals => Ok((&input[1..], MethodOperator::LessEquals, tok.span())),
            TokenKind::WrappedGreaterEquals => Ok((&input[1..], MethodOperator::GreaterEquals, tok.span())),
            TokenKind::WrappedGreater => Ok((&input[1..], MethodOperator::Greater, tok.span())),
            TokenKind::WrappedLess => Ok((&input[1..], MethodOperator::Less, tok.span())),
            TokenKind::WrappedPlus => Ok((&input[1..], MethodOperator::Plus, tok.span())),
            TokenKind::WrappedMinus => Ok((&input[1..], MethodOperator::Minus, tok.span())),
            TokenKind::WrappedDivide => Ok((&input[1..], MethodOperator::Divide, tok.span())),
            TokenKind::WrappedMultiply => Ok((&input[1..], MethodOperator::Multiply, tok.span())),
            _ => POut::err(ParserError::expected_got("wrapped_operator", tok))
        },
        None => POut::err(ParserError::UnexpectedEOF)
    }
}

pub fn parse_class_method_def_named<'a>() -> impl Parser<'a, MethodDef> {
    pair(
        left(
            parse_identifier(),
            match_token(TokenKind::ColonColon)),
        pair(
            parse_fn_type_params(),
            right(
                match_token(TokenKind::FatArrow),
                parse_fn_type_param())))
        .map(|(ident, (params, ret)), span|
            MethodDef::Named(ident, params, ret, span))
}

pub fn parse_instance_def(input: &[Token]) -> Output<Def> {
    pair(
        left(
            parse_instance_header(),
            match_token(TokenKind::ColonColon)),
        parse_instance_method_impls())
        .map(|((cls, typ), impls), span|
            Def::InstanceDef(cls, typ, impls, span))
        .parse(input)
}

pub fn parse_instance_header<'a>() -> impl Parser<'a, (Identifier, Identifier)> {
    right(
        match_token(TokenKind::InstanceKeyword),
        pair(
            parse_identifier(),
            parse_identifier()))
        .map(|(class, ty), _| (class, ty))
}

pub fn parse_instance_method_impls<'a>() -> impl Parser<'a, Vec<MethodImpl>> {
    one_or_more(parse_instance_method_impl())
}

pub fn parse_instance_method_impl<'a>() -> impl Parser<'a, MethodImpl> {
    or(
        parse_instance_method_impl_op(),
        parse_instance_method_impl_named())
}

pub fn parse_instance_method_impl_op<'a>() -> impl Parser<'a, MethodImpl> {
    pair(
        left(
            parse_method_operator,
            match_token(TokenKind::ColonColon)),
        parse_fn_expr())
        .map(|(op, expr), span| MethodImpl::Operator(op, expr, span))
}

pub fn parse_instance_method_impl_named<'a>() -> impl Parser<'a, MethodImpl> {
    pair(
        left(
            parse_identifier(),
            match_token(TokenKind::ColonColon)),
        parse_fn_expr())
        .map(|(name, expr), span| MethodImpl::Named(name, expr, span))
}
// ******************** EXPRESSION *********************

pub fn parse_expression<'a>() -> impl Parser<'a, Expr> {
    or_n(vec![
        BoxedParser::new(parse_fn_expr_as_expr()),
        BoxedParser::new(parse_let_in_expression),
        BoxedParser::new(parse_if_then_else),
        BoxedParser::new(parse_case_expression),
        BoxedParser::new(parse_bin_op(0)),
    ])
}

// ********* BINARY EXPRESSION RELATED ***********

// PRATT parser for binary expressions
pub fn parse_bin_op<'a>(min_bp: u8) -> impl Parser<'a, Expr> {
    move |input| {
        let (mut input, mut lhs, mut span) = parse_primary().parse(input)?;

        loop {
            let (rest, op, op_span) = match parse_operator().parse(input) {
                Ok((i, o, sp)) => (i, o, sp),
                Err(_) => break,
            };
            span = span.combine(op_span);

            let (_, r_bp) = match get_infix_binding_power(op.clone()) {
                bp if bp.0 >= min_bp => bp,
                _ => break,
            };

            let (rest, rhs, sp2) = parse_bin_op(r_bp).parse(rest)?;
            input = rest;
            span = span.combine(sp2);

            lhs = Expr::BinOpExpr(op, P(lhs), P(rhs), span.clone())
        }

        Ok((input, lhs, span))
    }
}

fn get_infix_binding_power(op: BinOperator) -> (u8, u8) {
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

pub fn parse_parenthesized_expression<'a>() -> impl Parser<'a, Expr> {
    right(
        match_token(TokenKind::LeftParen),
        left(
            parse_expression(),
            match_token(TokenKind::RightParen)))
}

pub fn parse_unary_op(input: &[Token]) -> Output<Expr> {
    pair(
        parse_unary_operator_from_token,
        parse_primary())
        .map(|(op, expr), span| Expr::UnaryOp(op, P(expr), span))
        .parse(input)
}

pub fn parse_unary_operator_from_token(input: &[Token]) -> Output<UnaryOperator> {
    match input.get(0) {
        Some(tok) => match &tok.0 {
            TokenKind::Minus => Ok((&input[1..], UnaryOperator::Negative, tok.span())),
            TokenKind::Bang => Ok((&input[1..], UnaryOperator::Not, tok.span())),
            _ => POut::err(ParserError::expected_got("unary operator '!' or '-'", tok)),
        },
        None => POut::err(ParserError::UnexpectedEOF),
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
        BoxedParser::new(parse_fn_call),
        BoxedParser::new(parse_literal().map(|l, s| Expr::LitExpr(l, s))),
        BoxedParser::new(parse_identifier().map(|i, s| Expr::IdentExpr(i, s))),
    ])
}
//
// *************** FUNCTION EXPRESSION ***********************

pub fn parse_fn_expr_as_expr<'a>() -> impl Parser<'a, Expr> {
    parse_fn_expr().map(|f, s| Expr::FnExpr(P(f), s))
}

pub fn parse_fn_expr<'a>() -> impl Parser<'a, FnExpr> {
    or(
        parse_fn_expr_single,
        parse_fn_expr_case)
}

pub fn parse_fn_expr_single(input: &[Token]) -> Output<FnExpr> {
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

pub fn parse_fn_expr_case(input: &[Token]) -> Output<FnExpr> {
    right(
        match_token(TokenKind::CaseKeyword),
        one_or_more(parse_fn_expr_case_branch))
        .map(|fn_exprs, span| FnExpr::CaseFnExpr(fn_exprs, span))
        .parse(input)
}

pub fn parse_fn_expr_case_branch(input: &[Token]) -> Output<FnExpr> {
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

pub fn parse_empty_set(input: &[Token]) -> Output<()> {
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

pub fn parse_set_literal_field(input: &[Token]) -> Output<(Identifier, Expr)> {
    pair(
        left(
            parse_identifier(),
            match_token(TokenKind::Equals)),
        parse_expression())
        .parse(input)
}
// **************** LET IN ******************
pub fn parse_let_in_expression(input: &[Token]) -> Output<Expr> {
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

pub fn parse_case_expression(input: &[Token]) -> Output<Expr> {
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

// **************** STATEMENTS **********************

pub fn parse_statement<'a>() -> impl Parser<'a, Stmt> {
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
}

pub fn parse_fn_call_stmt<'a>() -> BoxedParser<'a, Stmt> {
    parse_fn_call
    .map(|expr, _| match expr {
        Expr::FnCallExpr(ident, args, span) => Stmt::FnCallStmt(ident, args, span),
        _ => unreachable!(),
    })
}

pub fn parse_proc_call(input: &[Token]) -> Output<Stmt> {
    left(
        parse_identifier(),
        match_token(TokenKind::Bang))
        .map(|ident, span| Stmt::ProcCallStmt(ident, span))
        .parse(input)
}


pub fn parse_type_assignment(input: &[Token]) -> Output<Stmt> {
    pair(
        pair(
            parse_identifier(),
            right(
                match_token(TokenKind::Colon),
                parse_identifier())),
        right(
            match_token(TokenKind::Equals),
            parse_expression()))
        .map(|((i, t), e), span| Stmt::AssignStmt(i, Some(t), e, span))
        .parse(input)
}

pub fn parse_inferred_assignment(input: &[Token]) -> Output<Stmt> {
    pair(
        parse_identifier(),
        right(
            match_token(TokenKind::InferredEquals),
            parse_expression()))
        .map(|(id, e), span| Stmt::AssignStmt(id, None, e, span))
        .parse(input)
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

pub fn parse_comma_seperated_expressions(input: &[Token]) -> Output<Vec<Expr>> {
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

pub fn parse_comma_seperated_identifiers(input: &[Token]) -> Output<Vec<Identifier>> {
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
        .parse(input)
}

pub fn parse_set_deconstruct_assignment(input: &[Token]) -> Output<Stmt> {
    pair(
        parse_set_deconstruct(),
        right(
            or(
                match_token(TokenKind::InferredEquals),
                match_token(TokenKind::Equals)),
            parse_comma_seperated_expressions.map(|exprs, span| Expr::TupleExpr(exprs, span))))
        .map(|(decon_set, exprs), span| Stmt::SetDeconstructAssignStmt(decon_set, exprs, span))
        .parse(input)
}

pub fn parse_case_statement(input: &[Token]) -> Output<Stmt> {
    right(
        match_token(TokenKind::CaseKeyword),
        pair(
            parse_identifier(),
            one_or_more(parse_case_stmt_branch)))
        .map(|(ident, branches), span| Stmt::CaseStmt(ident, branches, span))
        .parse(input)
}

pub fn parse_case_stmt_branch(input: &[Token]) -> Output<(Vec<FnParam>, Stmt)> {
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
        .parse(input)
}

pub fn parse_if_then_else(input: &[Token]) -> Output<Expr> {
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

pub fn parse_elseif_p_then_e(input: &[Token]) -> Output<(Expr, Expr)> {
    pair(
        right(
            match_token(TokenKind::ElseIfKeyword),
            left(
                parse_expression(),
                match_token(TokenKind::ThenKeyword))),
        parse_expression())
        .parse(input)
}

// **************** FN_CALL ***************************

pub fn parse_fn_call(input: &[Token]) -> Output<Expr> {
    pair(
        parse_identifier(),
        one_or_more(parse_fn_arg))
        .map(|(func_name, params), span| {
            Expr::FnCallExpr(func_name, params, span)
        })
        .parse(input)
}

pub fn parse_fn_arg(input: &[Token]) -> Output<Expr> {
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
