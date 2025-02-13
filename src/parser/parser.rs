use crate::{lexer::token::{Token, TokenKind}, parser::{ast, ptr::*, error::*}};

use super::combinator::*;

pub type Span = (usize, usize);

pub type Output<'a, Out> = miette::Result<(&'a [Token], Out, Span)>;

pub trait POut<'a, Out> {
    fn ok(input: &'a [Token], out: Out, span: Span) -> Self;
    fn err(err: ParserError) -> Self;
    fn input(self) -> Option<&'a [Token]>;
    fn out(self) -> Option<Out>;
    fn span(self) -> Option<Span>;
}

impl<'a, Out> POut<'a, Out> for Output<'a, Out> {
    fn ok(input: &'a [Token], out: Out, span: Span) -> Self {
        Self::Ok((input, out, span))
    }

    fn err(err: ParserError) -> Self {
        Self::Err(err.into())
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
        Func: Fn(Out) -> NewOut + 'a,
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
    context("match_token",
        move |toks: &'a [Token]| {
            match toks.get(0) {
                Some(tok)
                    if tok.0 == expected =>
                        POut::ok(
                            &toks[1..],
                            (),
                            tok.1),
                Some(tok) => POut::err(ParserError::unexpected_tok(tok).into()),
                None => POut::err(ParserError::UnexpectedEOF),
            }
        }
    )
}

pub fn parse_identifier<'a>() -> impl Parser<'a, ast::Identifier> {
    context("parse_identifier",
    move |toks: &'a[Token]| {
        match toks.get(0) {
            Some(tok) =>
                match &tok.0 { 
                    TokenKind::Identifier(i) =>
                        POut::ok(&toks[1..], ast::Identifier(i.clone()), tok.1),
                    _ => POut::err(ParserError::expected_got("Identifier", tok))
                },
            None => POut::err(ParserError::UnexpectedEOF),
        }
    })
}

pub fn parse_literal<'a>() -> impl Parser<'a, ast::Literal> {
    context("parse_literal",
        move |input: &'a [Token]| {
            match input.get(0) {
                Some(tok) =>
                    match &tok.0 {
                        TokenKind::Bool(b) => Ok((&input[1..], ast::Literal::from(*b), tok.1)),
                        TokenKind::Integer(i) => Ok((&input[1..], ast::Literal::from(*i), tok.1)),
                        TokenKind::Float(f) => Ok((&input[1..], ast::Literal::from(*f), tok.1)),
                        TokenKind::String(s) => Ok((&input[1..], ast::Literal::from(s.clone()), tok.1)),
                         _ => POut::err(ParserError::expected_got("literal", tok)),
                    },
                None => POut::err(ParserError::UnexpectedEOF),
            }
    })
}

pub fn parse_literal_integer<'a>() -> impl Parser<'a, usize> {
    context("parse_literal_integer",
        move |input: &'a [Token]| {
            match input.get(0) {
                Some(t) => match &t.0 {
                    TokenKind::Integer(i) => Ok((&input[1..], *i, t.1)),
                    _ => POut::err(ParserError::expected_got("literal_integer", t)),
                },
                None => POut::err(ParserError::UnexpectedEOF),
            }
    })
}

pub fn parse_operator<'a>() -> impl Parser<'a, ast::BinOperator> {
    use ast::BinOperator;
    context("parse_operator",
    move |input: &'a [Token]| {
        match input.get(0) {
            Some(t) => match &t.0 {
                TokenKind::Plus => Ok((&input[1..], BinOperator::Plus, t.1)),
                TokenKind::Minus => Ok((&input[1..], BinOperator::Minus, t.1)),
                TokenKind::Multiply => Ok((&input[1..], BinOperator::Multiply, t.1)),
                TokenKind::Divide => Ok((&input[1..], BinOperator::Divide, t.1)),
                TokenKind::Exp => Ok((&input[1..], BinOperator::Exp, t.1)),
                TokenKind::And => Ok((&input[1..], BinOperator::And, t.1)),
                TokenKind::Or => Ok((&input[1..], BinOperator::Or, t.1)),
                TokenKind::EqualsEquals => Ok((&input[1..], BinOperator::EqualsEquals, t.1)),
                TokenKind::NotEquals => Ok((&input[1..], BinOperator::NotEquals, t.1)),
                TokenKind::Less => Ok((&input[1..], BinOperator::Less, t.1)),
                TokenKind::LessEquals => Ok((&input[1..], BinOperator::LessEquals, t.1)),
                TokenKind::Greater => Ok((&input[1..], BinOperator::Greater, t.1)),
                TokenKind::GreaterEquals => Ok((&input[1..], BinOperator::GreaterEquals, t.1)),
                TokenKind::Colon => Ok((&input[1..], BinOperator::AppendSet, t.1)),
                _ => POut::err(ParserError::expected_got("operator", t)),
            },
            None => POut::err(ParserError::UnexpectedEOF),
        }
    })
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
        BoxedParser::new(parse_type_alias_def()),
        BoxedParser::new(parse_class_def()),
        BoxedParser::new(parse_fn_def),
        BoxedParser::new(parse_proc_def()),
        BoxedParser::new(parse_const_def()),
    ])
}

pub fn parse_fn_def<'a>(input: &'a [Token]) -> Output<'a, ast::Def> {
    pair(
        pair(
            parse_fn_def_header,
            parse_fn_def_types()),
        pair(
            zero_or_one(parse_fn_type_constraints()),
            cut(
                parse_fn_expr(),
                "Expression expected after function definition!")))
    .map(|((i, (tp, rt)), (cons, exp))| ast::Def::FnDef(i, tp, rt, cons, exp))
    .parse(input)
}

pub fn parse_fn_def_header<'a>(input: &'a [Token]) -> Output<'a, ast::Identifier> {
    left(
        right(
            match_token(TokenKind::DefKeyword),
            parse_identifier()),
        cut(
            match_token(TokenKind::ColonColon),
            "Expected '::' after function or procedure defintion!"))
    .parse(input)
}

pub fn parse_fn_def_types<'a>() -> impl Parser<'a, (Vec<ast::TypeParam>, ast::TypeParam)> {
    pair(
        parse_fn_type_params(),
            right(
                match_token(TokenKind::FatArrow),
                    cut(
                        parse_fn_type_param(),
                        "Expected a return type followed by a '=>'!")))
    .map(|(type_params, return_type)| (type_params, return_type))
}

pub fn parse_fn_type_params<'a>() -> impl Parser<'a, Vec<ast::TypeParam>> {
    pair(
        parse_fn_type_param(),
        zero_or_more(
            right(
                match_token(TokenKind::Comma),
                parse_fn_type_param())))
    .map(|(first_type, types)| {
        let mut final_types = vec![first_type];
        final_types.extend(types);
        final_types
    })
}

pub fn parse_fn_type_param<'a>() -> impl Parser<'a, ast::TypeParam> {
    or_n(vec![
        parse_identifier().map(|i| ast::TypeParam::Type(i)),
        BoxedParser::new(parse_array_type_param()),
        BoxedParser::new(parse_fn_func_type_param),
    ])
}

pub fn parse_array_type_param<'a>() -> impl Parser<'a, ast::TypeParam> {
    right(
        match_token(TokenKind::LeftBracket),
        left(
            pair(
                parse_identifier(),
                zero_or_one(parse_literal_integer())),
            match_token(TokenKind::RightBracket)))
    .map(|(i, l)| ast::TypeParam::ArrayType(i, l))
}

pub fn parse_fn_func_type_param<'a>(input: &'a[Token]) -> Output<'a, ast::TypeParam> {
    pair(
        right(
            match_token(TokenKind::LeftParen),
            parse_fn_type_params()),
        right(
            match_token(TokenKind::FatArrow),
            left(
                parse_fn_type_param(),
                match_token(TokenKind::RightParen))))
    .map(|(params, ret)| ast::TypeParam::FuncType(params, P(ret)))
    .parse(input)
}

pub fn parse_fn_type_constraints<'a>() -> impl Parser<'a, Vec<(ast::Identifier, ast::Identifier)>> {
    right(
        match_token(TokenKind::WhereKeyword),
        left(
            one_or_more(
                pair(
                    left(
                        parse_identifier(),
                        match_token(TokenKind::Colon)),
                    parse_identifier())),
            match_token(TokenKind::InKeyword)))
}

pub fn parse_proc_def<'a>() -> impl Parser<'a, ast::Def> {
    pair(
        left(
            parse_fn_def_header,
            match_token(TokenKind::ProcType)),
        zero_or_more(parse_statement()))
    .map(|(ident, stmts)| ast::Def::ProcDef(ident, stmts))
}

pub fn parse_const_def<'a>() -> impl Parser<'a, ast::Def> {
    pair(
        right(
            match_token(TokenKind::ConstKeyword),
            parse_identifier()),
        right(
            match_token(TokenKind::ColonColon),
            parse_literal()))
    .map(|(i, l)| ast::Def::ConstDef(i, l))
}

pub fn parse_type_def<'a>() -> impl Parser<'a, ast::Def> {
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
                    parse_identifier()))))
    .map(|(i, v)| ast::Def::TypeDef(i, v))
}

pub fn parse_type_alias_def<'a>() -> impl Parser<'a, ast::Def> {
    pair(
        right(
            match_token(TokenKind::TypeKeyword),
            left(
                parse_identifier(),
                match_token(TokenKind::ColonColon))),
        parse_type_alias_types())
    .map(|(i, ty)| ast::Def::TypeAlias(i, ty)) 
}

pub fn parse_type_alias_types<'a>() -> impl Parser<'a, Vec<ast::TypeParam>> {
    pair(
        parse_fn_type_param(),
        zero_or_more(
            right(
            match_token(TokenKind::Union),
            parse_fn_type_param())))
    .map(|(p, ps)| {
        let mut final_params = vec![p];
        final_params.extend(ps);
        final_params
    })
}

pub fn parse_class_def<'a>() -> impl Parser<'a, ast::Def> {
    pair(
        right(
            match_token(TokenKind::ClassKeyword),
            pair(
                parse_identifier(),
                left(
                    parse_class_generic_types(),
                    match_token(TokenKind::ColonColon)))),
        parse_class_method_defs())
    .map(|((class_name, params), methods)|
        ast::Def::ClassDef(class_name, params, methods))
}

pub fn parse_class_generic_types<'a>() -> impl Parser<'a, Vec<ast::Identifier>> {
    one_or_more(parse_identifier())
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
            match_token(TokenKind::ColonColon)),
        pair(
            parse_class_method_type_params(),
            right(
                    match_token(TokenKind::FatArrow),
                    parse_identifier())))
    .map(|(op, (params, ret))|
        ast::MethodDef::Operator(op, params, ret))
}

pub fn parse_class_method_type_params<'a>() -> impl Parser<'a, Vec<ast::Identifier>> {
    pair(
        parse_identifier(),
        zero_or_more(
            right(
                match_token(TokenKind::Comma),
                parse_identifier())))
    .map(|(first_type, types)| {
        let mut final_type_params = vec![first_type];

        final_type_params.extend(types);

        final_type_params
    })
}

pub fn parse_method_operator<'a>(input: &'a [Token]) -> Output<'a, ast::MethodOperator> {
    match input.get(0) {
        Some(t) => match &t.0 {
            TokenKind::WrappedEqualsEquals => Ok((&input[1..], ast::MethodOperator::EqualsEquals, t.1)),
            TokenKind::WrappedNotEquals => Ok((&input[1..], ast::MethodOperator::NotEquals, t.1)),
            TokenKind::WrappedLessEquals => Ok((&input[1..], ast::MethodOperator::LessEquals, t.1)),
            TokenKind::WrappedGreaterEquals => Ok((&input[1..], ast::MethodOperator::GreaterEquals, t.1)),
            TokenKind::WrappedGreater => Ok((&input[1..], ast::MethodOperator::Greater, t.1)),
            TokenKind::WrappedLess => Ok((&input[1..], ast::MethodOperator::Less, t.1)),
            TokenKind::WrappedPlus => Ok((&input[1..], ast::MethodOperator::Plus, t.1)),
            TokenKind::WrappedMinus => Ok((&input[1..], ast::MethodOperator::Minus, t.1)),
            TokenKind::WrappedDivide => Ok((&input[1..], ast::MethodOperator::Divide, t.1)),
            TokenKind::WrappedMultiply => Ok((&input[1..], ast::MethodOperator::Multiply, t.1)),
            _ => POut::err(ParserError::expected_got("wrapped_operator", t))
        },
        None => POut::err(ParserError::UnexpectedEOF)
    }
}

pub fn parse_class_method_def_named<'a>() -> impl Parser<'a, ast::MethodDef> { 
    pair(
        left(
            parse_identifier(),
            match_token(TokenKind::ColonColon)),
        pair(
            parse_class_method_type_params(),
            right(
                    match_token(TokenKind::FatArrow),
                    parse_identifier())))
    .map(|(ident, (params, ret))|
        ast::MethodDef::Named(ident, params, ret))
}

pub fn parse_instance_def<'a>(input: &'a [Token]) -> Output<'a, ast::Def> {
    pair(
        left(
            pair(
                parse_instance_header(),
                parse_fn_expr_params()),
            match_token(TokenKind::ColonColon)),
        parse_instance_method_impls())
    .map(|(((cls, typ), params), impls)|
        ast::Def::InstanceDef(cls, typ, params, impls))
    .parse(input)
}

pub fn parse_instance_header<'a>() -> impl Parser<'a, (ast::Identifier, ast::Identifier)> {
    right(
        match_token(TokenKind::InstanceKeyword),
        pair(
            parse_identifier(),
            parse_identifier()))
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
            match_token(TokenKind::FatArrow)),
        parse_expression())
    .map(|(op, expr)| ast::MethodImpl::Operator(op, expr))
}

pub fn parse_instance_method_impl_named<'a>() -> impl Parser<'a, ast::MethodImpl> {
    pair(
        left(
            parse_identifier(),
            match_token(TokenKind::FatArrow)),
        parse_expression())
    .map(|(name, expr)| ast::MethodImpl::Named(name, expr))
}
// ******************** EXPRESSION *********************

pub fn parse_expression<'a>() -> impl Parser<'a, ast::Expr> {
    or_n(vec![
        BoxedParser::new(parse_fn_expr_as_expr()),
        BoxedParser::new(parse_let_in_expression),
        BoxedParser::new(parse_if_then_else),
        BoxedParser::new(parse_bin_op(0)),
    ])
}

// ********* BINARY EXPRESSION RELATED ***********

// PRATT parser for binary expressions
pub fn parse_bin_op<'a>(min_bp: u8) -> impl Parser<'a, ast::Expr> {
    move |input| {
        let (mut input, mut lhs, mut span) = parse_primary().parse(input)?;
  
        loop {
            let (rest, op, op_span) = match parse_operator().parse(input) {
                Ok((i, o, sp)) => (i, o, sp),
                Err(_) => break,
            };

            let (_, r_bp) = match get_infix_binding_power(op.clone()) {
                bp if bp.0 >= min_bp => bp,
                _ => break,
            };

            let (rest, rhs, sp2) = parse_bin_op(r_bp).parse(rest)?;
            input = rest;
            span.1 = sp2.1;
            
            lhs = ast::Expr::BinOpExpr(op, P(lhs), P(rhs))
        }

        Ok((input, lhs, span))
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
    right(
        match_token(TokenKind::LeftParen),
            left(
                parse_expression(),
                match_token(TokenKind::RightParen)))
}

pub fn parse_unary_op<'a>(input: &'a [Token]) -> Output<'a, ast::Expr> {
    pair(
        parse_unary_operator_from_token,
        parse_primary())
    .map(|(op, expr)| ast::Expr::UnaryOp(op, P(expr)))
    .parse(input)
}

pub fn parse_unary_operator_from_token<'a>(input: &'a [Token]) -> Output<'a, ast::UnaryOperator> {
    match input.get(0) {
        Some(t) => match &t.0 {
            TokenKind::Minus => Ok((&input[1..], ast::UnaryOperator::Negative, t.1)),
            TokenKind::Bang => Ok((&input[1..], ast::UnaryOperator::Not, t.1)),
            _ => POut::err(ParserError::expected_got("unary operator '!' or '-'", t)),
        },
        None => POut::err(ParserError::UnexpectedEOF),
    }
}

pub fn parse_member_expr<'a>() -> impl Parser<'a, ast::Expr> {
    pair(
        parse_identifier(),
        one_or_more(
            right(
            match_token(TokenKind::Dot),
            parse_identifier())))
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
        BoxedParser::new(parse_literal().map(|l| ast::Expr::LitExpr(l))),
        BoxedParser::new(parse_identifier().map(|i| ast::Expr::IdentExpr(i))),
    ])
}
//
// *************** FUNCTION EXPRESSION ***********************

pub fn parse_fn_expr_as_expr<'a>() -> impl Parser<'a, ast::Expr> {
    parse_fn_expr().map(|f| ast::Expr::FnExpr(P(f)))
}

pub fn parse_fn_expr<'a>() -> impl Parser<'a, ast::FnExpr> { 
    or(
        parse_fn_expr_single, 
        parse_fn_expr_case)
}

pub fn parse_fn_expr_single<'a>(input: &'a [Token]) -> Output<'a, ast::FnExpr> {
    pair(
        left(
            parse_fn_expr_params(),
            match_token(TokenKind::FatArrow)),
        cut(
            parse_comma_seperated_expressions,
            "Expression expected after function params and '=>' !"))
    .map(|(params, expr)| ast::FnExpr::FnExpr(params, expr))
    .parse(input)
}

pub fn parse_fn_expr_case<'a>(input: &'a [Token]) -> Output<'a, ast::FnExpr> {
    right(
        match_token(TokenKind::CaseKeyword),
        cut(
            one_or_more(parse_fn_expr_case_branch),
            "One or more function expression expected for case block!"))
    .map(|fn_exprs| ast::FnExpr::CaseFnExpr(fn_exprs))
    .parse(input)
}

pub fn parse_fn_expr_case_branch<'a>(input: &'a [Token]) -> Output<'a, ast::FnExpr> {
    pair(
        left(
            parse_fn_expr_case_params(),
            match_token(TokenKind::FatArrow)),
        parse_comma_seperated_expressions)
    .map(|(params, expr)| ast::FnExpr::FnExpr(params, expr))
    .parse(input)
}

pub fn parse_fn_expr_params<'a>() -> impl Parser<'a, Vec<ast::FnParam>> {
    pair(
        parse_fn_expr_param(),
        zero_or_more(
            right(
            match_token(TokenKind::Comma),
            parse_fn_expr_param())))
    .map(|(first_param, params)| {
        let mut final_params = vec![first_param];
        final_params.extend(params);
        final_params
    })
}

pub fn parse_fn_expr_param<'a>() -> impl Parser<'a, ast::FnParam> {
    or_n(vec![
        parse_identifier().map(|i| ast::FnParam::IdentParam(i)),
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
            match_token(TokenKind::Comma),
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
        parse_literal().map(|l| ast::FnParam::LiteralParam(l)))
}

pub fn parse_set_deconstruct<'a>() -> impl Parser<'a, Vec<ast::Identifier>> {
    right(
        match_token(TokenKind::LeftBrace),
        left(
            parse_comma_seperated_identiers,
            match_token(TokenKind::RightBrace)))
}

pub fn parse_fn_param_set_selector<'a>() -> impl Parser<'a, ast::FnParam> {
    pair(
        right(
            match_token(TokenKind::LeftBrace),
            parse_identifier()),
        right(
            match_token(TokenKind::Colon),
                left(
                    parse_identifier(),
                    match_token(TokenKind::RightBrace))))
    .map(|(first, rest)| ast::FnParam::SetSelectorParam(first, rest))                
}

// *************** SET LITERAL ***********************

pub fn parse_set_array<'a>() -> impl Parser<'a, ast::Expr> {
    or(
        parse_empty_set.map(|_| ast::Expr::ArrayExpr(vec![])),
        right(
            match_token(TokenKind::LeftBrace),
            left(
                    parse_comma_seperated_expressions,
                    match_token(TokenKind::RightBrace))) 
        .map(|exprs| ast::Expr::ArrayExpr(exprs)))
}

pub fn parse_empty_set<'a>(input: &'a [Token]) -> Output<'a, ()> {
    pair(
        match_token(TokenKind::LeftBrace),
        match_token(TokenKind::RightBrace))
    .map(|_| ())
    .parse(input)
}

pub fn parse_set_literal<'a>() -> impl Parser<'a, ast::Expr> {
    or(
        parse_empty_set.map(|_| ast::Expr::ArrayExpr(vec![])),
        right(
            match_token(TokenKind::LeftBrace),
            left(
                zero_or_one(parse_set_literal_comma_seperated_fields()),
                match_token(TokenKind::RightBrace)))
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
                match_token(TokenKind::Comma),
                parse_set_literal_field)))
    .map(|(first, rest)| {
        let mut fields = vec![first];
        fields.extend(rest);
        fields
    }) 
}

pub fn parse_set_literal_field<'a>(input: &'a [Token]) -> Output<'a, (ast::Identifier, ast::Expr)> {
    pair(
        left(
            parse_identifier(),
            match_token(TokenKind::Equals)),
        parse_expression())
    .parse(input)
}
// **************** LET IN ******************
pub fn parse_let_in_expression<'a>(input: &'a [Token]) -> Output<'a, ast::Expr> {
    right(
        match_token(TokenKind::LetKeyword),
        pair(
            left(
                zero_or_more(parse_statement()),
                match_token(TokenKind::InKeyword)),
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
            _ => unreachable!(),
        }),
    ])
}

pub fn parse_proc_call<'a>(input: &'a [Token]) -> Output<'a, ast::Stmt> {
    left(
        parse_identifier(),
        match_token(TokenKind::Bang))
    .map(|ident| ast::Stmt::ProcCallStmt(ident))
    .parse(input)
}


pub fn parse_type_assignment<'a>(input: &'a [Token]) -> Output<'a, ast::Stmt> {
    pair(
        pair(
            parse_identifier(),
            right(
                match_token(TokenKind::Colon),
                parse_identifier())),
        right(
            match_token(TokenKind::Equals),
            parse_expression()))
    .map(|((i, t), e)| ast::Stmt::AssignStmt(i, Some(t), e))
    .parse(input)
}

pub fn parse_inferred_assignment<'a>(input: &'a [Token]) -> Output<'a, ast::Stmt> {
    pair(
        parse_identifier(),
        right(
            match_token(TokenKind::InferredEquals),
            parse_expression()))
    .map(|(id, e)| ast::Stmt::AssignStmt(id, None, e))
    .parse(input)
}

pub fn parse_inferred_multi_assign_statement<'a>() -> impl Parser<'a, ast::Stmt> {
    pair(
        left(
            parse_comma_seperated_identiers,
            match_token(TokenKind::InferredEquals)),
        parse_comma_seperated_expressions) 
    .map(|(idents, exprs)| ast::Stmt::MultiAssignStmt(idents, None, exprs))
}

pub fn parse_typed_multi_assign_statement<'a>() -> impl Parser<'a, ast::Stmt> {
    pair(
        pair(
            parse_comma_seperated_identiers,
            right(
                match_token(TokenKind::Colon),    
                parse_comma_seperated_identiers)),
        right(
                match_token(TokenKind::Equals),
            parse_comma_seperated_expressions))
    .map(|((idents, types), exprs)| ast::Stmt::MultiAssignStmt(idents, Some(types), exprs))
}

pub fn parse_comma_seperated_expressions<'a>(input: &'a [Token]) -> Output<'a, Vec<ast::Expr>> {
    pair(
        parse_expression(),
        zero_or_more(
            right(
            match_token(TokenKind::Comma),
            parse_expression())))
    .map(|(first_ident, idents)| {
        let mut final_idents = vec![first_ident];
        final_idents.extend(idents);
        final_idents
    })
    .parse(input)
}

pub fn parse_comma_seperated_identiers<'a>(input: &'a [Token]) -> Output<'a, Vec<ast::Identifier>> {
    pair(
        parse_identifier(),
        zero_or_more(
            right(
            match_token(TokenKind::Comma),
            parse_identifier())))
    .map(|(first_ident, idents)| {
        let mut final_idents = vec![first_ident];
        final_idents.extend(idents);
        final_idents
    })
    .parse(input)
}

pub fn parse_set_deconstruct_assignment<'a>(input: &'a [Token]) -> Output<'a, ast::Stmt> {
    pair(
        parse_set_deconstruct(),
        right(
            or(
                match_token(TokenKind::InferredEquals),
                match_token(TokenKind::Equals)),
            parse_comma_seperated_expressions))
    .map(|(decon_set, exprs)| ast::Stmt::SetDeconstructAssignStmt(decon_set, exprs))
    .parse(input)
}

pub fn parse_if_then_else<'a>(input: &'a [Token]) -> Output<'a, ast::Expr> {
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
    .map(|(pred, ((expr, elseifs), else_expr))| {
        let mut final_result = vec![(pred, expr)];
        final_result.extend(elseifs);

        ast::Expr::IfThenElseIfExpr(final_result, P(else_expr))})
    .parse(input)
}

pub fn parse_elseif_p_then_e<'a>(input: &'a [Token]) -> Output<'a, (ast::Expr, ast::Expr)> {
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

pub fn parse_fn_call<'a>(input: &'a [Token]) -> Output<'a, ast::Expr> {
    pair(
        parse_identifier(),
        left(
            one_or_more(parse_fn_arg),
            cut(
                match_token(TokenKind::Bang),
                "Function call not terminated by '!'!")))
    .map(|(func_name, params)| {
        ast::Expr::FnCallExpr(func_name, params)
    })
    .parse(input)
}

pub fn parse_fn_arg<'a>(input: &'a [Token]) -> Output<'a, ast::Expr> {
    or_n(vec![
        BoxedParser::new(parse_parenthesized_expression()),
        BoxedParser::new(parse_set_literal()),
        BoxedParser::new(parse_set_array()),
        BoxedParser::new(parse_literal().map(|l| ast::Expr::LitExpr(l))),
        BoxedParser::new(parse_member_expr()),
        BoxedParser::new(parse_identifier().map(|i| ast::Expr::IdentExpr(i))),
    ])
    .parse(input)
}
