use winnow::Parser;
use winnow::combinator::{alt, trace};
use winnow::error::{FromRecoverableError, ParserError};
use winnow::stream::{Location, Recover, Stream, StreamIsPartial};
use winnow::token::one_of;
use crate::lexer::token::{Token, TokenKind};
use crate::parser::ast::{Expr, AstSpan, Identifier, BinOperator};
use crate::parser::{parse_identifier, parse_literal, ParserInput, ParserOutput};
use crate::parser::ptr::P;

pub fn parse_expression(input: &mut ParserInput) -> ParserOutput<Expr> {
    parse_bin_op
        .parse_next(input)
}

pub fn parse_primary(input: &mut ParserInput) -> ParserOutput<Expr> {
    alt((
        parse_literal.map(|l| Expr::LitExpr(l.clone(), l.span())),
        parse_identifier.map(|i| Expr::IdentExpr(i.clone(), i.span()))
    ))
        .parse_next(input)
}

pub fn parse_bin_op(input: &mut ParserInput) -> ParserOutput<Expr> {
    parse_bin_op_inner(input, 0)
}

fn parse_bin_op_inner(input: &mut ParserInput, min_bp: u8) ->  ParserOutput<Expr>
{
    let mut lhs = parse_primary(input)?;
    let mut span = lhs.span();

    let start = input.checkpoint();

    loop {
        let op = match parse_operator(input) {
            Ok(op) => op,
            Err(_) => {
                input.reset(&start);
                break;
            },
        };

        let (lbp, rbp) = get_infix_binding_power(op.clone());
        if lbp < min_bp {
            break;
        }

        let rhs = parse_bin_op_inner(input, rbp)?;
        span = span.start..rhs.span().end;

        lhs = Expr::BinOpExpr(op, P(lhs), P(rhs), span.clone());
    }

    Ok(lhs)
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


pub fn parse_operator<'a>(input: &mut ParserInput) -> ParserOutput<BinOperator> {
    trace("parse_operator",
        one_of(|t: &Token|
            matches!(t.kind(),
            TokenKind::Plus | TokenKind::Minus | TokenKind::Multiply |
            TokenKind::Divide | TokenKind::Exp | TokenKind::And |
            TokenKind::Or | TokenKind::EqualsEquals | TokenKind::NotEquals |
            TokenKind::Less | TokenKind::LessEquals | TokenKind::Greater |
            TokenKind::GreaterEquals | TokenKind::Colon))
            .map(|t: &Token| match t.kind() {
                TokenKind::Plus => BinOperator::Plus,
                TokenKind::Minus => BinOperator::Minus,
                TokenKind::Multiply => BinOperator::Multiply,
                TokenKind::Divide => BinOperator::Divide,
                TokenKind::Exp => BinOperator::Exp,
                TokenKind::And => BinOperator::And,
                TokenKind::Or => BinOperator::Or,
                TokenKind::EqualsEquals => BinOperator::EqualsEquals,
                TokenKind::NotEquals => BinOperator::NotEquals,
                TokenKind::Less => BinOperator::Less,
                TokenKind::LessEquals => BinOperator::LessEquals,
                TokenKind::Greater => BinOperator::Greater,
                TokenKind::GreaterEquals => BinOperator::GreaterEquals,
                TokenKind::Colon => BinOperator::AppendSet,
                _ => unreachable!(),
            }))
        .parse_next(input)
}