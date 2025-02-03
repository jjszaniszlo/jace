use std::{collections::VecDeque, sync::Arc};

use crate::lexer::{token::Token, LexerError};

use crate::parser::ast::*;
use super::*;

#[derive(Debug, Clone, PartialEq)]
enum ParserError {
    CouldNotMatchToken,
    CouldNotMatchTokenOneOrMore,
}

type ParseResult<'a, O> = Result<(&'a [Token], O), ParserError>;

trait Parser<'a, O> {
    fn parse(&self, input: &'a [Token]) -> ParseResult<'a, O>;

    fn map<F, N>(self, map_fn: F) -> BoxedParser<'a, N>
    where
        Self: Sized + 'a,
        O: 'a,
        N: 'a,
        F: Fn(O) -> N + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a [Token]) -> ParseResult<'a, Output>,
{
    fn parse(&self, input: &'a [Token]) -> ParseResult<'a, Output> {
        self(input)
    }
}

struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        Self {
            parser: Box::new(parser)
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a [Token]) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
}

fn match_token<'a>(expected: Token) -> impl Parser<'a, ()> {
    move |toks: &'a [Token]| match toks.get(0) {
        Some(tok) if *tok == expected => Ok((&toks[1..], ())),
        _ => Err(ParserError::CouldNotMatchToken),
    }
}

fn match_literal<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Literal> {
    match input.get(0) {
        Some(t) => match t {
            Token::Bool(b) => Ok((&input[1..], ast::Literal::from(*b))),
            Token::Integer(i) => Ok((&input[1..], ast::Literal::from(*i))),
            Token::Float(f) => Ok((&input[1..], ast::Literal::from(*f))),
            Token::String(s) => Ok((&input[1..], ast::Literal::from(s.clone()))),
            _ => Err(ParserError::CouldNotMatchToken),
        },
        None => Err(ParserError::CouldNotMatchToken),
    }
}

fn match_identifier<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Identifier> {
    match input.get(0) {
        Some(t) => match t { 
            Token::Identifier(i) => Ok((&input[1..], ast::Identifier(i.clone()))),
            _ => Err(ParserError::CouldNotMatchToken),
        },
        None => Err(ParserError::CouldNotMatchToken),
    }
}

fn pair<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        p1.parse(input).and_then(|(next, r1)| {
            p2.parse(next).map(|(last, r2)| (last, (r1, r2)))
        })
    }
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| parser.parse(input).map(|(next, result)| (next, map_fn(result)))
}

fn left<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(p1, p2), |(l, _)| l)
}

fn right<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(p1, p2), |(_, r)| r)
}

fn or<'a, P1, P2, R>(p1: P1, p2: P2) -> impl Parser<'a, R>
where
    P1: Parser<'a, R>,
    P2: Parser<'a, R>,
{
    move |input| {
        p1.parse(input).or_else(|_| p2.parse(input))
    }
}

fn or_n<'a, P, R>(ps: Vec<P>) -> impl Parser<'a, R>
where
    P: Parser<'a, R>
{
    move |input| {
        ps.iter().fold(
            Err(ParserError::CouldNotMatchToken),
            |prev, curr| prev.or_else(|_| curr.parse(input))
        )
    }
}

fn zero_or_more<'a, P, A>(p: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>
{
    move |mut input| {
        let result: Vec<A> = std::iter::from_fn(|| {
            match p.parse(input) {
                Ok((next, result)) => {
                    input = next;
                    Some(result)
                },
                Err(_) => None,
            }
        })
        .collect();

        Ok((input, result))
    }
}

fn one_or_more<'a, P, A>(p: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let result: Vec<A> = std::iter::from_fn(|| {
            match p.parse(input) {
                Ok((next, result)) => {
                    input = next;
                    Some(result)
                },
                Err(_) => None,
            }
        })
        .collect();

        Ok((input, result))
    }
}

fn parse_expression<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Expr> {
    or(
        match_literal.map(|l| ast::Expr::Literal(l)),
        match_identifier.map(|i| ast::Expr::Identifier(i)))
    .parse(input)
}

fn parse_type_assignment<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Stmt> {
    pair(
        pair(
            match_identifier,
            right(
                match_token(Token::Colon),
                match_identifier)),
        right(
            match_token(Token::Equals),
            match_literal))
    .map(|((i, t), l)| ast::Stmt::Asmt(i, Some(ast::TypeName(t)), ast::Expr::Literal(l)))
    .parse(input)
}

fn parse_inferred_assignment<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Stmt> {
    pair(
        match_identifier,
        right(
            match_token(Token::InferredEquals),
            match_literal))
    .map(|(id, lit)| ast::Stmt::Asmt(id, None, Expr::Literal(lit)))
    .parse(input)
}

fn parse_type_definition<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Def> {
    pair(
        right(
            match_token(Token::TypeKeyword),
            left(
                match_identifier,
                match_token(Token::ColonColon))),
        one_or_more(
            pair(
                match_identifier,
                right(
                    match_token(Token::Colon),
                    match_identifier))))
    .map(|(i, v)| ast::Def::TypeDef(
            ast::TypeName(i), 
            v.into_iter().map(|(i, t)| (i, ast::TypeName(t))).collect()))
    .parse(input) 
}

//fn parse_set_literal<'a>(input: &'a [Token]) -> ParseResult<'a, ast::Literal> {
//
//}

fn parse_statement<'a>() -> impl Parser<'a, ast::Stmt> {
    or(parse_inferred_assignment,parse_type_assignment)
}

pub fn parse<'a>(input: &'a [Token]) -> ast::Stmt {
    let (_, result) = parse_statement().parse(input).unwrap();
    result
}

#[test]
fn parse_type_definition_test() {
    let toks= vec![
        Token::TypeKeyword,
        Token::from("Person"),
        Token::ColonColon,
        Token::from("name"),
        Token::Colon,
        Token::from("String"),
        Token::from("age"),
        Token::Colon,
        Token::from("Integer"),
    ];

    let (_, result) = parse_type_definition(&toks).unwrap();

    assert_eq!(
        ast::Def::TypeDef(
            ast::TypeName::from("Person"),
            vec![
                (ast::Identifier::from("name"), ast::TypeName::from("String")),
                (ast::Identifier::from("age"), ast::TypeName::from("Integer")),
            ]),
        result);
}

#[test]
fn parse_assignment_test() {
    let toks = vec![
        Token::from("num"),
        Token::InferredEquals,
        Token::from(10)
    ];

    let toks2 = vec![
        Token::from("num"),
        Token::Colon,
        Token::from("Integer"),
        Token::Equals,
        Token::from(10)
    ];

    let (_, result) = parse_statement().parse(&toks).unwrap();
    let (_, result2) = parse_statement().parse(&toks2).unwrap();

    assert_eq!(
        ast::Stmt::Asmt(ast::Identifier::from("num"), None, ast::Expr::Literal(ast::Literal::from(10))),
        result
    );

    assert_eq!(
        ast::Stmt::Asmt(ast::Identifier::from("num"), Some(ast::TypeName::from("Integer")), ast::Expr::Literal(ast::Literal::from(10))),
        result2
    );
}

//#[test]
//fn left_combinator() {
//    let toks = vec![
//        Token::from("num"),
//        Token::InferredEquals,
//        Token::Integer(2),
//    ];
//
//    let assign = left(match_payload, match_token(Token::InferredEquals));
//
//    assert_eq!(
//        Ok((&[Token::Integer(2)] as &[Token], &Token::from("num"))),
//        assign.parse(&toks)
//    );
//}
//
//#[test]
//fn right_combinator() {
//    let toks = vec![
//        Token::from("num"),
//        Token::InferredEquals,
//        Token::Integer(2),
//    ];
//
//    let assign = right(match_payload, match_token(Token::InferredEquals));
//
//    assert_eq!(
//        Ok((&[Token::Integer(2)] as &[Token], ())),
//        assign.parse(&toks)
//    );
//}
//
//#[test]
//fn or_combinator() {
//    let toks1= vec![
//        Token::TypeKeyword,
//    ];
//
//    let toks2= vec![
//        Token::ClassKeyword,
//    ];
//
//    let class_or_type = or(
//        match_token(Token::TypeKeyword),
//        match_token(Token::ClassKeyword)
//    );
//
//    assert_eq!(
//        Ok((&[] as &[Token], ())),
//        class_or_type.parse(&toks1)
//    );
//
//    assert_eq!(
//        Ok((&[] as &[Token], ())),
//        class_or_type.parse(&toks2)
//    );
//}
//
//#[test]
//fn parse_assignment() {
//    let toks= vec![
//        Token::from("num"),
//        Token::InferredEquals,
//        Token::from(10),
//    ];
//
//    let assignment =
//        pair(
//            match_payload,
//            right(
//                match_token(Token::InferredEquals),
//                match_payload));
//
//    assert_eq!(
//        Ok((&[] as &[Token], (&Token::from("num"), &Token::from(10)))),
//        assignment.parse(&toks));
//}
//
//#[test]
//fn parse_type() {
//    let toks= vec![
//        Token::TypeKeyword,
//        Token::from("Person"),
//        Token::ColonColon,
//        Token::from("name"),
//        Token::Colon,
//        Token::from("String"),
//        Token::from("age"),
//        Token::Colon,
//        Token::from("Integer"),
//    ];
//
//    let type_def = 
//        pair(
//            right(
//                match_token(Token::TypeKeyword),
//                left(
//                        match_payload,
//                        match_token(Token::ColonColon))),
//            one_or_more(
//                pair(
//                    match_payload,
//                    right(
//                        match_token(Token::Colon),
//                        match_payload))));
//
//    let result = type_def.parse(&toks);
//
//    let (_, (class_name, members)) = result.clone().unwrap();
//
//    assert_eq!(
//        *class_name,
//        Token::from("Person")
//    );
//
//    assert_eq!(
//        *members,
//        vec![
//            (&Token::from("name"), &Token::from("String")),
//            (&Token::from("age"), &Token::from("Integer")),
//        ]
//    );
//
//    assert_eq!(
//        Ok((&[] as &[Token], (&Token::from("Person"), vec![(&Token::from("name"), &Token::from("String")), (&Token::from("age"), &Token::from("Integer"))]))),
//        result
//    );
//}
