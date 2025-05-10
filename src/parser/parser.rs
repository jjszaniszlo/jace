use super::combinator::*;
use crate::err::Span;
use crate::parser::ast::*;
use crate::{lexer::token::{Token, TokenKind}, parser::{ast, error::*}};

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