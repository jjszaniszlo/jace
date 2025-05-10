use crate::parser::combinator::*;
use crate::parser::parser::{match_token, Parser};
use crate::parser::tokenstream::{TokenStream};
use crate::lexer::token::{Token, TokenKind};
use std::ops::Range;

fn dummy_token(kind: TokenKind, start: usize, length: usize) -> Token {
    Token::new(kind, start, length)
}

fn dummy_token_stream<'a>(tokens: &'a [Token]) -> TokenStream<'a> {
    TokenStream::new(tokens)
}

#[test]
fn test_pair() {
    let tokens = vec![
        dummy_token(TokenKind::Plus, 0, 1),
        dummy_token(TokenKind::Minus, 1, 1),
    ];

    let parser = pair(
        match_token(TokenKind::Plus),
        match_token(TokenKind::Minus),
    );

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_ok());
    let (_, (left, right), span) = result.unwrap();
    assert_eq!(left, ());
    assert_eq!(right, ());
    assert_eq!(span, 0..2);
}

#[test]
fn test_or() {
    let tokens = vec![dummy_token(TokenKind::Minus, 0, 1)];

    let parser = or(
        match_token(TokenKind::Plus),
        match_token(TokenKind::Minus),
    );

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_ok());
    let (_, output, span) = result.unwrap();
    assert_eq!(output, ());
    assert_eq!(span, 0..1);
}

#[test]
fn test_or_no_match() {
    let tokens = vec![dummy_token(TokenKind::Multiply, 0, 1)];

    let parser = or(
        match_token(TokenKind::Plus),
        match_token(TokenKind::Minus),
    );

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_err());
}

#[test]
fn test_zero_or_more() {
    let tokens = vec![
        dummy_token(TokenKind::Plus, 0, 1),
        dummy_token(TokenKind::Plus, 1, 1),
    ];
    let parser = zero_or_more(match_token(TokenKind::Plus));

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_ok());
    let (_, output, span) = result.unwrap();
    assert_eq!(output.len(), 2);
    assert_eq!(span, 0..2);
}

#[test]
fn test_zero_or_more_no_match() {
    let tokens = vec![dummy_token(TokenKind::Minus, 0, 1)];
    let parser = zero_or_more(match_token(TokenKind::Plus));

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_ok());
    let (_, output, span) = result.unwrap();
    assert!(output.is_empty());
    assert_eq!(span, 0..0);
}

#[test]
fn test_one_or_more() {
    let tokens = vec![
        dummy_token(TokenKind::Plus, 0, 1),
        dummy_token(TokenKind::Plus, 1, 1),
    ];
    let parser = one_or_more(match_token(TokenKind::Plus));

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_ok());
    let (_, output, span) = result.unwrap();
    assert_eq!(output.len(), 2);
    assert_eq!(span, 0..2);
}

#[test]
fn test_one_or_more_no_match() {
    let tokens = vec![dummy_token(TokenKind::Minus, 0, 1)];
    let parser = one_or_more(match_token(TokenKind::Plus));

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_err());
}

#[test]
fn test_left() {
    let tokens = vec![
        dummy_token(TokenKind::Plus, 0, 1),
        dummy_token(TokenKind::Minus, 1, 1),
    ];

    let parser = left(
        match_token(TokenKind::Plus),
        match_token(TokenKind::Minus),
    );

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_ok());
    let (_, output, span) = result.unwrap();
    assert_eq!(output, ());
    assert_eq!(span, 0..2);
}

#[test]
fn test_right() {
    let tokens = vec![
        dummy_token(TokenKind::Plus, 0, 1),
        dummy_token(TokenKind::Minus, 1, 1),
    ];

    let parser = right(
        match_token(TokenKind::Plus),
        match_token(TokenKind::Minus),
    );

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_ok());
    let (_, output, span) = result.unwrap();
    assert_eq!(output, ());
    assert_eq!(span, 0..2);
}

#[test]
fn test_map() {
    let tokens = vec![dummy_token(TokenKind::Plus, 0, 1)];
    let parser = match_token(TokenKind::Plus).map(|_, _| "mapped");

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_ok());
    let (_, output, span) = result.unwrap();
    assert_eq!(output, "mapped");
    assert_eq!(span, 0..1);
}

#[test]
fn test_not() {
    let tokens = vec![dummy_token(TokenKind::Minus, 0, 1)];
    let parser = not(match_token(TokenKind::Plus));

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_ok());
    let (_, output, span) = result.unwrap();
    assert_eq!(output, ());
    assert_eq!(span, 0..0);
}

#[test]
fn test_not_with_match() {
    let tokens = vec![dummy_token(TokenKind::Plus, 0, 1)];
    let parser = not(match_token(TokenKind::Plus));

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_err());
}

#[test]
fn test_surrounded() {
    let tokens = vec![
        dummy_token(TokenKind::LeftParen, 0, 1),
        dummy_token(TokenKind::Plus, 1, 1),
        dummy_token(TokenKind::RightParen, 2, 1),
    ];
    let parser = surrounded(
        match_token(TokenKind::LeftParen),
        match_token(TokenKind::Plus),
        match_token(TokenKind::RightParen),
    );

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_ok());
    let (_, output, span) = result.unwrap();
    assert_eq!(output, ());
    assert_eq!(span, 0..3);
}

#[test]
fn test_surrounded_missing_closing() {
    let tokens = vec![
        dummy_token(TokenKind::LeftParen, 0, 1),
        dummy_token(TokenKind::Plus, 1, 1),
    ];
    let parser = surrounded(
        match_token(TokenKind::LeftParen),
        match_token(TokenKind::Plus),
        match_token(TokenKind::RightParen),
    );

    let input = dummy_token_stream(&tokens);

    let result = parser.parse_next(input);
    assert!(result.is_err());
}