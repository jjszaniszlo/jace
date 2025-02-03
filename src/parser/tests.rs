use super::{ast, combinator::*, parser::*};
use crate::lexer::token::Token;

#[test]
fn test_parse_set_literal_with_valid_commas() {
    let toks = vec![
        Token::LeftBrace,
        Token::from("name"),
        Token::Equals,
        Token::String("Alice".to_string()),
        Token::Comma,
        Token::from("age"),
        Token::Equals,
        Token::Integer(30),
        Token::RightBrace,
    ];

    let (_, result) = parse_set_literal(&toks).unwrap();

    assert_eq!(
        ast::Literal::Set(vec![
            (ast::Identifier::from("name"), ast::Expr::Literal(ast::Literal::String("Alice".to_string()))),
            (ast::Identifier::from("age"), ast::Expr::Literal(ast::Literal::from(30))),
        ]),
        result
    );
}

#[test]
fn test_parse_set_literal_with_no_trailing_comma() {
    let toks = vec![
        Token::LeftBrace,
        Token::from("name"),
        Token::Equals,
        Token::String("Bob".to_string()),
        Token::Comma,
        Token::from("age"),
        Token::Equals,
        Token::Integer(40),
        Token::RightBrace,
    ];

    let (_, result) = parse_set_literal(&toks).unwrap();
    assert_eq!(
        ast::Literal::Set(vec![
            (ast::Identifier::from("name"), ast::Expr::Literal(ast::Literal::String("Bob".to_string()))),
            (ast::Identifier::from("age"), ast::Expr::Literal(ast::Literal::from(40))),
        ]),
        result
    );
}

#[test]
fn test_parse_set_literal_rejects_trailing_comma() {
    let toks = vec![
        Token::LeftBrace,
        Token::from("name"),
        Token::Equals,
        Token::String("Charlie".to_string()),
        Token::Comma,
        Token::RightBrace, // Trailing comma should cause a failure
    ];

    assert!(parse_set_literal(&toks).is_err());
}

#[test]
fn test_match_token() {
    let toks = vec![Token::from("let"), Token::from("foo")];

    // Should match the "let" token and consume it
    let (_, result) = match_token(Token::from("let")).parse(&toks).unwrap();
    assert_eq!(result, ());
}

#[test]
fn test_match_token_fail() {
    let toks = vec![Token::from("foo")];

    // Should fail because "foo" is not "let"
    assert!(match_token(Token::from("let")).parse(&toks).is_err());
}

#[test]
fn test_match_literal_integer() {
    let toks = vec![Token::Integer(42), Token::from("foo")];

    // Should parse an integer literal
    let (_, result) = match_literal(&toks).unwrap();
    assert_eq!(result, ast::Literal::from(42));
}

#[test]
fn test_match_literal_string() {
    let toks = vec![Token::String("hello".to_string()), Token::from("foo")];

    // Should parse a string literal
    let (_, result) = match_literal(&toks).unwrap();
    assert_eq!(result, ast::Literal::from("hello".to_string()));
}

#[test]
fn test_match_literal_fail() {
    let toks = vec![Token::from("foo")];

    // Should fail because it's not a recognized literal
    assert!(match_literal(&toks).is_err());
}

#[test]
fn test_match_identifier() {
    let toks = vec![Token::Identifier("x".to_string()), Token::from("foo")];

    // Should parse an identifier token
    let (_, result) = match_identifier(&toks).unwrap();
    assert_eq!(result, ast::Identifier::from("x"));
}

#[test]
fn test_pair_parsers() {
    let toks = vec![Token::from("let"), Token::Identifier("x".to_string())];

    let parser = pair(
        match_token(Token::from("let")),
        match_identifier,
    );

    // Should parse "let" followed by an identifier
    let (_, (t1, t2)) = parser.parse(&toks).unwrap();
    assert_eq!(t1, ());
    assert_eq!(t2, ast::Identifier::from("x"));
}

#[test]
fn test_or_parser() {
    let toks1 = vec![Token::Integer(42)];
    let toks2 = vec![Token::Identifier("x".to_string())];

    let parser = or(
        match_literal.map(|l| ast::Expr::Literal(l)),
        match_identifier.map(|i| ast::Expr::Identifier(i)),
    );

    // Should match the literal in toks1
    let (_, result1) = parser.parse(&toks1).unwrap();
    assert_eq!(result1, ast::Expr::Literal(ast::Literal::from(42)));

    // Should match the identifier in toks2
    let (_, result2) = parser.parse(&toks2).unwrap();
    assert_eq!(result2, ast::Expr::Identifier(ast::Identifier::from("x")));
}

#[test]
fn test_zero_or_more() {
    let toks = vec![
        Token::Integer(1),
        Token::Integer(2),
        Token::Integer(3),
    ];

    let parser = zero_or_more(match_literal.map(|l| ast::Literal::from(l)));

    // Should parse all integers in sequence
    let (_, result) = parser.parse(&toks).unwrap();
    assert_eq!(result, vec![
        ast::Literal::from(1),
        ast::Literal::from(2),
        ast::Literal::from(3)
    ]);
}

#[test]
fn test_zero_or_one() {
    let toks1 = vec![Token::Integer(10)];
    let toks2 = vec![Token::from("foo")]; // No integer token

    let parser = zero_or_one(match_literal);

    // Should match one integer
    let (_, result1) = parser.parse(&toks1).unwrap();
    assert_eq!(result1, Some(ast::Literal::from(10)));

    // Should match nothing
    let (_, result2) = parser.parse(&toks2).unwrap();
    assert_eq!(result2, None);
}

#[test]
fn test_one_or_more() {
    let toks = vec![
        Token::Integer(1),
        Token::Integer(2),
        Token::Integer(3),
    ];

    let parser = one_or_more(match_literal.map(|l| ast::Literal::from(l)));

    // Should parse all integers in sequence
    let (_, result) = parser.parse(&toks).unwrap();
    assert_eq!(result, vec![
        ast::Literal::from(1),
        ast::Literal::from(2),
        ast::Literal::from(3)
    ]);
}

#[test]
fn test_parse_set_literal() {
    let toks = vec![
        Token::LeftBrace,
        Token::from("name"),
        Token::Equals,
        Token::String("Alice".to_string()),
        Token::Comma,
        Token::from("age"),
        Token::Equals,
        Token::Integer(30),
        Token::RightBrace,
    ];

    let (_, result) = parse_set_literal(&toks).unwrap();

    assert_eq!(
        result,
        ast::Literal::Set(vec![
            (ast::Identifier::from("name"), ast::Expr::Literal(ast::Literal::String("Alice".to_string()))),
            (ast::Identifier::from("age"), ast::Expr::Literal(ast::Literal::from(30))),
        ])
    );
}

#[test]
fn test_parse_type_definition() {
    let toks = vec![
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
        result,
        ast::Def::TypeDef(
            ast::TypeName::from("Person"),
            vec![
                (ast::Identifier::from("name"), ast::TypeName::from("String")),
                (ast::Identifier::from("age"), ast::TypeName::from("Integer")),
            ]
        )
    );
}

#[test]
fn test_parse_statement_inferred_assignment() {
    let toks = vec![
        Token::from("num"),
        Token::InferredEquals,
        Token::from(10)
    ];

    let (_, result) = parse_statement().parse(&toks).unwrap();

    assert_eq!(
        result,
        ast::Stmt::Asmt(
            ast::Identifier::from("num"),
            None,
            ast::Expr::Literal(ast::Literal::from(10))
        )
    );
}

#[test]
fn test_parse_statement_type_assignment() {
    let toks = vec![
        Token::from("num"),
        Token::Colon,
        Token::from("Integer"),
        Token::Equals,
        Token::from(10)
    ];

    let (_, result) = parse_statement().parse(&toks).unwrap();

    assert_eq!(
        result,
        ast::Stmt::Asmt(
            ast::Identifier::from("num"),
            Some(ast::TypeName::from("Integer")),
            ast::Expr::Literal(ast::Literal::from(10))
        )
    );
}

