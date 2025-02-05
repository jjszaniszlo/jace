use super::*;
use crate::lexer::token::Token;
use crate::parser::ast::{ClassName, TypeName, MethodImpl, Identifier, MethodOperator};

fn identifier_token(name: &str) -> Token {
    Token::Identifier(name.to_string())
}

 #[test]
fn test_parse_instance_def_with_params() {
    let tokens = vec![
        Token::InstanceKeyword,
        identifier_token("Equal"),
        identifier_token("Person"),
        identifier_token("x"),
        identifier_token("y"),
        Token::ColonColon,
        identifier_token("wow"),
        Token::FatArrow,
        Token::Identifier("x".to_string()),
        Token::EqualsEquals,
        Token::Identifier("y".to_string()),
    ];

    let result = parse_instance_def().parse(&tokens);
    assert!(result.is_ok(), "Failed to parse instance definition with parameters");

    let (_, instance_def) = result.unwrap();
    match instance_def {
        ast::Def::InstanceDef(cls, typ, params, impls) => {
            assert_eq!(cls, ClassName(Identifier("Equal".to_string())));
            assert_eq!(typ, TypeName(Identifier("Person".to_string())));
            assert_eq!(params.len(), 2);
            assert_eq!(params[0], Identifier("x".to_string()));
            assert_eq!(params[1], Identifier("y".to_string()));
            assert_eq!(impls.len(), 1);
        }
        _ => panic!("Unexpected result for instance definition"),
    }
}

#[test]
fn test_parse_instance_method_impl_op() {
    let tokens = vec![
        Token::WrappedPlus,
        Token::FatArrow,
        Token::Integer(10),
    ];

    let result = parse_instance_method_impl_op().parse(&tokens);
    assert!(result.is_ok(), "Failed to parse instance method operator implementation");

    let (_, method_impl) = result.unwrap();
    match method_impl {
        MethodImpl::Operator(op, expr) => {
            assert_eq!(op, MethodOperator::Plus);
            match expr {
                ast::Expr::LitExpr(ast::Literal::Integer(i)) => assert_eq!(i, 10),
                _ => panic!("Unexpected expression in operator impl"),
            }
        }
        _ => panic!("Unexpected result for method impl"),
    }
}

#[test]
fn test_parse_instance_method_impl_named() {
    let tokens = vec![
        identifier_token("myMethod"),
        Token::FatArrow,
        Token::String("Hello".to_string()),
    ];

    let result = parse_instance_method_impl_named().parse(&tokens);
    assert!(result.is_ok(), "Failed to parse named instance method implementation");

    let (_, method_impl) = result.unwrap();
    match method_impl {
        MethodImpl::Named(Identifier(name), expr) => {
            assert_eq!(name, "myMethod".to_string());
            match expr {
                ast::Expr::LitExpr(ast::Literal::String(ref s)) => assert_eq!(s, "Hello"),
                _ => panic!("Unexpected expression in named method impl"),
            }
        }
        _ => panic!("Unexpected result for named method impl"),
    }
}

#[test]
fn test_parse_instance_header() {
    let tokens = vec![
        Token::InstanceKeyword,
        identifier_token("MyClass"),
        identifier_token("MyType"),
    ];

    let result = parse_instance_header().parse(&tokens);
    assert!(result.is_ok(), "Failed to parse instance header");

    let (_, (class_name, type_name)) = result.unwrap();
    assert_eq!(class_name, ClassName(Identifier("MyClass".to_string())));
    assert_eq!(type_name, TypeName(Identifier("MyType".to_string())));
}

#[test]
fn test_parse_instance_method_impls() {
    let tokens = vec![
        Token::WrappedPlus,
        Token::FatArrow,
        Token::Integer(5),
        identifier_token("myMethod"),
        Token::FatArrow,
        Token::String("Test".to_string()),
    ];

    let result = parse_instance_method_impls().parse(&tokens);
    assert!(result.is_ok(), "Failed to parse instance method implementations");

    let (_, impls) = result.unwrap();
    assert_eq!(impls.len(), 2);
}
