use crate::lexer::{lex_tokenize, tokenize_into_vec, Token};

macro_rules! lexer_test {
    (FAIL: $name:ident, $func:ident, $src:expr) => {
        #[cfg(test)]
        #[test]
        fn $name() {
            let got = $func($src);
            assert!(got.is_err(), "{:?} should error but did not!", got);
        }
    };
    ($name:ident, $func:ident, $src:expr => $should_be:expr) => {  
        #[cfg(test)]
        #[test]
        fn $name() {
            let (got, _bytes_read) = $func($src).unwrap();
            assert_eq!(got, $should_be, "Got {:?} but should be {:?}", $src, $should_be);
        }
    };
}

// identifier tests
lexer_test!(lex_identifier_single_letter, lex_tokenize, "F" => Token::from("F"));
lexer_test!(lex_identifier_underscore_first, lex_tokenize, "_nice" => Token::from("_nice"));
lexer_test!(lex_identifier_underscore_throughout, lex_tokenize, "_n_i_c_e" => Token::from("_n_i_c_e"));
lexer_test!(lex_identifier_number, lex_tokenize, "nice4" => Token::from("nice4"));
lexer_test!(FAIL: lex_identifier_cant_start_with_number, lex_tokenize, "1wow");
lexer_test!(lex_type_keyword, lex_tokenize, "type" => Token::TypeKeyword);
lexer_test!(lex_class_keyword, lex_tokenize, "class" => Token::ClassKeyword);
lexer_test!(lex_instance_keyword, lex_tokenize, "instance" => Token::InstanceKeyword);
lexer_test!(lex_case_keyword, lex_tokenize, "case" => Token::CaseKeyword);
lexer_test!(lex_if_keyword, lex_tokenize, "if" => Token::IfKeyword);
lexer_test!(lex_then_keyword, lex_tokenize, "then" => Token::ThenKeyword);
lexer_test!(lex_else_keyword, lex_tokenize, "else" => Token::ElseKeyword);

// number tests
lexer_test!(lex_integer, lex_tokenize, "123" => Token::from(123));
lexer_test!(lex_float, lex_tokenize, "1.23" => Token::from(1.23));
lexer_test!(lex_float2, lex_tokenize, "0.56" => Token::from(0.56));
lexer_test!(lex_float3, lex_tokenize, "1000.35" => Token::from(1000.35));
lexer_test!(lex_float4, lex_tokenize, "1432.356161" => Token::from(1432.356161));
lexer_test!(FAIL:lex_malformed_float, lex_tokenize, "001.23");
lexer_test!(FAIL:lex_malformed_float2, lex_tokenize, "1000.a");
lexer_test!(FAIL:lex_malformed_float3, lex_tokenize, "5.a44f");
lexer_test!(FAIL:lex_malformed_integer, lex_tokenize, "0123");
lexer_test!(FAIL:lex_malformed_integer2, lex_tokenize, "0000123");
lexer_test!(FAIL:lex_malformed_integer3, lex_tokenize, "1a23");

// symbol tests
lexer_test!(lex_equals, lex_tokenize, "=" => Token::Equals);
lexer_test!(lex_infered_equals, lex_tokenize, ":=" => Token::InferredEquals);
lexer_test!(lex_equals_equals, lex_tokenize, "==" => Token::EqualsEquals);
lexer_test!(lex_not_equals, lex_tokenize, "!=" => Token::NotEquals);
lexer_test!(lex_and, lex_tokenize, "&&" => Token::And);
lexer_test!(lex_or, lex_tokenize, "||" => Token::Or);
lexer_test!(lex_greater_than, lex_tokenize, ">" => Token::GreaterThan);
lexer_test!(lex_less_than, lex_tokenize, "<" => Token::LessThan);
lexer_test!(lex_greater_equal_than, lex_tokenize, ">=" => Token::GreaterEqualThan);
lexer_test!(lex_less_equal_than, lex_tokenize, "<=" => Token::LessEqualThan);
lexer_test!(lex_colon, lex_tokenize, ":" => Token::Colon);
lexer_test!(lex_colon_colon, lex_tokenize, "::" => Token::ColonColon);
lexer_test!(lex_fat_arrow, lex_tokenize, "=>" => Token::FatArrow);
lexer_test!(lex_comma, lex_tokenize, "," => Token::Comma);
lexer_test!(lex_left_paren, lex_tokenize, "(" => Token::LeftParen);
lexer_test!(lex_right_paren, lex_tokenize, ")" => Token::RightParen);
lexer_test!(lex_left_brace, lex_tokenize, "{" => Token::LeftBrace);
lexer_test!(lex_right_brace, lex_tokenize, "}" => Token::RightBrace);
lexer_test!(lex_left_bracket, lex_tokenize, "[" => Token::LeftBracket);
lexer_test!(lex_right_bracket, lex_tokenize, "]" => Token::RightBracket);
lexer_test!(lex_union, lex_tokenize, "|" => Token::Union);
lexer_test!(lex_plus, lex_tokenize, "+" => Token::Plus);
lexer_test!(lex_minus, lex_tokenize, "-" => Token::Minus);
lexer_test!(lex_divide, lex_tokenize, "/" => Token::Divide);
lexer_test!(lex_multiply, lex_tokenize, "*" => Token::Multiply);

// these are to test that it lexes only a single one of these specific symbols.
lexer_test!(lex_multi_left_paren, lex_tokenize, "(((((" => Token::LeftParen);
lexer_test!(lex_multi_right_paren, lex_tokenize, ")))))" => Token::RightParen);
lexer_test!(lex_multi_left_brace, lex_tokenize, "{{{{" => Token::LeftBrace);
lexer_test!(lex_multi_right_brace, lex_tokenize, "}}}" => Token::RightBrace);
lexer_test!(lex_multi_left_bracket, lex_tokenize, "[[[" => Token::LeftBracket);
lexer_test!(lex_multi_plus, lex_tokenize, "+++" => Token::Plus);
lexer_test!(lex_multi_minus, lex_tokenize, "---" => Token::Minus);
lexer_test!(lex_multi_divide, lex_tokenize, "///" => Token::Divide);
lexer_test!(lex_multi_multiply, lex_tokenize, "***" => Token::Multiply);

// these are not valid symbols.  Tests invalid union symbols too.
lexer_test!(FAIL:lex_invalid, lex_tokenize, "$");
lexer_test!(FAIL:lex_invalid1, lex_tokenize, "^");
lexer_test!(FAIL:lex_invalid2, lex_tokenize, "#");
lexer_test!(FAIL:lex_invalid3, lex_tokenize, "===>");
lexer_test!(FAIL:lex_invalid4, lex_tokenize, "===");
lexer_test!(FAIL:lex_invalid5, lex_tokenize, "!===");
lexer_test!(FAIL:lex_invalid6, lex_tokenize, "%%");
lexer_test!(FAIL:lex_invalid7, lex_tokenize, ">>");
lexer_test!(FAIL:lex_invalid8, lex_tokenize, "<<");

// string tests.  For now, only single line.
lexer_test!(lex_test_string, lex_tokenize, "\"Test\"" => Token::String("Test".to_string()));
lexer_test!(FAIL:lex_random_string, lex_tokenize, "\"asd\n");
lexer_test!(FAIL:lex_random_string1, lex_tokenize, "\"fasga ashasd asd\r");
lexer_test!(FAIL:lex_random_string2, lex_tokenize, "\"asda asd asd \nasd asdasdasd\nasdasd as dasd\"");
lexer_test!(lex_tab_string, lex_tokenize, "\"tab\ttest\"" => Token::String("tab\ttest".to_string()));
lexer_test!(lex_tab_string1, lex_tokenize, "\"Totally a valid string!\t:)\"" => Token::String("Totally a valid string!\t:)".to_string()));
lexer_test!(lex_hello_world_string, lex_tokenize, "\"Hello World!\"" => Token::String("Hello World!".to_string()));
lexer_test!(lex_ecaped_multiline_string, lex_tokenize, "\"Here is a multiline\\n string escaped.\"" => Token::String("Here is a multiline\\n string escaped.".to_string()));
lexer_test!(lex_symbol_after_string, lex_tokenize, "\"Hi!\"," => Token::String("Hi!".to_string()));

// full lexer tests.
#[cfg(test)]
#[test]
fn tokenize_function_factorial() {
    use crate::lexer::tokenize_into_vec_no_positions;

    let src = concat!(
        "factorial :: Integer => Integer\n",
        "factorial :: case\n",
        "\t0 => 1\n",
        "\tn => n * factorial (n - 1)\n",
    );
    let should_be = vec![
        Token::from("factorial"),
        Token::ColonColon,
        Token::from("Integer"),
        Token::FatArrow,
        Token::from("Integer"),
        Token::from("factorial"),
        Token::ColonColon,
        Token::CaseKeyword,
        Token::from(0),
        Token::FatArrow,
        Token::from(1),
        Token::from("n"),
        Token::FatArrow,
        Token::from("n"),
        Token::Multiply,
        Token::from("factorial"),
        Token::LeftParen,
        Token::from("n"),
        Token::Minus,
        Token::from(1),
        Token::RightParen,
    ];
    assert_eq!(tokenize_into_vec_no_positions(src).unwrap(), should_be);
}
