#[cfg(test)]
mod tests {
    use crate::jace_file::JaceFile;
    use crate::lexer::lexer::Lexer;
    use crate::lexer::prelude::Token;
    use crate::parser::ast::{BinOperator, Expr, Identifier};
    use crate::parser::expr::parse_bin_op;
    use crate::parser::prelude::TokenStream;
    use crate::parser::ptr::P;
    use crate::parser::parser::Parser;

    pub fn lex_tokens_into_stream(code: &'static str) -> Vec<Token> {
        let jcf = JaceFile::new("test.jc", code);

        let mut lexer = Lexer::new(jcf).into_iter();
        lexer.filter_map(|t| t.ok()).collect()
    }

    #[test]
    pub fn test_bin_expr1() {
        let code = "a + b - c";
        let expected = Ok((Expr::BinOpExpr(
            BinOperator::Plus,
            P(Expr::IdentExpr(Identifier("a".to_string()), 0..1)),
            P(Expr::IdentExpr(Identifier("b".to_string()), 4..5)),
            0..7), 0..7));

        let toks = lex_tokens_into_stream(code);
        let ast = parse_bin_op(0).parse_next(&mut TokenStream::new(&toks));

        println!("{:?}", toks);
        println!("{:?}", ast);

        assert_eq!(expected, ast);
    }
}