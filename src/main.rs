mod lexer;
mod parser;

fn main() {
    let lex_test = concat!(
        "map :: (a => b), {a} => {b}\n",
        "map :: case\n",
        "   _, {} => {}\n",
        "   func, {t:ts} => {func t} + (map func ts)\n"
    );

    let toks = lexer::tokenize_into_vec(&lex_test).unwrap();
    println!("{:?}", toks);

    // parser::test_ast2();
}
