mod lexer;
mod parser;

fn main() {
    let toks = lexer::tokenize_into_vec_no_positions(concat!(
        "{name = \"John\" age = 21}"
    )).unwrap();
    println!("{:?}", toks);

    let result = parser::parser::parse(&toks);
    println!("{:#?}", result);
}
