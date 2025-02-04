mod lexer;
mod parser;

fn main() {
    let toks = lexer::tokenize_into_vec_no_positions(concat!(
        "(a, b => a + b)",
    )).unwrap();
    //let toks = lexer::tokenize_into_vec_no_positions(concat!(\n
    //    "{john = \"John\", age = 21, init = age, name => {age = age, name = name}}",
    //)).unwrap();
    println!("{:?}", toks);

    let result = parser::parser::parse(&toks);
    println!("{:#?}", result);
}
