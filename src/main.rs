mod lexer;
mod parser;

fn main() {
    let toks = lexer::tokenize_into_vec_no_positions(concat!(
        "let\n",
        "john := { name = \"John\" , age = 21 }\n",
        "foo := 9+2\n",
        "in john + foo\n"
    )).unwrap();
    //let toks = lexer::tokenize_into_vec_no_positions(concat!(\n
    //    "{john = \"John\", age = 21, init = age, name => {age = age, name = name}}",
    //)).unwrap();
    println!("{:?}", toks);

    let result = parser::parser::parse(&toks);
    println!("{:#?}", result);
}
