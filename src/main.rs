mod lexer;
mod parser;

fn main() {
    let toks = lexer::tokenize_into_vec_no_positions(concat!(
        "instance Equal Person {a,b}, y ::\n",
        "\twow => x == y\n",
        "1 + 1",
    )).unwrap();
    //let toks = lexer::tokenize_into_vec_no_positions(concat!(\n
    //    "{john = \"John\", age = 21, init = age, name => {age = age, name = name}}",
    //)).unwrap();
    println!("{:?}", toks);

    let (_, result) = parser::parse(&toks).unwrap();
    println!("{:#?}", result);
}
