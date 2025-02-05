mod lexer;
mod parser;

fn main() {
    let toks = lexer::tokenize_into_vec_no_positions(
        r#"

        map :: [a], [b] => [c]
        map :: case
            _, {} => {}
            f, {x:xs} => {f x} + (map f xs)

        1
        "#
    ).unwrap();

    //let toks = lexer::tokenize_into_vec_no_positions(concat!(\n
    //    "{john = \"John\", age = 21, init = age, name => {age = age, name = name}}",
    //)).unwrap();
    println!("{:?}", toks);

    let (_, result) = parser::parse(&toks).unwrap();
    println!("{:#?}", result);
}
