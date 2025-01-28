mod lexer;

fn main() {
    let tokens = lexer::tokenize_into_vec("print \"Hello World!\"");
    println!("{:?}", tokens.unwrap());
}
