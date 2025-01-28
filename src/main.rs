mod lexer;

fn main() {
    let (tok, bytes_read) = lexer::lex_identifier("hello").unwrap();
    println!("{}", tok);
}
