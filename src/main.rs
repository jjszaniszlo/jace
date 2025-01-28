mod lexer;

fn main() {
    let (tok, bytes_read) = lexer::lex_identifier("case").unwrap();
    println!("{}, {}", tok, bytes_read);
}
