use lexer::Lexer;

mod lexer;

fn main() {
    let program = "1 + 1";

    let lexer = Lexer::init(program.to_string());
}
