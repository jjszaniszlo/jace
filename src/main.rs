mod lexer;
mod parser;

fn main() {
    unsafe { backtrace_on_stack_overflow::enable() };

    let toks = lexer::tokenize_into_vec_no_positions(concat!(
        "{name = \"John\"}"
    )).unwrap();
    println!("{:?}", toks);

    let result = parser::parser::parse(&toks);
    println!("{:#?}", result);
}
