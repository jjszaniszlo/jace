mod lexer;
mod parser;

fn main() {
    unsafe { backtrace_on_stack_overflow::enable() };

    let toks = lexer::tokenize_into_vec_no_positions(concat!(
        "a, b => case\n",
        "   0, n => n\n",
        "   n, 0 => n\n",
        "   0, 0 => (a, b => a) a b\n",
    )).unwrap();
    //let toks = lexer::tokenize_into_vec_no_positions(concat!(
    //    "{john = \"John\", age = 21, init = age, name => {age = age, name = name}}",
    //)).unwrap();
    println!("{:?}", toks);

    let result = parser::parser::parse(&toks);
    println!("{:#?}", result);
}
