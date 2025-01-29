mod lexer;
mod parser;

fn main() {
    let tokens = lexer::tokenize_into_vec(concat!(
        "factorial :: Integer => Integer\n",
        "factorial :: case\n",
        "\t0 => 1\n",
        "\tn => n * factorial (n - 1)\n",
    ));
    println!("{:?}", tokens.unwrap());
}
