mod lexer;
mod parser;
mod cli;
mod err;

use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use clap::Parser;
use cli::Cli;
use cli::Command;

fn main() {
    let args = Cli::parse();
    
    match args.command {
        Command::Run { path } => run(path),
        Command::Build { path } => println!("Building: {path:?}"),
    }
}

fn run(path: PathBuf) {
    if path.is_file() {
        let mut f = err::error_maybe(
            File::open(path.clone()),
            format!("File Error '{:?}'", path));

        let mut buf = String::new();

        let _read = err::error_maybe(
            f.read_to_string(&mut buf),
            format!("Read Error"));

        let toks = 
            err::error_maybe(
            lexer::tokenize_into_vec_no_positions(buf.as_str()),
            format!("Lexer Error"));

        let (_, result) = 
            err::error_maybe(
            parser::parse(&toks),
            format!("Parse Error"));

        println!("{:#?}", result);
    } else if path.is_dir() {
        println!("Opening dir {path:?}");
    } 
}

    //let toks = lexer::tokenize_into_vec_no_positions(
    //    r#"
    //    type Person ::
    //        name : String
    //        age : Integer
    //
    //    class Equal Person a ::
    //        (==) :: a, a => Bool
    //        sameAge :: a, a => Bool
    //        sameName :: a, a => Bool
    //
    //    instance Equal Person {name, age}, {name2, age2} ::
    //        (==) => name == name2 && age == age2
    //        sameAge => age == age2
    //        sameName => name == name2
    //
    //    let
    //        john := {name = "john", age = 21}
    //        harry := {name = "harry", age = 21}
    //    in sameAge (a,b => a+b) (harry)
    //    "#
    //).unwrap();
    ////let toks = lexer::tokenize_into_vec_no_positions(concat!(\n
    ////    "{john = \"John\", age = 21, init = age, name => {age = age, name = name}}",
    ////)).unwrap();
    //println!("{:?}", toks);
    //
    //let (_, result) = parser::parse(&toks).unwrap();
    //println!("{:#?}", result);
