mod cli;
mod err;
mod jace_file;
mod lexer;
mod parser;

use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use clap::Parser;
use cli::Cli;
use cli::Command;
use jace_file::JaceFile;
use crate::lexer::prelude::*;

fn main() {
    //let args = Cli::parse();
    
    //match args.command {
    //    Command::Run { path } => run(path),
    //    Command::Build { path } => println!("Building: {path:?}"),
    //}

    let jcf = JaceFile::new("test.jc", 
        r#"
            def map :: (a => b), [a] => [b]
            where
                a : Number
                b : Number
            in
            case
                _, {} => {}
                f, {x:xs} => f x! : map f xs!

            type Number :: Float | Integer

            def sum :: a, a, a, a => a
            where
                a : Number
            in
                a, b, c, d => let
                    inter := a + b
                    inter2 := c + d
                in inter + inter2

            def complexOperation :: Number, Number, Number => Number
                a, b, c => a^2+a*b+2^2*5^2/57*56

            def multi_return :: Number, Number, Number, Number => Number
                a, b, c, d => a*5+b*7,c*3+d*4

            type Foo ::
                bar : String
                baz : Number

            const MATH_PI :: 3.14

            class Equal a ::
                (==) :: a, a => Bool

            def main :: ()
                foo := {bar = "FooBaz", baz = 67}
                multi1,multi2 := multi_return 1 2 3 4!
                {m1, m2} := multi_return 5 6 7 8!
                arr_ret := {multi_return 5 6 7 8!}
                arr_ret_x_2 := map (a => a * 2) arr_ret!

            1 +
        "#);

    let mut lexer = Lexer::new(jcf).into_iter();
    let toks: Vec<Token> = lexer
        .filter_map(|t| t.ok())
        .collect();
    println!("{toks:#?}");

    match parser::parse(&toks) {
        Ok((r, t)) => println!("{t:#?}"),
        Err(e) => println!("{e:?}")
    }
}

fn compiler_pipeline(path: PathBuf) {
    if path.is_file() {
        let mut f = err::error_maybe(
            File::open(path.clone()),
            format!("File Error '{:?}'", path));

        let mut buf = String::new();

        let _read = err::error_maybe(
            f.read_to_string(&mut buf),
            format!("Read Error"));

        let file_name = path.file_stem().unwrap().to_str().unwrap();

    } else if path.is_dir() {
        println!("Opening dir {path:?}");
    } 
}
