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
use crate::parser::prelude::*;

fn main() {
    //let args = Cli::parse();
    
    //match args.command {
    //    Command::Run { path } => run(path),
    //    Command::Build { path } => println!("Building: {path:?}"),
    //}

    let jcf = JaceFile::new("test.jc", 
        r#"
            type Result a b :: Ok(a) | Err(b)
            type Bool :: True | False

            type Option a :: Some(a) | None

            type ContextError a :: SomethingWentWrong(a) | NoContextError

            const MATH_PI :: 3.14

            type Person ::
                name : String
                age : Integer

            -- class Equal a b c ::
            --    (==) :: a, a => Bool
            --    (==) :: a, b => Bool
            --    (==) :: a, c => Bool

            -- instance Equal Person Integer String ::
            --    (==) :: x, y => x.name == y.name && x.age == y.age
            --    (==) :: x, i => x.age == i
            --    (==) :: x, s => x.name == s

            def result_default :: a, a => a
            where
                a : Equal + Add + Multiply
            in case
                Ok(v), _ => let
                        calc := v ^ 2
                    in case calc
                        0 => calc + 1
                        1 => (a => a*2+calc^5*6)
                        3 => 5*2, 3, 6, a*b
                Err(e), v => v

            def main :: ()
                print "hello world!"!
                x := case y
                    1 => ident 2!
                ident2 := ident3
        "#);

    let mut lexer = Lexer::new(jcf).into_iter();
    let toks: Vec<Token> = lexer
        .filter_map(|t| t.ok())
        .collect();

    println!("{toks:?}");

    match parser::parse(&toks) {
        Ok((r, t, _)) => println!("{t:#?}"),
        Err(e) =>
            println!("{:?}", e.with_source_code(jcf.contents())),
        _ => {},
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
