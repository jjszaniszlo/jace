mod cli;
mod err;
mod jace_file;
mod lexer;
mod parser;
mod typecheck;
mod codegen;

use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use miette::Report;
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
                            type Option a :: Some a | None

                            -- type List a :: Nil | Cons a (List a)

                            -- def join :: List (List a) => List a
                            -- case
                            --     Nil => Nil;
                            --     Cons xs xss => cat xs (join xss);

                            -- def sum :: Integer, Integer => Integer
                            -- do
                            --     a, b => a * b

                            "#);

    let mut lexer = Lexer::new(jcf).into_iter();
    let toks: Vec<Token> = lexer
        .filter_map(|t| t.ok())
        .collect();

    println!("{toks:?}");

    match parser::parse(&toks, jcf) {
        Ok(m) => println!("{m:#?}"),
        Err(e) => {
            let err = Report::from(e);
            println!("{err:?}");
        },
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
            "Read Error".to_string());

        let file_name = path.file_stem().unwrap().to_str().unwrap();

    } else if path.is_dir() {
        println!("Opening dir {path:?}");
    } 
}
