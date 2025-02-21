mod cli;
mod err;
mod jace_file;
mod lexer;
mod parser;

mod typecheck;

use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use clap::Parser;
use miette::Report;
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
                            def main :: ()
                                a := -(-2 + 5) >>=
                            "#);

    let mut lexer = Lexer::new(jcf).into_iter();
    let toks: Vec<Token> = lexer
        .filter_map(|t| t.ok())
        .collect();

    println!("{toks:?}");

    //match parser::parse(&toks, jcf) {
    //    Ok(m) => println!("{m:#?}"),
    //    Err(e) => {
    //        let err = Report::from(e);
    //        println!("{err:?}");
    //    },
    //}
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
