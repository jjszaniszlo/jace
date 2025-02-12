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
            def main :: ()
                print 23ab
        "#);

    let mut lexer = Lexer::new(jcf).into_iter();
    let toks: Vec<Token> = lexer
        .filter_map(|t| t.ok())
        .collect();

    println!("{toks:#?}");

    match parser::parse(&toks) {
        Ok((r, t, _)) => println!("{t:#?}"),
        Err(ParserError::UnrecoverableError(e)) => 
            println!("{:?}", miette::Report::new(e).with_source_code(jcf.contents())),
        Err(ParserError::ContextualError(e)) =>
            println!("{:?}", miette::Report::new(e).with_source_code(jcf.contents())),
        Err(ParserError::InnerError(e)) =>
            println!("{:?}", miette::Report::new(e).with_source_code(jcf.contents())),
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
