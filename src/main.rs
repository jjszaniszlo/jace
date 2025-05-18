mod cli;
mod err;
mod jace_file;
mod lexer;
mod parser;
mod typecheck;
mod codegen;

use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;

use clap::Parser;
use cli::{Cli, Command};
use codegen::emitter;
use miette::Report;
use jace_file::JaceFile;
use crate::lexer::prelude::*;

use crate::codegen::emitter::*;

fn main() {
    let args = Cli::parse();
    
    match args.command {
       Command::Run { path } => compiler_pipeline(path),
       Command::Build { path } => println!("Building: {path:?}"),
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

        let jcf = JaceFile::new("foo", &buf);
        let mut lexer = Lexer::new(jcf).into_iter();
        let toks = lexer
            .filter_map(|t| t.ok())
            .collect::<Vec<Token>>();

        match parser::parse(&toks, jcf) {
            Ok(m) => {
                print!("Parsed: {m:#?}");
                let mut emitter = LuaEmitter::new();
                let lua_code = m.accept(&mut emitter);

                // create a new file with the same name but ending in .lua in the same directory
                let mut new_path = path.clone();
                new_path.set_extension("lua");
                let mut f = err::error_maybe(
                    File::create(new_path),
                    "File Error".to_string());
                let _write = err::error_maybe(
                    f.write_all(lua_code.as_bytes()),
                    "Write Error".to_string());
            },
            Err(e) => todo!(),
        }


    } else if path.is_dir() {
        println!("Opening dir {path:?}");
    } 
}
