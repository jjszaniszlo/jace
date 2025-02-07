mod lexer;
mod parser;
mod cli;
mod err;

use codespan_reporting::files::SimpleFiles;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::sync::LazyLock;
use std::sync::RwLock;

use clap::Parser;
use cli::Cli;
use cli::Command;

pub static SOURCES: LazyLock<RwLock<SimpleFiles<String, String>>> = LazyLock::new(|| SimpleFiles::new().into());

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

        let file_name = path.file_stem().unwrap().to_str().unwrap();

        let id = SOURCES
            .write()
            .unwrap()
            .add(file_name.to_string(), buf);

        compile(id).expect("Could not compile!");
    } else if path.is_dir() {
        println!("Opening dir {path:?}");
    } 
}

fn compile(id: usize) -> Result<(), String> {
    Ok(())
}
