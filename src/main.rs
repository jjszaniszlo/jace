mod lexer;
mod cli;
mod err;
mod jace_file;

use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use clap::Parser;
use cli::Cli;
use cli::Command;
use jace_file::JaceFile;
use lexer::Lexer;
use lexer::LexerIterator;

fn main() {
    //let args = Cli::parse();
    
    //match args.command {
    //    Command::Run { path } => run(path),
    //    Command::Build { path } => println!("Building: {path:?}"),
    //}

    let jcf = JaceFile::new("test.jc", 
        r#"
            class Equal a ::
                (==) :: a, a => Bool
                (!=) :: a, a => Bool
                (>=) :: a, a => Bool
                (<=) :: a, a => Bool
                (+)  :: a, a => a
                (-)  :: a, a => a 
                (*)  :: a, a => a 
                (/)  :: a, a => a 

            def doOperation::Integer,Integer=>Integer
                a,b=>a+b*(2^5)-2.5+3.6,7

            def main::()
                @
                print "Hello World!"!
                john := {name="John", age = 21}
                john.name
        "#);

    let mut lexer = Lexer::new(jcf).into_iter();

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
