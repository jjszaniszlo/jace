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
            type Result a b :: (Ok a) | (Err b)
            type Bool :: True | False

            type Option a :: (Some a) | None

            type ContextError a :: (SomethingWentWrong a) | NoContextError

            const MATH_PI :: 3.14

            class Monad m ::
                bind :: (m a), (a => (m b)) => (m b)
                return :: a => (m a)

            instance Monad Option ::
                bind :: case
                    None, _ => None;
                    (Some x), f => f x;
                return :: a => Some a;

            type Sheep ::
                father : (Option Sheep)
                mother : (Option Sheep)

            def father :: Sheep => (Option Sheep)
                s => s.father;

            def mother :: Sheep => (Option Sheep)
                s => s.mother;

            def materialGrandFather :: Sheep => (Option Sheep)
                s => bind (bind (return s) mother) father;

            def paternalGrandFather :: Sheep => (Option Sheep)
                s => bind (bind (return s) father) father;

            type List a :: Nil | (Cons a (List a))

            def join :: (List (List a)) => (List a)
            case
                Nil => Nil;
                (Cons xs xss) => cat xs (join xss);

            def cat :: (List a), (List a) => (List a)
            case
                Nil, ls => ls;
                (Cons x xs), ys => Cons x (cat xs ys);

            def somthin :: a, b => ()
                a, b => print a b;

            def main :: ()
                list := Cons 5 (Cons 10 Nil);
                a,b := 1+2*3^5, 6*7^5/2;
                c,d : Integer, Integer = 1+2,3*6;
                e,f := Cons 5, Cons (Cons (2*2) Nil);

                grandpa := { father = None, mother = None };
                father := { father = grandpa, mother = None };
                sheep := { father = father, mother = None };

                grandpa_result := paternalGrandFather sheep;

                foo := case grandpa_result
                    (Some x) => print "sheep has a grandpa!" x;
                    None => 10;;

                print "hello world!";
                my_proc;
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
