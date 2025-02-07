use std::path::PathBuf;

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "jace")]
#[command(about = "A programming language based on Haskell.")]
#[command(version, about, long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Subcommand)]
pub enum Command {
    Run {
        #[arg(required = true)]
        path: PathBuf,
    },
    Build {
        #[arg(required = true)]
        path: PathBuf,
    }
}
