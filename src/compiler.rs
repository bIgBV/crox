use thiserror::Error;

use crate::{
    chunk::Chunk,
    scanner::{self, Scanner},
};

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn complie(&self, source: &str) -> Result<Chunk, CompilerError> {
        let scanner = Scanner::init(source);

        let mut line = 0;

        for token in scanner {
            if token.line != line {
                println!("{:>4}", token.line);
                line = token.line;
            } else {
                println!("   | ");
            }

            println!("{:#}", token);
        }

        Ok(Chunk::new("test"))
    }
}

#[derive(Debug, Error)]
pub enum CompilerError {}
