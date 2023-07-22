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

    pub fn complie(&self, line: &str) -> Result<Chunk, CompilerError> {
        let scanner = Scanner::new(line);
        todo!()
    }
}

#[derive(Debug, Error)]
pub enum CompilerError {}
