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
        let scanner = Scanner::init(line);
        todo!()
    }
}

#[derive(Debug, Error)]
pub enum CompilerError {}
