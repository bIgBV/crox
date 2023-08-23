use thiserror::Error;

use crate::{
    chunk::Chunk,
    scanner::{self, ScanError, Scanner, Token, TokenType},
};

pub struct Compiler<'source> {
    previous: Token<'source>,
    current: Token<'source>,
}

impl<'source> Compiler<'source> {
    pub fn new() -> Self {
        Self {
            previous: Token::empty(),
            current: Token::empty(),
        }
    }

    pub fn complie(&self, source: &'source str) -> Result<Chunk, CompilerError> {
        let scanner = Scanner::init(source);
        let chunk = Chunk::new("another-one");

        Ok(chunk)
    }

    fn advance(&mut self, scanner: &'source mut Scanner) -> Result<(), CompilerError> {
        self.previous = self.current.clone();

        for token in scanner {
            self.current = token?;
        }

        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum CompilerError {
    #[error("Encountered a scanner error: {0}")]
    Scanner(#[from] ScanError),
}
