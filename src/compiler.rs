use thiserror::Error;

use crate::{
    chunk::Chunk,
    scanner::{self, Scanner, Token, TokenType},
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

    fn advance(&mut self, scanner: &'source mut Scanner) {
        self.previous = self.current.clone();

        for token in scanner {
            self.current = token;

            if self.current.kind != TokenType::Error {
                break;
            }
        }
    }
}

#[derive(Debug, Error)]
pub enum CompilerError {}
