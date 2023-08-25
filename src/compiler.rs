use std::sync::Arc;

use arc_swap::{access::Access, ArcSwapOption};
use miette::Diagnostic;
use thiserror::Error;

use crate::{
    chunk::Chunk,
    repl::report_error,
    scanner::{ScanError, Scanner, Token},
};

pub struct Compiler<'source> {
    previous: Token<'source>,
    current: Token<'source>,
    source: ArcSwapOption<String>,
}

impl<'source> Compiler<'source> {
    pub fn new() -> Self {
        Self {
            previous: Token::empty(),
            current: Token::empty(),
            source: ArcSwapOption::empty(),
        }
    }

    pub fn complie(&mut self, source: &'source str) -> Result<Chunk, CompilerError> {
        // Store the source before begining compilation.
        self.source.store(Some(Arc::new(source.to_string())));

        let mut scanner = Scanner::init(source);
        let chunk = Chunk::new("another-one");
        self.advance(&mut scanner)?;
        //self.expression()?;

        Ok(chunk)
    }

    fn advance(&mut self, scanner: &mut Scanner) -> Result<(), CompilerError> {
        self.previous = self.current.clone();

        for token in scanner {
            if token.is_ok() {
                break;
            }

            report_error(
                &(&*self.source.load().as_ref().unwrap()),
                &token.err().unwrap(),
            );
        }
        Ok(())
    }

    fn expression(&self) -> Result<(), CompilerError> {
        todo!()
    }
}

#[derive(Error, Debug, Diagnostic)]
pub enum CompilerError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Scanner(#[from] ScanError),
}
