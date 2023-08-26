use std::{num::ParseFloatError, sync::Arc};

use arc_swap::{access::Access, ArcSwap, ArcSwapOption};
use miette::Diagnostic;
use thiserror::Error;

use crate::{
    chunk::{self, Chunk, ChunkError, OpCode},
    repl::report_error,
    scanner::{ScanError, Scanner, Token, TokenType},
    value::Value,
};

pub struct Compiler<'source> {
    parser: Parser<'source>,
    source: ArcSwap<String>,
    pub had_error: bool,
}

struct Parser<'source> {
    pub previous: Token<'source>,
    pub current: Token<'source>,
    scanner: Scanner<'source>,
}

impl<'source> Parser<'source> {
    pub fn new() -> Self {
        Parser {
            previous: Token::empty(),
            current: Token::empty(),
            scanner: Scanner::init(""),
        }
    }

    fn update_scanner(&mut self, source: &'source str) {
        self.scanner = Scanner::init(source);
    }

    fn advance(&mut self) -> Result<(), CompilerError> {
        self.previous = self.current.clone();

        for token in &mut self.scanner {
            if token.is_ok() {
                break;
            }
        }
        Ok(())
    }

    fn consume(&mut self, kind: TokenType) -> Result<(), CompilerError> {
        if self.current.kind == kind {
            self.advance()?;
            Ok(())
        } else {
            Err(CompilerError::UnexpectedToken(
                self.current.kind.clone(),
                kind,
            ))
        }
    }
}

impl<'source> Compiler<'source> {
    pub fn new() -> Self {
        Self {
            parser: Parser::new(),
            source: ArcSwap::new(Arc::new(String::new())),
            had_error: false,
        }
    }

    pub fn complie(&mut self, source: &'source str) -> Result<Chunk, CompilerError> {
        // Store the source before begining compilation.
        self.source.store(Arc::new(source.to_string()));
        self.parser.update_scanner(source);

        let mut chunk = Chunk::new("another-one");

        if let Err(error) = self.parser.advance() {
            self.handle_error(error);
        }
        //self.expression()?;

        self.end_compiler(&mut chunk);
        // TODO how to surface the fact that there was an error _after_ finishing parsing the source?
        Ok(chunk)
    }

    fn end_compiler(&self, chunk: &mut Chunk) {
        self.emit_byte(chunk, OpCode::Return)
    }

    fn emit_byte(&self, chunk: &mut Chunk, opcode: OpCode) {
        chunk.write(opcode, self.parser.current.line)
    }

    fn expression(&mut self, chunk: &mut Chunk) -> Result<(), CompilerError> {
        // We simply parse the lowest precedence level, which subsumes all of the higher-precedence expressions too
        self.parse_precedence(Precedence::Assignment, chunk)?;

        Ok(())
    }

    fn number(&self, chunk: &mut Chunk) -> Result<(), CompilerError> {
        let source = self.source.load();
        let range =
            self.parser.previous.start..self.parser.previous.start + self.parser.previous.length;
        let value = &source[range]
            .parse::<f64>()
            .map_err(|err| Serialization::Numer(err))?;

        chunk.write_constant(*value, self.parser.previous.line)?;

        Ok(())
    }

    fn grouping(&mut self, chunk: &mut Chunk) -> Result<(), CompilerError> {
        // We should have already consumed the LeftParen
        self.expression(chunk)?;
        self.parser.consume(TokenType::RightParen)?;

        Ok(())
    }

    // We evaluate the operand first which leaves its value on the stack.
    // Then we pop that value, negate it, and push the result.
    fn unary(&mut self, chunk: &mut Chunk) -> Result<(), CompilerError> {
        let kind = self.parser.previous.kind;

        // Compile the operand
        self.parse_precedence(Precedence::Unary, chunk)?;

        match kind {
            TokenType::Minus => self.emit_byte(chunk, OpCode::Negate),
            _ => unreachable!("Only unary negation is supported for now"),
        };

        Ok(())
    }

    fn binary(&mut self, chunk: &mut Chunk) -> Result<(), CompilerError> {
        let operator = self.parser.previous.kind;
        let rule = parse_rule(operator);

        // parse the right operand, only parsing expression of higher precedence
        self.parse_precedence(rule.precedence(), chunk)?;

        match operator {
            TokenType::Minus => self.emit_byte(chunk, OpCode::Negate),
            TokenType::Plus => self.emit_byte(chunk, OpCode::Add),
            TokenType::Slash => self.emit_byte(chunk, OpCode::Divide),
            TokenType::Star => self.emit_byte(chunk, OpCode::Multiply),
            _ => unreachable!("Only arithmetic binary operators should be possible"),
        };

        Ok(())
    }

    // These are all of Lox’s precedence levels in order from lowest to highest.
    // Since C implicitly gives successively larger numbers for enums, this means
    // that PREC_CALL is numerically larger than PREC_UNARY. For example, say the
    // compiler is sitting on a chunk of code like:
    // -a.b + c
    // If we call parsePrecedence(PREC_ASSIGNMENT), then it will parse the
    // entire expression because + has higher precedence than assignment. If
    // instead we call parsePrecedence(PREC_UNARY), it will compile the -a.b and
    // stop there. It doesn’t keep going through the + because the addition has
    // lower precedence than unary operators.
    fn parse_precedence(
        &mut self,
        precedence: Precedence,
        chunk: &mut Chunk,
    ) -> Result<(), CompilerError> {
        todo!()
    }

    fn handle_error(&mut self, error: CompilerError) {
        // Only report the first error
        if !self.had_error {
            report_error(&self.source.load(), &error);
            self.had_error = true;
        }
    }
}

fn parse_rule(operator: TokenType) -> ParseRule {
    todo!()
}

pub struct ParseRule {}

impl ParseRule {
    fn precedence(&self) -> Precedence {
        todo!()
    }
}

#[derive(Error, Debug, Diagnostic)]
pub enum CompilerError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Scanner(#[from] ScanError),

    #[error("Unexpected token encountered {0} expected {1}")]
    UnexpectedToken(TokenType, TokenType),

    #[error(transparent)]
    Chunk(#[from] ChunkError),

    #[error(transparent)]
    Conversion(#[from] Serialization),
}

#[derive(Error, Debug, Diagnostic)]
pub enum Serialization {
    #[label = "Error parsing numerical value"]
    #[error(transparent)]
    Numer(#[from] ParseFloatError),
}

#[repr(u8)]
pub enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Eq,         // == !=
    Comp,       // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}
