use std::{
    cell::OnceCell,
    num::ParseFloatError,
    sync::{
        atomic::{AtomicBool, Ordering},
        OnceLock, RwLock,
    },
};

use miette::Diagnostic;
use thiserror::Error;
use tracing::instrument;

use crate::{
    chunk::{Chunk, ChunkError, OpCode},
    repl::report_error,
    scanner::{ScanError, Scanner, Token, TokenType},
};

#[derive(Debug)]
struct Parser<'source> {
    previous: Token<'source>,
    current: Token<'source>,
    scanner: RwLock<Scanner<'source>>,
    source: OnceCell<&'source str>,
    had_error: AtomicBool,
}

impl<'source> Parser<'source> {
    pub fn new() -> Self {
        Parser {
            previous: Token::empty(),
            current: Token::empty(),
            scanner: RwLock::new(Scanner::init("")),
            source: OnceCell::new(),
            had_error: AtomicBool::new(false),
        }
    }

    fn update_scanner(&mut self, source: &'source str) {
        self.source.get_or_init(|| source);

        *self.scanner.write().unwrap() = Scanner::init(source);
    }

    fn source(&self) -> &str {
        self.source.get().expect("Parser in uninitialized state!")
    }

    fn advance(&mut self) -> Result<(), CompilerError> {
        self.previous = self.current.clone();

        for token in &mut *self.scanner.write().unwrap() {
            match token {
                Ok(token) => {
                    self.current = token;
                    break;
                }
                Err(err) => {
                    self.handle_error(CompilerError::Scanner(err));
                }
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

    fn handle_error(&self, error: CompilerError) {
        // Only report the first error
        if !self.had_error.load(Ordering::Relaxed) {
            report_error(self.source(), &error);
            self.had_error.store(true, Ordering::Relaxed);
        }
    }
}

#[instrument(skip(source))]
pub fn compile(source: &str) -> Result<Chunk, CompilerError> {
    // Store the source before begining compilation.
    let mut parser = Parser::new();
    parser.update_scanner(source);

    let mut chunk = Chunk::new("another-one");

    if let Err(error) = parser.advance() {
        parser.handle_error(error);
    }

    expression(&mut parser, &mut chunk)?;

    end_compiler(&parser, &mut chunk);
    // TODO how to surface the fact that there was an error _after_ finishing parsing the source?
    Ok(chunk)
}

fn end_compiler(parser: &Parser, chunk: &mut Chunk) {
    emit_byte(parser, chunk, OpCode::Return)
}

fn emit_byte(parser: &Parser, chunk: &Chunk, opcode: OpCode) {
    chunk.write(opcode, parser.current.line)
}

// All compiler frontend methods need to distinguish the lifetime that the
// parser internally manages from the lifetime of the references to the parser
// itself.  Both the parser and the chunk are instantiated in the `compile`
// method so they can share the same lifetime.
#[instrument(skip_all, fields(previous = %parser.previous.kind, current = %parser.current.kind))]
fn expression<'compile, 'source>(
    parser: &'compile mut Parser<'source>,
    chunk: &'compile mut Chunk,
) -> Result<(), CompilerError> {
    // We simply parse the lowest precedence level, which subsumes all of the higher-precedence expressions too
    parse_precedence(parser, Precedence::Assignment, chunk)?;

    Ok(())
}

#[instrument(skip_all, fields(previous = %parser.previous.kind, current = %parser.current.kind))]
fn number<'compile, 'source>(
    parser: &'compile mut Parser<'source>,
    chunk: &'compile mut Chunk,
) -> Result<(), CompilerError> {
    let source = parser.source();
    let range = parser.previous.start..parser.previous.start + parser.previous.length;
    let value = &source[range]
        .parse::<f64>()
        .map_err(|err| Serialization::Numer(err))?;

    chunk.write_constant(*value, parser.previous.line)?;

    Ok(())
}

#[instrument(skip_all, fields(previous = %parser.previous.kind, current = %parser.current.kind))]
fn grouping<'compile, 'source>(
    parser: &'compile mut Parser<'source>,
    chunk: &'compile mut Chunk,
) -> Result<(), CompilerError> {
    // We should have already consumed the LeftParen
    expression(parser, chunk)?;
    parser.consume(TokenType::RightParen)?;

    Ok(())
}

// We evaluate the operand first which leaves its value on the stack.
// Then we pop that value, negate it, and push the result.
#[instrument(skip_all, fields(previous = %parser.previous.kind, current = %parser.current.kind))]
fn unary<'compile, 'source>(
    parser: &'compile mut Parser<'source>,
    chunk: &'compile mut Chunk,
) -> Result<(), CompilerError> {
    let kind = parser.previous.kind;

    // Compile the operand
    parse_precedence(parser, Precedence::Unary, chunk)?;

    match kind {
        TokenType::Minus => emit_byte(parser, chunk, OpCode::Negate),
        _ => unreachable!("Only unary negation is supported for now"),
    };

    Ok(())
}

#[instrument(skip_all, fields(previous = %parser.previous.kind, current = %parser.current.kind))]
fn binary<'compile, 'source>(
    parser: &'compile mut Parser<'source>,
    chunk: &'compile mut Chunk,
) -> Result<(), CompilerError> {
    let operator = parser.previous.kind;
    let rule = parse_rule(operator);

    // parse the right operand, only parsing expression of higher precedence
    parse_precedence(parser, rule.precedence, chunk)?;

    match operator {
        TokenType::Minus => emit_byte(parser, chunk, OpCode::Negate),
        TokenType::Plus => emit_byte(parser, chunk, OpCode::Add),
        TokenType::Slash => emit_byte(parser, chunk, OpCode::Divide),
        TokenType::Star => emit_byte(parser, chunk, OpCode::Multiply),
        _ => unreachable!("Only arithmetic binary operators should be possible"),
    };

    Ok(())
}

#[instrument(skip(parser, chunk), fields(previous = %parser.previous.kind, current = %parser.current.kind))]
fn parse_precedence<'compile, 'source>(
    parser: &'compile mut Parser<'source>,
    precedence: Precedence,
    chunk: &'compile mut Chunk,
) -> Result<(), CompilerError> {
    // Advance so we consume the token.
    parser.advance()?;
    let Some(prefix) = parse_rule(parser.previous.kind).prefix else {
        return Err(CompilerError::NoPrefixFn(parser.previous.kind));
    };

    prefix(parser, chunk)?;

    while precedence as u8 <= parse_rule(parser.current.kind).precedence as u8 {
        // advance the parser so we consume the infix operator
        parser.advance()?;
        let Some(infix) = parse_rule(parser.previous.kind).infix else {
            return Err(CompilerError::NoInfixFn(parser.previous.kind));
        };

        infix(parser, chunk)?;
    }

    Ok(())
}

/// The main parser dispatch table.
///
/// This table contains an array of [Compiler::ParseRule]s ordered by TokenType
/// The ordering occurs based on the `From<TokenType> for usize` implementation
/// which allows us to seemlessly index into the array.
static PARSE_RULES: OnceLock<[ParseRule; 38]> = OnceLock::new();

/// A function pointer to a compiler function.
type ParseFn =
    Option<for<'a, 'b> fn(&'a mut Parser<'b>, &'a mut Chunk) -> Result<(), CompilerError>>;

/// A grouping of parsing functions for a given token type along with it's associated precedence
#[derive(Debug)]
struct ParseRule {
    prefix: ParseFn,
    infix: ParseFn,
    precedence: Precedence,
}

#[rustfmt::skip]
fn parse_rule(operator: TokenType) -> &'static ParseRule {
    PARSE_RULES.get_or_init(|| {
        [
            ParseRule { prefix: Some(grouping), infix: None,         precedence: Precedence::None }, // TokenType::LeftParen 
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::RightParen
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::LeftBrace
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::RightBrace
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::Comma,
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::Dot,
            ParseRule { prefix: Some(unary),    infix: Some(binary), precedence: Precedence::Term }, // TokenType::Minus
            ParseRule { prefix: None,           infix: Some(binary), precedence: Precedence::Term }, // TokenType::Plus
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::SemiColon
            ParseRule { prefix: None,           infix: Some(binary), precedence: Precedence::Factor }, // TokenType::Slash
            ParseRule { prefix: None,           infix: Some(binary), precedence: Precedence::Factor }, // TokenType::Star
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::Bang
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::BangEqual
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::Equal
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::EqualEqual
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::Greater
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::GreaterEqual
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::Less
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::LessEqual
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::Identifier
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::String
            ParseRule { prefix: Some(number),   infix: None,         precedence: Precedence::None }, // TokenType::Number
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::And
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::Class
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::Else
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::False
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::For
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::Fun
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::If
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::Nil
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::Or
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::Print
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::Return
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::Super
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::This
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::True
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::Var
            ParseRule { prefix: None,           infix: None,         precedence: Precedence::None }, // TokenType::While
        ]
    });

    &PARSE_RULES.get().unwrap()[operator as usize]
}

/// These are all of Lox’s precedence levels in order from lowest to highest.
///
/// For example, say the compiler is sitting on a chunk of code like:
/// -a.b + c If we call parsePrecedence(PREC_ASSIGNMENT), then it will parse the
/// entire expression because + has higher precedence than assignment. If
/// instead we call parse_precedence(UNARY), it will compile the -a.b and
/// stop there. It doesn’t keep going through the + because the addition has
/// lower precedence than unary operators.
#[repr(u8)]
#[derive(Debug, Clone, Copy)]
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

    #[error("Expected Infix mapping for {0}")]
    NoInfixFn(TokenType),

    #[error("Expected Prefix mapping for {0}")]
    NoPrefixFn(TokenType),
}

#[derive(Error, Debug, Diagnostic)]
pub enum Serialization {
    #[label = "Error parsing numerical value"]
    #[error(transparent)]
    Numer(#[from] ParseFloatError),
}
