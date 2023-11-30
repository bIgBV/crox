use std::{
    cell::OnceCell,
    fmt::Display,
    num::ParseFloatError,
    str::FromStr,
    sync::{
        atomic::{AtomicBool, Ordering},
        OnceLock, RwLock,
    },
};

use miette::Diagnostic;
use thiserror::Error;
use tracing::{debug, error, instrument, trace};

use crate::{
    chunk::{Chunk, ChunkError, OpCode},
    repl::report_error,
    scanner::{ScanError, Scanner, Token, TokenType},
    value::Value,
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

    #[instrument(skip_all)]
    fn advance(&mut self) -> Result<(), CompilerError> {
        trace!("Advancing parser");
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
            Err(CompilerError::UnexpectedToken(self.current.kind, kind))
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

fn emit_bytes(parser: &Parser, chunk: &Chunk, first: OpCode, second: OpCode) {
    chunk.write(first, parser.current.line);
    chunk.write(second, parser.current.line);
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
        .map_err(Serialization::Number)?;

    trace!(number=%(*value), "parsed number");
    chunk.write_constant(*value, parser.previous.line)?;

    Ok(())
}

#[instrument(skip_all, fields(previous = %parser.previous.kind, current = %parser.current.kind))]
fn grouping<'compile, 'source>(
    parser: &'compile mut Parser<'source>,
    chunk: &'compile mut Chunk,
) -> Result<(), CompilerError> {
    trace!("parsing gropuing");
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

    trace!(op_kind=?kind);
    match kind {
        TokenType::Minus => emit_byte(parser, chunk, OpCode::Negate),
        TokenType::Bang => emit_byte(parser, chunk, OpCode::Not),
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

    debug!(op=?operator, "Parsing binary operator");

    match operator {
        TokenType::Minus => emit_byte(parser, chunk, OpCode::Negate),
        TokenType::Plus => emit_byte(parser, chunk, OpCode::Add),
        TokenType::Slash => emit_byte(parser, chunk, OpCode::Divide),
        TokenType::Star => emit_byte(parser, chunk, OpCode::Multiply),
        TokenType::BangEqual => emit_bytes(parser, chunk, OpCode::Equal, OpCode::Not),
        TokenType::EqualEqual => emit_byte(parser, chunk, OpCode::Equal),
        TokenType::Greater => emit_byte(parser, chunk, OpCode::Greater),
        // Simplifying the opcodes by doing negations for compound comparisons
        TokenType::GreaterEqual => emit_bytes(parser, chunk, OpCode::Less, OpCode::Not),
        TokenType::Less => emit_byte(parser, chunk, OpCode::Less),
        // Simplifying the opcodes by doing negations for compound comparisons
        TokenType::LessEqual => emit_bytes(parser, chunk, OpCode::Greater, OpCode::Not),
        _ => unreachable!("Only arithmetic binary operators should be possible"),
    };

    Ok(())
}

#[instrument(skip_all, fields(previous = %parser.previous.kind, current = %parser.current.kind))]
fn literal<'compile, 'source>(
    parser: &'compile mut Parser<'source>,
    chunk: &'compile mut Chunk,
) -> Result<(), CompilerError> {
    let kind = parser.previous.kind;
    debug!(?kind, "Parsing literal");

    match kind {
        TokenType::False => emit_byte(parser, chunk, OpCode::False),
        TokenType::True => emit_byte(parser, chunk, OpCode::True),
        TokenType::Nil => emit_byte(parser, chunk, OpCode::Nil),
        _ => unreachable!("No other literal possible"),
    };

    Ok(())
}

#[instrument(skip_all, fields(previous = %parser.previous.kind, current = %parser.current.kind))]
fn string<'compile, 'source>(
    parser: &'compile mut Parser<'source>,
    chunk: &'compile mut Chunk,
) -> Result<(), CompilerError> {
    let Some(str) = parser
        .previous
        .source
        .get(parser.previous.start + 1..parser.previous.length - 2)
    else {
        error!(%parser.previous, "Incomplete string");
        return Err(CompilerError::IncompleteStr(parser.previous.start));
    };

    chunk.add_value(Value::new_string(str))?;

    Ok(())
}

#[instrument(skip(parser, chunk), fields(previous = %parser.previous.kind, current = %parser.current.kind))]
fn parse_precedence<'compile, 'source>(
    parser: &'compile mut Parser<'source>,
    precedence: Precedence,
    chunk: &'compile mut Chunk,
) -> Result<(), CompilerError> {
    debug!(%precedence, "Parsing with precedence");
    // Advance so we consume the token.
    parser.advance()?;
    let Some(prefix) = parse_rule(parser.previous.kind).prefix else {
        return Err(CompilerError::NoPrefixFn(parser.previous.kind));
    };

    prefix(parser, chunk)?;

    // While the precedence is lower than the current token's precedence, keep parsing the infix expression
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
#[instrument]
fn parse_rule(operator: TokenType) -> &'static ParseRule {
    PARSE_RULES.get_or_init(|| {
        [
            ParseRule { prefix: Some(grouping), infix: None,            precedence: Precedence::None   }, // TokenType::LeftParen 
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::RightParen
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::LeftBrace
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::RightBrace
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::Comma,
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::Dot,
            ParseRule { prefix: Some(unary),    infix: Some(binary),    precedence: Precedence::Term   }, // TokenType::Minus
            ParseRule { prefix: None,           infix: Some(binary),    precedence: Precedence::Term   }, // TokenType::Plus
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::SemiColon
            ParseRule { prefix: None,           infix: Some(binary),    precedence: Precedence::Factor }, // TokenType::Slash
            ParseRule { prefix: None,           infix: Some(binary),    precedence: Precedence::Factor }, // TokenType::Star
            ParseRule { prefix: Some(unary),    infix: None,            precedence: Precedence::None   }, // TokenType::Bang
            ParseRule { prefix: None,           infix: Some(binary),    precedence: Precedence::Eq     }, // TokenType::BangEqual
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::Equal
            ParseRule { prefix: None,           infix: Some(binary),    precedence: Precedence::Eq     }, // TokenType::EqualEqual
            ParseRule { prefix: None,           infix: Some(binary),    precedence: Precedence::Comp   }, // TokenType::Greater
            ParseRule { prefix: None,           infix: Some(binary),    precedence: Precedence::Comp   }, // TokenType::GreaterEqual
            ParseRule { prefix: None,           infix: Some(binary),    precedence: Precedence::Comp   }, // TokenType::Less
            ParseRule { prefix: None,           infix: Some(binary),    precedence: Precedence::Comp   }, // TokenType::LessEqual
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::Identifier
            ParseRule { prefix: Some(string),   infix: None,            precedence: Precedence::None   }, // TokenType::String
            ParseRule { prefix: Some(number),   infix: None,            precedence: Precedence::None   }, // TokenType::Number
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::And
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::Class
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::Else
            ParseRule { prefix: Some(literal),  infix: None,            precedence: Precedence::None   }, // TokenType::False
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::For
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::Fun
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::If
            ParseRule { prefix: Some(literal),  infix: None,            precedence: Precedence::None   }, // TokenType::Nil
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::Or
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::Print
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::Return
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::Super
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::This
            ParseRule { prefix: Some(literal),  infix: None,            precedence: Precedence::None   }, // TokenType::True
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::Var
            ParseRule { prefix: None,           infix: None,            precedence: Precedence::None   }, // TokenType::While
        ]
    });

    let rule = &PARSE_RULES.get().unwrap()[operator as usize];
    trace!(?rule, "matched rule");
    rule
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

impl Display for Precedence {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Precedence::None => write!(f, "None {}", 0),
            Precedence::Assignment => write!(f, "= {}", 1),
            Precedence::Or => write!(f, "|| {}", 2),
            Precedence::And => write!(f, "&& {}", 3),
            Precedence::Eq => write!(f, "Eq {}", 4),
            Precedence::Comp => write!(f, "Comp {}", 5),
            Precedence::Term => write!(f, "Term {}", 6),
            Precedence::Factor => write!(f, "Factor {}", 7),
            Precedence::Unary => write!(f, "Unary {}", 8),
            Precedence::Call => write!(f, "Call {}", 9),
            Precedence::Primary => write!(f, "Primary {}", 10),
        }
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

    #[error("Expected Infix mapping for {0}")]
    NoInfixFn(TokenType),

    #[error("Expected Prefix mapping for {0}")]
    NoPrefixFn(TokenType),

    #[error("Incomplete string found at token: {0}")]
    IncompleteStr(usize),
}

#[derive(Error, Debug, Diagnostic)]
pub enum Serialization {
    #[label = "Error parsing numerical value"]
    #[error(transparent)]
    Number(#[from] ParseFloatError),
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;

    use crate::chunk::OpCode;

    use super::compile;

    #[test]
    fn parse_literal() {
        let source = "nil";
        let result = compile(source);
        assert!(result.is_ok());
        let result = result.unwrap();
        let expected = [OpCode::Nil as u8, OpCode::Return as u8];
        assert_eq!(&(*result.code.read().unwrap()), &expected);

        let source = "false";
        let result = compile(source);
        assert!(result.is_ok());
        let result = result.unwrap();
        let expected = [OpCode::False as u8, OpCode::Return as u8];
        assert_eq!(&(*result.code.read().unwrap()), &expected);

        let source = "true";
        let result = compile(source);
        assert!(result.is_ok());
        let result = result.unwrap();
        let expected = [OpCode::True as u8, OpCode::Return as u8];
        assert_eq!(&(*result.code.read().unwrap()), &expected);
    }

    #[test]
    fn parse_number() {
        let source = "7003.8008";
        let result = compile(source);
        assert!(result.is_ok());
        let result = result.unwrap();

        let offset = result.constant_offsets()[0];
        // The the offset of the constant is also inserted into the chunk. In this case
        // it's 0usize which would take up 8 bytes
        let mut expected = vec![OpCode::Constant as u8];
        expected.extend_from_slice(&bytemuck::cast::<usize, [u8; 8]>(offset.0));
        expected.push(OpCode::Return as u8);

        assert_eq!(&(*result.code.read().unwrap()), &expected);
    }

    #[test]
    fn parse_unary() {
        let source = "-78";
        let result = compile(source);
        assert!(result.is_ok());
        let chunk = result.unwrap();
        let offset = chunk.constant_offsets()[0];

        let mut expected = vec![];
        expected.push(OpCode::Constant as u8);
        expected.extend_from_slice(&bytemuck::cast::<usize, [u8; 8]>(offset.0));
        expected.push(OpCode::Negate as u8);
        expected.push(OpCode::Return as u8);
        assert_eq!((*chunk.code.read().unwrap()), expected);

        let source = "!true";

        let result = compile(source);
        assert!(result.is_ok());

        let mut expected = vec![];
        expected.push(OpCode::True as u8);
        expected.push(OpCode::Not as u8);
        expected.push(OpCode::Return as u8);

        assert_eq!(*result.unwrap().code.read().unwrap(), expected);
    }

    #[test]
    fn parse_binary() {
        let source = "(-7 * (8 / (64 + 8))) >= (6 <= (84 == 2))";
        let result = compile(source);
        assert!(result.is_ok());

        println!("{:?}", result.unwrap().code.read());
    }
}
