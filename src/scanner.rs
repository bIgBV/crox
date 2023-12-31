use std::{fmt::Display, str::Chars};

use miette::Diagnostic;
use thiserror::Error;
use tracing::{debug, instrument, trace};

#[derive(Debug)]
pub struct Scanner<'source> {
    start: usize,
    current: usize,
    line: usize,
    // Storing the reference to the entire string in
    source: &'source str,
    source_iter: Chars<'source>,
}

impl<'source> Scanner<'source> {
    pub fn init(source: &'source str) -> Self {
        Self {
            start: 0,
            current: 0,
            line: 1,
            source,
            // Initialize the UTF-8 character iterator and save it
            source_iter: source.chars(),
        }
    }

    fn is_at_end(&self) -> bool {
        // Whenever we reach the end of the source.
        // We should never go past the end, but I'm being defensive.
        self.current > self.source.len() - 1
    }

    #[instrument(skip_all, fields(start = self.start, current = self.current))]
    fn make_token(&self, kind: TokenType) -> Token<'source> {
        let token = Token {
            kind,
            start: self.start,
            length: self.current - self.start,
            line: self.line,
            source: self.source,
        };
        debug!(%token, "Creating token");
        token
    }

    #[instrument(skip_all, fields(start = self.start, current = self.current))]
    fn advance(&mut self) -> char {
        debug_assert!(
            !self.is_at_end(),
            "Called Scanner::advance after we read past the end of the source"
        );

        self.current += 1;
        trace!("Advancing iterator");
        self.source_iter.next().unwrap()
    }

    #[instrument(skip_all, fields(start = self.start, current = self.current))]
    fn match_char(&mut self, expected: char) -> bool {
        trace!(expected_char=%expected);
        if self.is_at_end() || self.current_char() != Some(expected) {
            false
        } else {
            self.advance();
            true
        }
    }

    #[instrument(skip_all, fields(start = self.start, current = self.current))]
    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                Some(' ') | Some('\t') => {
                    trace!("Whitespace");
                    self.advance();
                }
                Some('\n') => {
                    trace!("New line!");
                    self.line += 1;
                    self.advance();
                }
                Some('/') => match self.peek_next() {
                    Some('/') => {
                        trace!("Skiping comment");
                        while self.peek() != Some('\n') && !self.is_at_end() {
                            self.advance();
                        }
                    }
                    // We aren't in a line comment, return without consuming anything
                    Some(_) | None => return,
                },
                _ => break,
            }
        }
    }

    #[instrument(skip_all, fields(start = self.start, current = self.current))]
    fn peek(&mut self) -> Option<char> {
        // Linear search getting more and more expenvise everytime. Fine for now?
        self.current_char()
    }

    #[instrument(skip_all, fields(start = self.start, current = self.current))]
    fn current_char(&self) -> Option<char> {
        self.source.chars().nth(self.current)
    }

    #[instrument(skip_all, fields(start = self.start, current = self.current))]
    fn start_char(&self, idx: usize) -> char {
        debug_assert!(self.start < self.current, "Start not less than current");
        self.source.chars().nth(self.start + idx).unwrap()
    }

    #[instrument(skip_all, fields(start = self.start, current = self.current))]
    fn peek_next(&mut self) -> Option<char> {
        if self.is_at_end() {
            None
        } else {
            self.source.chars().nth(self.current + 1)
        }
    }

    #[instrument(skip_all, fields(start = self.start, current = self.current))]
    fn string(&mut self) -> Result<Token<'source>, ScanError> {
        while self.peek() != Some('"') && !self.is_at_end() {
            if self.peek() == Some('\n') {
                self.line += 1
            }

            self.advance();
        }

        if self.is_at_end() {
            return Err(ScanError::UnterminatedString(self.start));
        }

        // The closing quote.
        self.advance();

        Ok(self.make_token(TokenType::String))
    }

    #[instrument(skip_all, fields(start = self.start, current = self.current))]
    fn number(&mut self) -> Token<'source> {
        while self.peek().is_some_and(|c| c.is_ascii_digit()) {
            self.advance();
        }

        // Look for the fractional part
        if self.peek() == Some('.') && self.peek_next().is_some_and(|c| c.is_ascii_digit()) {
            // Consume the "."
            self.advance();
            while self.peek().is_some_and(|c| c.is_ascii_digit()) {
                self.advance();
            }
        }

        self.make_token(TokenType::Number)
    }

    #[instrument(skip_all, fields(start = self.start, current = self.current))]
    fn identifier(&mut self) -> Token<'source> {
        while let Some(true) = self.peek().map(|c| c.is_alphanumeric()) {
            self.advance();
        }

        self.make_token(self.identifier_type())
    }

    #[instrument]
    fn identifier_type(&self) -> TokenType {
        match self.start_char(0) {
            'a' => self.check_keyword(1, 2, "nd", TokenType::And),
            'c' => self.check_keyword(1, 4, "lass", TokenType::Class),
            'e' => self.check_keyword(1, 3, "lse", TokenType::Else),
            'i' => self.check_keyword(1, 1, "f", TokenType::If),
            'f' => match self.start_char(1) {
                'a' => self.check_keyword(2, 3, "lse", TokenType::False),
                'o' => self.check_keyword(2, 1, "r", TokenType::For),
                'u' => self.check_keyword(2, 1, "n", TokenType::Fun),
                _ => TokenType::Identifier,
            },
            't' => match self.start_char(1) {
                'h' => self.check_keyword(2, 2, "is", TokenType::This),
                'r' => self.check_keyword(2, 2, "ue", TokenType::True),
                _ => TokenType::Identifier,
            },
            'n' => self.check_keyword(1, 2, "il", TokenType::Nil),
            'o' => self.check_keyword(1, 1, "r", TokenType::Or),
            'p' => self.check_keyword(1, 4, "rint", TokenType::Print),
            'r' => self.check_keyword(1, 5, "eturn", TokenType::Return),
            's' => self.check_keyword(1, 4, "uper", TokenType::Super),
            'v' => self.check_keyword(1, 2, "ar", TokenType::Var),
            'w' => self.check_keyword(1, 4, "hile", TokenType::While),
            _ => TokenType::Identifier,
        }
    }

    #[instrument]
    fn check_keyword(
        &self,
        start: usize,
        length: usize,
        rest: &'static str,
        token_type: TokenType,
    ) -> TokenType {
        let keyword_start = self.start + start;
        if self
            .source
            .get(keyword_start..keyword_start + length)
            .is_some_and(|keyword_chunk| keyword_chunk == rest)
        {
            token_type
        } else {
            TokenType::Identifier
        }
    }
}

#[derive(Debug, Error, Clone, PartialEq, Copy, Eq, Diagnostic)]
pub enum ScanError {
    #[error("Unexpected charecter at {0}")]
    UnexpectedCharacter(usize),

    #[error("Unterminated string encountered starting at {0}")]
    UnterminatedString(usize),
}

impl<'source> Iterator for Scanner<'source> {
    type Item = Result<Token<'source>, ScanError>;

    fn next(&mut self) -> Option<Self::Item> {
        // Skip whitespace before producing a new token
        self.skip_whitespace();
        let mut token = None;

        if !self.is_at_end() {
            // Advance scanner to the current idx
            self.start = self.current;

            let c = self.advance();

            if c.is_alphabetic() {
                return Some(Ok(self.identifier()));
            }

            if c.is_ascii_digit() {
                return Some(Ok(self.number()));
            }

            token = match c {
                '(' => Some(Ok(self.make_token(TokenType::LeftParen))),
                ')' => Some(Ok(self.make_token(TokenType::RightParen))),
                '{' => Some(Ok(self.make_token(TokenType::LeftBrace))),
                '}' => Some(Ok(self.make_token(TokenType::RightBrace))),
                ';' => Some(Ok(self.make_token(TokenType::Semicolon))),
                ',' => Some(Ok(self.make_token(TokenType::Comma))),
                '.' => Some(Ok(self.make_token(TokenType::Dot))),
                '-' => Some(Ok(self.make_token(TokenType::Minus))),
                '+' => Some(Ok(self.make_token(TokenType::Plus))),
                '/' => Some(Ok(self.make_token(TokenType::Slash))),
                '*' => Some(Ok(self.make_token(TokenType::Star))),
                '!' => {
                    let kind = if self.match_char('=') {
                        TokenType::BangEqual
                    } else {
                        TokenType::Bang
                    };
                    Some(Ok(self.make_token(kind)))
                }
                '=' => {
                    let kind = if self.match_char('=') {
                        TokenType::EqualEqual
                    } else {
                        TokenType::Equal
                    };
                    Some(Ok(self.make_token(kind)))
                }
                '<' => {
                    let kind = if self.match_char('=') {
                        TokenType::LessEqual
                    } else {
                        TokenType::Less
                    };
                    Some(Ok(self.make_token(kind)))
                }
                '>' => {
                    let kind = if self.match_char('=') {
                        TokenType::GreaterEqual
                    } else {
                        TokenType::Greater
                    };
                    Some(Ok(self.make_token(kind)))
                }
                '"' => Some(self.string()),
                _ => Some(Err(ScanError::UnexpectedCharacter(self.start))),
            };
        }

        token
    }
}

/// A token of the Lox language.
///
/// Each token is a span into the source this scanner is initialized with.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token<'source> {
    pub kind: TokenType,
    pub start: usize,
    pub length: usize,
    pub line: usize,
    pub source: &'source str,
}

impl<'source> Display for Token<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lexeme = self
            .source
            .get(self.start..self.start + self.length)
            .expect("Unable to get lexeme string slice");
        write!(f, "{:#}", lexeme)
    }
}

impl<'source> Token<'source> {
    pub fn empty() -> Self {
        Token {
            kind: TokenType::Empty,
            start: 0,
            length: 0,
            line: 1,
            source: "empty",
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals.
    Identifier,
    String,
    Number,
    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Empty,
}

impl From<TokenType> for usize {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::LeftParen => 0,
            TokenType::RightParen => 1,
            TokenType::LeftBrace => 2,
            TokenType::RightBrace => 3,
            TokenType::Comma => 4,
            TokenType::Dot => 5,
            TokenType::Minus => 6,
            TokenType::Plus => 7,
            TokenType::Semicolon => 8,
            TokenType::Slash => 9,
            TokenType::Star => 10,
            TokenType::Bang => 11,
            TokenType::BangEqual => 12,
            TokenType::Equal => 13,
            TokenType::EqualEqual => 14,
            TokenType::Greater => 15,
            TokenType::GreaterEqual => 16,
            TokenType::Less => 17,
            TokenType::LessEqual => 18,
            TokenType::Identifier => 19,
            TokenType::String => 20,
            TokenType::Number => 21,
            TokenType::And => 22,
            TokenType::Class => 23,
            TokenType::Else => 24,
            TokenType::False => 25,
            TokenType::For => 26,
            TokenType::Fun => 27,
            TokenType::If => 28,
            TokenType::Nil => 29,
            TokenType::Or => 30,
            TokenType::Print => 31,
            TokenType::Return => 32,
            TokenType::Super => 33,
            TokenType::This => 34,
            TokenType::True => 35,
            TokenType::Var => 36,
            TokenType::While => 37,
            TokenType::Empty => {
                unreachable!("Empty token is only used during parser initialization")
            }
        }
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::LeftParen => write!(f, "("),
            TokenType::RightParen => write!(f, ")"),
            TokenType::LeftBrace => write!(f, "{{"),
            TokenType::RightBrace => write!(f, "}}"),
            TokenType::Comma => write!(f, ","),
            TokenType::Dot => write!(f, "."),
            TokenType::Minus => write!(f, "-"),
            TokenType::Plus => write!(f, "+"),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::Slash => write!(f, "/"),
            TokenType::Star => write!(f, "*"),
            TokenType::Bang => write!(f, "!"),
            TokenType::BangEqual => write!(f, "!="),
            TokenType::Equal => write!(f, "="),
            TokenType::EqualEqual => write!(f, "=="),
            TokenType::Greater => write!(f, ">"),
            TokenType::GreaterEqual => write!(f, ">="),
            TokenType::Less => write!(f, "<"),
            TokenType::LessEqual => write!(f, "<="),
            TokenType::Identifier => write!(f, ""),
            TokenType::String => write!(f, "String"),
            TokenType::Number => write!(f, "Number"),
            TokenType::And => write!(f, "And"),
            TokenType::Class => write!(f, "Class"),
            TokenType::Else => write!(f, "Else"),
            TokenType::False => write!(f, "False"),
            TokenType::For => write!(f, "For"),
            TokenType::Fun => write!(f, "Fun"),
            TokenType::If => write!(f, "If"),
            TokenType::Nil => write!(f, "Nil"),
            TokenType::Or => write!(f, "Or"),
            TokenType::Print => write!(f, "Print"),
            TokenType::Return => write!(f, "Return"),
            TokenType::Super => write!(f, "Super"),
            TokenType::This => write!(f, "This"),
            TokenType::True => write!(f, "True"),
            TokenType::Var => write!(f, "Var"),
            TokenType::While => write!(f, "While"),
            TokenType::Empty => write!(f, "Empty"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;
    use tracing_test::traced_test;

    macro_rules! unwrap_assert_ok {
        ($scanner:expr, $token:expr) => {{
            // Get the Result<Token<'source>, ScanError>
            let result = $scanner.next().unwrap();
            assert_eq!(result, Ok($token));
        }};
    }

    macro_rules! unwrap_assert_err {
        ($scanner:expr, $err:expr) => {{
            // Get the Result<Token<'source>, ScanError>
            let result = $scanner.next().unwrap();
            assert_eq!(result, Err($err));
        }};
    }

    #[traced_test]
    #[test]
    fn test_unexpected_character() {
        let source = "∆";
        let mut scanner = Scanner::init(source);

        unwrap_assert_err!(scanner, ScanError::UnexpectedCharacter(0));
        // TODO once error happens, what do we do next?
        // assert_eq!(scanner.next(), None);
    }

    #[traced_test]
    #[test]
    fn test_unterminated_string() {
        let source = "\"this is an unterminated string";
        let mut scanner = Scanner::init(source);

        unwrap_assert_err!(scanner, ScanError::UnterminatedString(0));
    }

    #[traced_test]
    #[test]
    fn test_termination() {
        let source = ".";
        let mut scanner = Scanner::init(source);

        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Dot,
                start: 0,
                length: 1,
                line: 1,
                source
            }
        );

        assert_eq!(scanner.next(), None);
    }

    #[traced_test]
    #[test]
    fn single_character_tokens() {
        let source = "()!,=><.";

        let mut scanner = Scanner::init(source);

        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::LeftParen,
                start: 0,
                length: 1,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::RightParen,
                start: 1,
                length: 1,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Bang,
                start: 2,
                length: 1,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Comma,
                start: 3,
                length: 1,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Equal,
                start: 4,
                length: 1,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Greater,
                start: 5,
                length: 1,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Less,
                start: 6,
                length: 1,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Dot,
                start: 7,
                length: 1,
                line: 1,
                source
            }
        );
        assert_eq!(scanner.next(), None);
    }

    #[traced_test]
    #[test]
    fn two_character_tokens() {
        let source = "!=<=>===";

        let mut scanner = Scanner::init(source);

        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::BangEqual,
                start: 0,
                length: 2,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::LessEqual,
                start: 2,
                length: 2,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::GreaterEqual,
                start: 4,
                length: 2,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::EqualEqual,
                start: 6,
                length: 2,
                line: 1,
                source
            }
        );

        assert_eq!(scanner.next(), None);
    }

    #[traced_test]
    #[test]
    fn test_whitespace() {
        let source = "!=    <=

        >= == = ";

        let mut scanner = Scanner::init(source);

        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::BangEqual,
                start: 0,
                length: 2,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::LessEqual,
                start: 6,
                length: 2,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::GreaterEqual,
                start: 18,
                length: 2,
                line: 3,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::EqualEqual,
                start: 21,
                length: 2,
                line: 3,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Equal,
                start: 24,
                length: 1,
                line: 3,
                source
            }
        );

        assert_eq!(scanner.next(), None);
    }

    #[traced_test]
    #[test]
    fn test_comments() {
        let source = "./
// this should be ignored
!=";

        let mut scanner = Scanner::init(source);
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Dot,
                start: 0,
                length: 1,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Slash,
                start: 1,
                length: 1,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::BangEqual,
                start: 29,
                length: 2,
                line: 3,
                source
            }
        );

        assert_eq!(scanner.next(), None);
    }

    #[traced_test]
    #[test]
    fn test_string() {
        let source = ".\"Another sting\",";
        let mut scanner = Scanner::init(source);

        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Dot,
                start: 0,
                length: 1,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::String,
                start: 1,
                length: 15,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Comma,
                start: 16,
                length: 1,
                line: 1,
                source
            }
        );

        assert_eq!(scanner.next(), None);
    }

    #[traced_test]
    #[test]
    fn test_number() {
        let source = "42.69 1337";
        let mut scanner = Scanner::init(source);

        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Number,
                start: 0,
                length: 5,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Number,
                start: 6,
                length: 4,
                line: 1,
                source
            }
        );

        assert_eq!(scanner.next(), None)
    }

    #[traced_test]
    #[test]
    fn test_keywords() {
        let source =
            "and class else false for fun if nil or print return super this true var while";
        let mut scanner = Scanner::init(source);

        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::And,
                start: 0,
                length: 3,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Class,
                start: 4,
                length: 5,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Else,
                start: 10,
                length: 4,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::False,
                start: 15,
                length: 5,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::For,
                start: 21,
                length: 3,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Fun,
                start: 25,
                length: 3,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::If,
                start: 29,
                length: 2,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Nil,
                start: 32,
                length: 3,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Or,
                start: 36,
                length: 2,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Print,
                start: 39,
                length: 5,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Return,
                start: 45,
                length: 6,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Super,
                start: 52,
                length: 5,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::This,
                start: 58,
                length: 4,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::True,
                start: 63,
                length: 4,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Var,
                start: 68,
                length: 3,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::While,
                start: 72,
                length: 5,
                line: 1,
                source
            }
        );

        assert_eq!(scanner.next(), None);
    }

    #[traced_test]
    #[test]
    fn test_simple_if_expression() {
        let source = "if (5 > 2) { yes } else { false }";
        let mut scanner = Scanner::init(source);

        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::If,
                start: 0,
                length: 2,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::LeftParen,
                start: 3,
                length: 1,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Number,
                start: 4,
                length: 1,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Greater,
                start: 6,
                length: 1,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Number,
                start: 8,
                length: 1,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::RightParen,
                start: 9,
                length: 1,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::LeftBrace,
                start: 11,
                length: 1,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Identifier,
                start: 13,
                length: 3,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::RightBrace,
                start: 17,
                length: 1,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::Else,
                start: 19,
                length: 4,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::LeftBrace,
                start: 24,
                length: 1,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::False,
                start: 26,
                length: 5,
                line: 1,
                source
            }
        );
        unwrap_assert_ok!(
            scanner,
            Token {
                kind: TokenType::RightBrace,
                start: 32,
                length: 1,
                line: 1,
                source
            }
        );

        assert_eq!(scanner.next(), None);
    }
}
