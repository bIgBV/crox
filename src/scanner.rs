pub struct Scanner<'source> {
    start: usize,
    current: usize,
    line: usize,
    // Storing the reference to the entire string in
    source: &'source str,
}

impl<'source> Scanner<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            start: 0,
            current: 0,
            line: 1,
            source,
        }
    }

    fn is_at_end(&self) -> bool {
        // Whenever we reach the end of the source.
        // We should never go past the end, but I'm being defensive.
        self.current >= self.source.len()
    }

    fn make_token(&self, kind: TokenType) -> Token<'_> {
        Token {
            kind,
            start: self.start,
            length: self.current - self.start,
            line: self.line,
            source: self.source,
        }
    }

    fn advance(&self) -> &char {
        self.current += 1;
        debug_assert!(
            !self.is_at_end(),
            "Called Scanner::advance after we read past the end of the source"
        );

        self.source.get(self.current).unwrap()
    }
}

impl<'source> Iterator for Scanner<'source> {
    type Item = Token<'source>;

    fn next(&mut self) -> Option<Self::Item> {
        // Advance scanner to the current idx
        self.start = self.current;

        if (self.is_at_end()) {
            None
        } else {
            self.m
        }

        // TODO handle error messages. Probably through string interning?
    }
}

/// A token of the Lox language.
///
/// Each token is a span into the source this scanner is initialized with.
pub struct Token<'source> {
    kind: TokenType,
    start: usize,
    length: usize,
    line: usize,
    source: &'source str,
}

impl<'source> Token<'source> {}

pub enum TokenType {
    // Single-character tokens.
    TokenLeftParen,
    TokenRightParen,
    TokenLeftBrace,
    TokenRightBrace,
    TokenComma,
    TokenDot,
    TokenMinus,
    TokenPlus,
    TokenSemicolon,
    TokenSlash,
    TokenStar,
    // One or two character tokens.
    TokenBang,
    TokenBangEqual,
    TokenEqual,
    TokenEqualEqual,
    TokenGreater,
    TokenGreaterEqual,
    TokenLess,
    TokenLessEqual,
    // Literals.
    TokenIdentifier,
    TokenString,
    TokenNumber,
    // Keywords.
    TokenAnd,
    TokenClass,
    TokenElse,
    TokenFalse,
    TokenFor,
    TokenFun,
    TokenIf,
    TokenNil,
    TokenOr,
    TokenPrint,
    TokenReturn,
    TokenSuper,
    TokenThis,
    TokenTrue,
    TokenVar,
    TokenWhile,

    TokenError,
    TokenEof,
}
