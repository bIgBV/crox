use std::{hint, str::Chars};

pub struct Scanner<'source> {
    start: usize,
    current: usize,
    line: usize,
    // Storing the reference to the entire string in
    source: &'source str,
    source_iter: Chars<'source>,
}

impl<'source> Scanner<'source> {
    pub fn new(source: &'source str) -> Self {
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
        self.current > self.source.len()
    }

    fn make_token(&self, kind: TokenType) -> Token<'source> {
        Token {
            kind,
            start: self.start,
            length: self.current - self.start,
            line: self.line,
            source: self.source,
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        debug_assert!(
            !self.is_at_end(),
            "Called Scanner::advance after we read past the end of the source"
        );

        self.source_iter.next().unwrap()
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            false
        } else if self.current_char() != expected {
            false
        } else {
            self.advance();
            true
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                ' ' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' => {
                    let Some(peek_next) = self.peek_next() else {
                        return;
                    };

                    if peek_next == '/' {
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        // We aren't in a line comment, return
                        return;
                    }
                }
                _ => break,
            }
        }
    }

    fn peek(&mut self) -> char {
        debug_assert!(
            !self.is_at_end(),
            "Called Scanner::peek after we read past the end of the source"
        );

        // Linear search getting more and more expenvise everytime. Fine for now?
        self.current_char()
    }

    fn current_char(&mut self) -> char {
        self.source.chars().nth(self.current).unwrap()
    }

    fn peek_next(&mut self) -> Option<char> {
        if self.is_at_end() {
            None
        } else {
            self.source.chars().nth(self.current + 1)
        }
    }
}

impl<'source> Iterator for Scanner<'source> {
    type Item = Token<'source>;

    fn next(&mut self) -> Option<Self::Item> {
        // Skip whitespace before producing a new token
        self.skip_whitespace();
        // Advance scanner to the current idx
        self.start = self.current;

        if self.is_at_end() {
            None
        } else {
            let c = self.advance();

            match c {
                '(' => Some(self.make_token(TokenType::LeftParen)),
                ')' => Some(self.make_token(TokenType::RightParen)),
                '{' => Some(self.make_token(TokenType::LeftBrace)),
                '}' => Some(self.make_token(TokenType::RightBrace)),
                ';' => Some(self.make_token(TokenType::Semicolon)),
                ',' => Some(self.make_token(TokenType::Comma)),
                '.' => Some(self.make_token(TokenType::Dot)),
                '-' => Some(self.make_token(TokenType::Minus)),
                '+' => Some(self.make_token(TokenType::Plus)),
                '/' => Some(self.make_token(TokenType::Slash)),
                '*' => Some(self.make_token(TokenType::Star)),
                '!' => {
                    let kind = if self.match_char('=') {
                        TokenType::BangEqual
                    } else {
                        TokenType::Bang
                    };
                    Some(self.make_token(kind))
                }
                '=' => {
                    let kind = if self.match_char('=') {
                        TokenType::EqualEqual
                    } else {
                        TokenType::Equal
                    };
                    Some(self.make_token(kind))
                }
                '<' => {
                    let kind = if self.match_char('=') {
                        TokenType::LessEqual
                    } else {
                        TokenType::Less
                    };
                    Some(self.make_token(kind))
                }
                '>' => {
                    let kind = if self.match_char('=') {
                        TokenType::GreaterEqual
                    } else {
                        TokenType::Greater
                    };
                    Some(self.make_token(kind))
                }
                _ => Some(self.make_token(TokenType::Error)),
            }
        }
    }
}

/// A token of the Lox language.
///
/// Each token is a span into the source this scanner is initialized with.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token<'source> {
    kind: TokenType,
    start: usize,
    length: usize,
    line: usize,
    source: &'source str,
}

impl<'source> Token<'source> {}

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

    Error,
    Eof,
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn single_character_tokens() {
        let source = "()!,=><.";

        let mut scanner = Scanner::new(source);

        assert_eq!(
            scanner.next(),
            Some(Token {
                kind: TokenType::LeftParen,
                start: 0,
                length: 1,
                line: 1,
                source
            })
        );
        assert_eq!(
            scanner.next(),
            Some(Token {
                kind: TokenType::RightParen,
                start: 1,
                length: 1,
                line: 1,
                source
            })
        );
        assert_eq!(
            scanner.next(),
            Some(Token {
                kind: TokenType::Bang,
                start: 2,
                length: 1,
                line: 1,
                source
            })
        );
        assert_eq!(
            scanner.next(),
            Some(Token {
                kind: TokenType::Comma,
                start: 3,
                length: 1,
                line: 1,
                source
            })
        );
        assert_eq!(
            scanner.next(),
            Some(Token {
                kind: TokenType::Equal,
                start: 4,
                length: 1,
                line: 1,
                source
            })
        );
        assert_eq!(
            scanner.next(),
            Some(Token {
                kind: TokenType::Greater,
                start: 5,
                length: 1,
                line: 1,
                source
            })
        );
        assert_eq!(
            scanner.next(),
            Some(Token {
                kind: TokenType::Less,
                start: 6,
                length: 1,
                line: 1,
                source
            })
        );
        assert_eq!(
            scanner.next(),
            Some(Token {
                kind: TokenType::Dot,
                start: 7,
                length: 1,
                line: 1,
                source
            })
        )
    }

    #[test]
    fn two_character_tokens() {
        let source = "!=<=>===";

        let mut scanner = Scanner::new(source);

        assert_eq!(
            scanner.next(),
            Some(Token {
                kind: TokenType::BangEqual,
                start: 0,
                length: 2,
                line: 1,
                source
            })
        );
        assert_eq!(
            scanner.next(),
            Some(Token {
                kind: TokenType::LessEqual,
                start: 2,
                length: 2,
                line: 1,
                source
            })
        );
        assert_eq!(
            scanner.next(),
            Some(Token {
                kind: TokenType::GreaterEqual,
                start: 4,
                length: 2,
                line: 1,
                source
            })
        );
        assert_eq!(
            scanner.next(),
            Some(Token {
                kind: TokenType::EqualEqual,
                start: 6,
                length: 2,
                line: 1,
                source
            })
        );
    }

    #[test]
    fn test_whitespace() {
        let source = "!=    <=

        >= == = ";

        let mut scanner = Scanner::new(source);

        assert_eq!(
            scanner.next(),
            Some(Token {
                kind: TokenType::BangEqual,
                start: 0,
                length: 2,
                line: 1,
                source
            })
        );
        assert_eq!(
            scanner.next(),
            Some(Token {
                kind: TokenType::LessEqual,
                start: 6,
                length: 2,
                line: 1,
                source
            })
        );
        assert_eq!(
            scanner.next(),
            Some(Token {
                kind: TokenType::GreaterEqual,
                start: 26,
                length: 2,
                line: 3,
                source
            })
        );
        assert_eq!(
            scanner.next(),
            Some(Token {
                kind: TokenType::EqualEqual,
                start: 29,
                length: 2,
                line: 3,
                source
            })
        );
        assert_eq!(
            scanner.next(),
            Some(Token {
                kind: TokenType::Equal,
                start: 32,
                length: 1,
                line: 3,
                source
            })
        );
    }
}
