use std::str;

#[derive(Clone, Debug)]
pub enum TokenKind {
    Unknown,
    Literal,
    Identifier,

    Plus,
    Minus,
    Star,
    Slash,
    Power,
    Equals,
    Exclamation,

    Deg,
    Rad,

    Pipe,
    OpenParenthesis,
    ClosedParenthesis,
    Comma,

    EOF,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
}

pub struct Lexer<'a> {
    source: &'a [u8],
    index: usize,
}

impl<'a> Lexer<'a> {
    pub fn lex(source: &str) -> Vec<Token> {
        let mut lexer = Lexer {
            source: source.as_bytes(),
            index: 0,
        };
        let mut tokens = Vec::new();

        while !lexer.is_at_end() {
            tokens.push(lexer.next());
        }

        // If there isn't already an EOF token, add it.
        if let TokenKind::EOF = tokens.last().unwrap().kind {
        } else {
            tokens.push(build(TokenKind::EOF, ""));
        }

        tokens
    }

    fn next(&mut self) -> Token {
        let mut c = self.peek();

        while c == ' ' || c == '\t' || c == '\r' || c == '\n' {
            self.advance();

            if self.is_at_end() {
                return build(TokenKind::EOF, "");
            } else {
                c = self.peek();
            }
        }

        if c.is_digit(10) {
            return self.next_number_literal();
        }

        if c.is_alphabetic() {
            return self.next_identifier();
        }

        let token = match c {
            '+' => build(TokenKind::Plus, ""),
            '-' => build(TokenKind::Minus, ""),
            '*' => build(TokenKind::Star, ""),
            '/' => build(TokenKind::Slash, ""),
            '^' => build(TokenKind::Power, ""),
            '(' => build(TokenKind::OpenParenthesis, ""),
            ')' => build(TokenKind::ClosedParenthesis, ""),
            '|' => build(TokenKind::Pipe, ""),
            '=' => build(TokenKind::Equals, ""),
            '!' => build(TokenKind::Exclamation, ""),
            ',' => build(TokenKind::Comma, ""),
            _ => build(TokenKind::Unknown, ""),
        };

        self.advance();

        token
    }

    fn next_number_literal(&mut self) -> Token {
        let start = self.index;
        let mut end = start;

        while !self.is_at_end() && (self.peek().is_digit(10) || self.peek() == '.') {
            end += 1;
            self.advance();
        }

        if let Ok(value) = str::from_utf8(&self.source[start..end]) {
            build(TokenKind::Literal, value)
        } else {
            build(TokenKind::Unknown, "")
        }
    }

    fn next_identifier(&mut self) -> Token {
        let start = self.index;
        let mut end = start;

        while !self.is_at_end() && is_valid_identifier(self.peek()) {
            end += 1;
            self.advance();
        }

        if let Ok(value) = str::from_utf8(&self.source[start..end]) {
            let kind = match value {
                "deg" | "°" => TokenKind::Deg,
                "rad" => TokenKind::Rad,
                _ => TokenKind::Identifier,
            };

            build(kind, value)
        } else {
            build(TokenKind::Unknown, "")
        }
    }

    fn peek(&self) -> char {
        self.source[self.index].into()
    }

    fn advance(&mut self) {
        self.index = self.index + 1;
    }

    fn is_at_end(&self) -> bool {
        self.index >= self.source.len()
    }
}

fn build(kind: TokenKind, value: &str) -> Token {
    Token {
        kind,
        value: value.to_string(),
    }
}

fn is_valid_identifier(c: char) -> bool {
    c.is_alphabetic() || c == '°' || c == '√' || c == '\'' || c == '¨' || c == 'Σ'
}
