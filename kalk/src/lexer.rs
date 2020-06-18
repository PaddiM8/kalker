use std::iter::Peekable;
use std::str;
use std::str::Chars;

#[derive(Clone, Debug, PartialEq)]
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

    UnitKeyword,
    ToKeyword,

    Pipe,
    OpenCeil,
    ClosedCeil,
    OpenFloor,
    ClosedFloor,
    OpenParenthesis,
    ClosedParenthesis,
    Comma,
    Semicolon,

    EOF,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub span: (usize, usize),
}

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    index: usize,
}

impl<'a> Lexer<'a> {
    pub fn lex(source: &str) -> Vec<Token> {
        let mut lexer = Lexer {
            chars: source.chars().peekable(),
            index: 0,
        };
        let mut tokens = Vec::new();

        loop {
            let next = lexer.next();

            if let TokenKind::EOF = next.kind {
                tokens.push(next);
                break;
            } else {
                tokens.push(next);
            }
        }

        tokens
    }

    fn next(&mut self) -> Token {
        let mut c = if let Some(c) = self.peek() {
            *c
        } else {
            return build(TokenKind::EOF, "", (self.index, self.index));
        };

        while c == ' ' || c == '\t' || c == '\r' || c == '\n' {
            if let None = self.advance() {
                return build(TokenKind::EOF, "", (self.index, self.index));
            }

            c = *self.peek().unwrap();
        }

        if c.is_digit(10) {
            return self.next_number_literal();
        }

        if is_valid_identifier(Some(&c)) {
            return self.next_identifier();
        }

        let span = (self.index, self.index + 1);
        let token = match c {
            '+' => build(TokenKind::Plus, "", span),
            '-' => build(TokenKind::Minus, "", span),
            '*' => build(TokenKind::Star, "", span),
            '/' => build(TokenKind::Slash, "", span),
            '^' => build(TokenKind::Power, "", span),
            '|' => build(TokenKind::Pipe, "", span),
            '⌈' => build(TokenKind::OpenCeil, "", span),
            '⌉' => build(TokenKind::ClosedCeil, "", span),
            '⌊' => build(TokenKind::OpenFloor, "", span),
            '⌋' => build(TokenKind::ClosedFloor, "", span),
            '(' => build(TokenKind::OpenParenthesis, "", span),
            ')' => build(TokenKind::ClosedParenthesis, "", span),
            '=' => build(TokenKind::Equals, "", span),
            '!' => build(TokenKind::Exclamation, "", span),
            ',' => build(TokenKind::Comma, "", span),
            ';' => build(TokenKind::Semicolon, "", span),
            _ => build(TokenKind::Unknown, "", span),
        };

        self.advance();

        token
    }

    fn next_number_literal(&mut self) -> Token {
        let start = self.index;
        let mut end = start;
        let mut value = String::new();

        loop {
            let c = if let Some(c) = self.peek() {
                *c
            } else {
                break;
            };

            if !c.is_digit(10) && c != '.' && !c.is_whitespace() {
                break;
            }

            end += 1;
            value.push(c);
            self.advance();
        }

        build(TokenKind::Literal, &value, (start, end))
    }

    fn next_identifier(&mut self) -> Token {
        let start = self.index;
        let mut end = start;
        let letter_reg = regex::Regex::new(r"[A-z'_]").unwrap();
        let mut value = String::new();

        while is_valid_identifier(self.peek()) {
            let c = *self.peek().unwrap();

            // If the current character is an underscore, expect a number next.
            // This is to allow the notation like the following: x_1
            if c == '_' {
                self.advance();
                let num = self.next_number_literal().value;
                value.push('_');
                value.push_str(&num.trim_end()); // Trim, since the number_literal function allows whitespace, which identifiers should not contain.
                break;
            }

            // Only allow identifiers with a special character to have *one* character. No more.
            // Break the loop if it isn't the first run and the current character is a special character.
            if end - start > 0 && !letter_reg.is_match(&c.to_string()) {
                break;
            }

            end += 1;
            value.push(c);
            self.advance();
        }

        let kind = match value.as_ref() {
            "unit" => TokenKind::UnitKeyword,
            "to" => TokenKind::ToKeyword,
            _ => TokenKind::Identifier,
        };

        build(kind, &value, (start, end))
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn advance(&mut self) -> Option<char> {
        self.index += 1;
        self.chars.next()
    }
}

fn build(kind: TokenKind, value: &str, span: (usize, usize)) -> Token {
    Token {
        kind,
        value: value.to_string(),
        span,
    }
}

fn is_valid_identifier(c: Option<&char>) -> bool {
    if let Some(c) = c {
        regex::Regex::new(r"[^\s\n\r0-9\+-/\*\^!\(\)=\.,;|⌊⌋⌈⌉]")
            .unwrap()
            .is_match(&c.to_string())
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    fn match_tokens(tokens: Vec<Token>, expected: Vec<TokenKind>) {
        let mut expected_iter = expected.iter();

        for token in tokens {
            assert_eq!(token.kind, *expected_iter.next().unwrap());
        }
    }

    #[test]
    fn test_token_kinds() {
        let tokens = Lexer::lex("+-*/^()|=!,");
        let expected = vec![
            TokenKind::Plus,
            TokenKind::Minus,
            TokenKind::Star,
            TokenKind::Slash,
            TokenKind::Power,
            TokenKind::OpenParenthesis,
            TokenKind::ClosedParenthesis,
            TokenKind::Pipe,
            TokenKind::Equals,
            TokenKind::Exclamation,
            TokenKind::Comma,
            TokenKind::EOF,
        ];

        match_tokens(tokens, expected);
    }

    #[test_case("1")]
    #[test_case("24")]
    #[test_case("56.4")]
    fn test_number_literal(input: &str) {
        let tokens = Lexer::lex(input);
        let expected = vec![TokenKind::Literal, TokenKind::EOF];

        assert_eq!(&tokens[0].value, input);
        match_tokens(tokens, expected);
    }

    #[test_case("x")]
    #[test_case("xy")]
    fn test_identifier(input: &str) {
        let tokens = Lexer::lex(input);
        let expected = vec![TokenKind::Identifier, TokenKind::EOF];

        assert_eq!(&tokens[0].value, input);
        match_tokens(tokens, expected);
    }

    #[test]
    fn test_function_call() {
        let tokens = Lexer::lex("f(x)");
        let expected = vec![
            TokenKind::Identifier,
            TokenKind::OpenParenthesis,
            TokenKind::Identifier,
            TokenKind::ClosedParenthesis,
            TokenKind::EOF,
        ];

        match_tokens(tokens, expected);
    }
}
