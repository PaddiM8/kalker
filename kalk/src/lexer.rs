use std::str;

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

    Deg,
    Rad,

    Pipe,
    OpenParenthesis,
    ClosedParenthesis,
    Comma,

    EOF,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub span: (usize, usize),
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
            tokens.push(build(TokenKind::EOF, "", (source.len(), source.len())));
        }

        tokens
    }

    fn next(&mut self) -> Token {
        let mut c = self.peek();

        while c == ' ' || c == '\t' || c == '\r' || c == '\n' {
            self.advance();

            if self.is_at_end() {
                return build(TokenKind::EOF, "", (self.index, self.index));
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

        let span = (self.index, self.index + 1);
        let token = match c {
            '+' => build(TokenKind::Plus, "", span),
            '-' => build(TokenKind::Minus, "", span),
            '*' => build(TokenKind::Star, "", span),
            '/' => build(TokenKind::Slash, "", span),
            '^' => build(TokenKind::Power, "", span),
            '(' => build(TokenKind::OpenParenthesis, "", span),
            ')' => build(TokenKind::ClosedParenthesis, "", span),
            '|' => build(TokenKind::Pipe, "", span),
            '=' => build(TokenKind::Equals, "", span),
            '!' => build(TokenKind::Exclamation, "", span),
            ',' => build(TokenKind::Comma, "", span),
            _ => build(TokenKind::Unknown, "", span),
        };

        self.advance();

        token
    }

    fn next_number_literal(&mut self) -> Token {
        let start = self.index;
        let mut end = start;

        while !self.is_at_end()
            && (self.peek().is_digit(10) || self.peek() == '.' || self.peek().is_whitespace())
        {
            end += 1;
            self.advance();
        }

        if let Ok(value) = str::from_utf8(&self.source[start..end]) {
            build(TokenKind::Literal, value, (start, end))
        } else {
            build(TokenKind::Unknown, "", (self.index, self.index))
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

            build(kind, value, (start, end))
        } else {
            build(TokenKind::Unknown, "", (self.index, self.index))
        }
    }

    fn peek(&self) -> char {
        self.source[self.index].into()
    }

    fn advance(&mut self) {
        self.index += 1;
    }

    fn is_at_end(&self) -> bool {
        self.index >= self.source.len()
    }
}

fn build(kind: TokenKind, value: &str, span: (usize, usize)) -> Token {
    Token {
        kind,
        value: value.to_string(),
        span,
    }
}

fn is_valid_identifier(c: char) -> bool {
    c.is_alphabetic() || c == '°' || c == '√' || c == '\'' || c == '¨' || c == 'Σ'
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
