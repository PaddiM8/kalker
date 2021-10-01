use std::iter::Peekable;
use std::str;
use std::str::Chars;

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum TokenKind {
    Unknown,
    Literal,
    Identifier,

    Plus,
    Minus,
    Star,
    Slash,
    Power,
    Exclamation,
    Percent,
    Tick,
    GreaterThan,
    LessThan,
    Equals,
    NotEquals,
    GreaterOrEquals,
    LessOrEquals,

    UnitKeyword,
    ToKeyword,
    IfKeyword,
    OtherwiseKeyword,

    Pipe,
    OpenCeil,
    ClosedCeil,
    OpenFloor,
    ClosedFloor,
    OpenParenthesis,
    ClosedParenthesis,
    OpenBracket,
    ClosedBracket,
    OpenBrace,
    ClosedBrace,
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
        let eof = build(TokenKind::EOF, "", (self.index, self.index));
        let mut c = if let Some(c) = self.peek() {
            *c
        } else {
            return eof;
        };

        while c == ' ' || c == '\t' || c == '\r' || c == '\n' {
            if let None = self.advance() {
                return eof;
            }

            c = if let Some(c) = self.peek() {
                *c
            } else {
                return eof;
            }
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
            '[' => build(TokenKind::OpenBracket, "", span),
            ']' => build(TokenKind::ClosedBracket, "", span),
            '{' => build(TokenKind::OpenBrace, "", span),
            '}' => build(TokenKind::ClosedBrace, "", span),
            '!' => build(TokenKind::Exclamation, "", span),
            '=' => build(TokenKind::Equals, "", span),
            '>' => build(TokenKind::GreaterThan, "", span),
            '<' => build(TokenKind::LessThan, "", span),
            ',' => build(TokenKind::Comma, "", span),
            ';' => build(TokenKind::Semicolon, "", span),
            '%' => build(TokenKind::Percent, "", span),
            '\'' => build(TokenKind::Tick, "", span),
            '≠' => build(TokenKind::NotEquals, "", span),
            '≥' => build(TokenKind::GreaterOrEquals, "", span),
            '≤' => build(TokenKind::LessOrEquals, "", span),
            // Some of the special symbols will be lexed here,
            // so that they don't merge with other symbols.
            'π' => build(TokenKind::Identifier, "pi", span),
            '√' => build(TokenKind::Identifier, "sqrt", span),
            'τ' => build(TokenKind::Identifier, "tau", span),
            'ϕ' => build(TokenKind::Identifier, "phi", span),
            'Γ' => build(TokenKind::Identifier, "gamma", span),
            '∏' => build(TokenKind::Identifier, "prod", span),
            _ => build(TokenKind::Unknown, "", span),
        };

        self.advance();

        // Handle tokens with two characters
        match (token.kind, self.peek()) {
            (TokenKind::Star, Some('*')) => {
                self.advance();
                return build(TokenKind::Power, "", span);
            }
            (TokenKind::Exclamation, Some('=')) => {
                self.advance();
                return build(TokenKind::NotEquals, "", span);
            }
            (TokenKind::GreaterThan, Some('=')) => {
                self.advance();
                return build(TokenKind::GreaterOrEquals, "", span);
            }
            (TokenKind::LessThan, Some('=')) => {
                self.advance();
                return build(TokenKind::LessOrEquals, "", span);
            }
            _ => (),
        }

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

            if !c.is_digit(10) && c != '.' && !c.is_whitespace() || c == '\n' || c == '\r' {
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
        let mut value = String::new();

        while is_valid_identifier(self.peek()) {
            let c = *self.peek().unwrap();

            // If the current character is an underscore, allow a number next.
            // This is to allow the notation like the following: x_1
            if c == '_' {
                self.advance();
                let num = self.next().value;
                value.push('_');
                value.push_str(&num.trim_end()); // Trim, since the number_literal function allows whitespace, which identifiers should not contain.
                break;
            }

            // Only allow identifiers with a special character to have *one* character. No more.
            // Break the loop if it isn't the first run and the current character is a special character.
            if end - start > 0 && !(c.is_ascii_alphabetic() || c == '\'' || c == '_') {
                break;
            }

            end += 1;
            value.push(c);
            self.advance();
        }

        let kind = match value.as_ref() {
            "unit" => TokenKind::UnitKeyword,
            "to" => TokenKind::ToKeyword,
            "if" => TokenKind::IfKeyword,
            "otherwise" => TokenKind::OtherwiseKeyword,
            _ => TokenKind::Identifier,
        };

        let value = match value.as_ref() {
            "Σ" | "∑" => String::from("sum"),
            "∫" => String::from("integrate"),
            "°" => String::from("deg"),
            _ => value,
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
        match c {
            '+' | '-' | '/' | '*' | '%' | '^' | '!' | '(' | ')' | '=' | '.' | ',' | ';' | '|'
            | '⌊' | '⌋' | '⌈' | '⌉' | '[' | ']' | '{' | '}' | 'π' | '√' | 'τ' | 'ϕ' | 'Γ' | '<'
            | '>' | '≠' | '≥' | '≤' => false,
            _ => !c.is_digit(10),
        }
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;
    use wasm_bindgen_test::*;
    wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

    fn match_tokens(tokens: Vec<Token>, expected: Vec<TokenKind>) {
        let mut expected_iter = expected.iter();

        for token in tokens {
            assert_eq!(token.kind, *expected_iter.next().unwrap());
        }
    }

    #[test]
    #[wasm_bindgen_test]
    fn test_token_kinds() {
        let tokens = Lexer::lex("+-*/%^()|=!,");
        let expected = vec![
            TokenKind::Plus,
            TokenKind::Minus,
            TokenKind::Star,
            TokenKind::Slash,
            TokenKind::Percent,
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

    #[test]
    #[wasm_bindgen_test]
    fn test_brackets() {
        let tokens = Lexer::lex("[1 < 2]");
        let expected = vec![
            TokenKind::OpenBracket,
            TokenKind::Literal,
            TokenKind::LessThan,
            TokenKind::Literal,
            TokenKind::ClosedBracket,
            TokenKind::EOF,
        ];

        match_tokens(tokens, expected);
    }

    #[test]
    #[wasm_bindgen_test]
    fn test_empty() {
        // test_case macro doesn't seem to work with spaces.
        let test_cases = vec![" ", "     ", "test ", " test     "];

        for input in test_cases {
            let tokens = Lexer::lex(input);

            if regex::Regex::new(r"^\s*$").unwrap().is_match(input) {
                let expected = vec![TokenKind::EOF];
                match_tokens(tokens, expected);
            } else {
                let expected = vec![TokenKind::Identifier, TokenKind::EOF];
                match_tokens(tokens, expected);
            }
        }
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
