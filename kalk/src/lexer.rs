use crate::text_utils::{is_subscript, is_superscript};
use std::iter::Peekable;
use std::str;
use std::str::Chars;

#[derive(Clone, Debug, PartialEq, Eq, Copy)]
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
    And,
    Or,
    Not,
    True,
    False,

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
    OpenDoubleBracket,
    ClosedBracket,
    ClosedDoubleBracket,
    OpenBrace,
    ClosedBrace,
    Comma,
    Colon,
    Semicolon,
    Newline,

    Eof,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub span: (usize, usize),
}

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    index: usize,
    other_radix: Option<u8>,
    buffer: Option<char>,
    has_backtracked: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            chars: source.chars().peekable(),
            index: 0,
            other_radix: None,
            buffer: None,
            has_backtracked: false,
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            let mut next = self.next();
            if next.kind == TokenKind::Power && !next.value.is_empty() {
                let value: String = next.value.drain(..).collect();
                let span = next.span;
                tokens.push(next);
                tokens.push(build(TokenKind::Identifier, &value, span));
            } else if TokenKind::Eof == next.kind {
                tokens.push(next);
                break;
            } else {
                tokens.push(next);
            }
        }

        tokens
    }

    pub fn get_other_radix(&self) -> Option<u8> {
        self.other_radix
    }

    fn next(&mut self) -> Token {
        let eof = build(TokenKind::Eof, "", (self.index, self.index));
        let mut c = if let Some(c) = self.peek() {
            c
        } else {
            return eof;
        };

        while c == ' ' || c == '\t' || c == '\r' {
            if self.advance().is_none() {
                return eof;
            }

            c = if let Some(c) = self.peek() {
                c
            } else {
                return eof;
            }
        }

        if c.is_ascii_digit() || c == '.' {
            return self.next_number_literal();
        }

        if is_valid_identifier(Some(c)) {
            return self.next_identifier();
        }

        let span = (self.index, self.index + 1);
        let token = match c {
            '+' => build(TokenKind::Plus, "", span),
            '-' => build(TokenKind::Minus, "", span),
            '*' | '×' | '⋅' => build(TokenKind::Star, "", span),
            '/' | '÷' => build(TokenKind::Slash, "", span),
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
            '⟦' => build(TokenKind::OpenDoubleBracket, "", span),
            '⟧' => build(TokenKind::ClosedDoubleBracket, "", span),
            '{' => build(TokenKind::OpenBrace, "", span),
            '}' => build(TokenKind::ClosedBrace, "", span),
            '!' => build(TokenKind::Exclamation, "", span),
            '=' => build(TokenKind::Equals, "", span),
            '>' => build(TokenKind::GreaterThan, "", span),
            '<' => build(TokenKind::LessThan, "", span),
            '∧' => build(TokenKind::And, "", span),
            '∨' => build(TokenKind::Or, "", span),
            '¬' => build(TokenKind::Not, "", span),
            ',' => build(TokenKind::Comma, "", span),
            ':' => build(TokenKind::Colon, "", span),
            ';' => build(TokenKind::Semicolon, "", span),
            '\n' => build(TokenKind::Newline, "", span),
            '%' => build(TokenKind::Percent, "", span),
            '\'' => build(TokenKind::Tick, "", span),
            '≠' => build(TokenKind::NotEquals, "", span),
            '≥' => build(TokenKind::GreaterOrEquals, "", span),
            '≤' => build(TokenKind::LessOrEquals, "", span),
            // A bit hacky. When the result is handled, this token is turned into two tokens
            'ᵀ' => build(TokenKind::Power, "T", span),
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
            (TokenKind::Star, Some('⋅')) => {
                self.advance();
                return build(TokenKind::Power, "", span);
            }
            (TokenKind::OpenBracket, Some('[')) => {
                self.advance();
                return build(TokenKind::OpenDoubleBracket, "", span);
            }
            (TokenKind::ClosedBracket, Some(']')) => {
                self.advance();
                return build(TokenKind::ClosedDoubleBracket, "", span);
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
        let mut start = self.index;
        let mut end = start;
        let mut value = String::new();
        let mut leading_zero = self.peek().unwrap_or('\0') == '0';
        let mut base = 10u8;
        let mut is_e_notation = false;

        while let Some(c) = self.peek() {
            let c = c.clone();
            // If at the second character and
            // the first character is a zero,
            // allow a letter
            if end - start == 1 && leading_zero {
                base = match c {
                    'b' => 2,
                    'o' => 8,
                    'x' => 16,
                    _ => 10,
                };

                // Don't include eg. 0x in the value
                if base != 10 {
                    start += 2;
                    end += 1;
                    self.advance();
                    value.clear();
                    leading_zero = false;
                    continue;
                }
            }

            if is_e_notation && c == 'E' {
                break;
            }

            if end != start && c == 'E' {
                is_e_notation = true;
                end += 1;
                value.push(c);
                self.advance();

                if let Some('-') = self.peek() {
                    end += 1;
                    value.push('-');
                    self.advance();
                } else if !self.peek().unwrap_or('\0').is_ascii_digit() {
                    end -= 1;
                    value.pop();
                    self.backtrack();
                    break;
                }

                continue;
            }

            if !c.is_digit(base as u32) && c != '.' && c != '_' && !c.is_whitespace()
                || c == '\n'
                || c == '\r'
            {
                break;
            }

            end += 1;
            value.push(c);
            self.advance();
        }

        // Subscript unicode symbols after the literal, eg. 11₂
        let mut base_str = String::new();
        while crate::text_utils::is_subscript(&self.peek().unwrap_or('\0')) {
            base_str.push(self.peek().unwrap());
            self.advance();
        }

        if !base_str.is_empty() {
            base = crate::text_utils::subscript_to_normal(base_str.chars())
                .parse::<u8>()
                .unwrap_or(10);
        }

        if base != 10 {
            value.push_str(&format!("_{}", base));
            if let Some(other_radix) = self.other_radix {
                // Don't bother keeping track of radixes
                // if several different ones are used
                if other_radix != base {
                    self.other_radix = None;
                }
            } else {
                self.other_radix = Some(base);
            }
        }

        build(TokenKind::Literal, &value, (start, end))
    }

    fn next_identifier(&mut self) -> Token {
        let start = self.index;
        let mut end = start;
        let mut value = String::new();
        let mut subscript = String::new();

        while is_valid_identifier(self.peek()) {
            let c = self.peek().unwrap();

            // If the current character is an underscore, allow a number next.
            // This is to allow the notation like the following: x_1
            if c == '_' {
                self.advance();
                let num = self.next().value;
                value.push('_');
                value.push_str(num.trim_end()); // Trim, since the number_literal function allows whitespace, which identifiers should not contain.
                break;
            }

            // Only allow identifiers with a special character to have *one* character. No more.
            // Break the loop if it isn't the first run and the current character is a special character.
            if end - start > 0
                && !(c.is_ascii_alphabetic()
                    || c == '\''
                    || c == '_'
                    || is_superscript(&c)
                    || is_subscript(&c))
            {
                break;
            }

            if is_subscript(&c) {
                subscript.push(c);
            } else {
                value.push(c);
            }

            end += 1;
            self.advance();
        }

        let kind = match value.as_ref() {
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "not" => TokenKind::Not,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "mod" => TokenKind::Percent,
            "unit" => TokenKind::UnitKeyword,
            "to" => TokenKind::ToKeyword,
            "if" => TokenKind::IfKeyword,
            "otherwise" => TokenKind::OtherwiseKeyword,
            _ => TokenKind::Identifier,
        };

        let value = match value.as_ref() {
            "Σ" | "∑" => String::from("sum"),
            "∏" => String::from("prod"),
            "∫" | "integral" => String::from("integrate"),
            "sin⁻¹" => String::from("asin"),
            "cos⁻¹" => String::from("acos"),
            "tan⁻¹" => String::from("atan"),
            "cot⁻¹" => String::from("acot"),
            "cosec⁻¹" => String::from("acosec"),
            "sec⁻¹" => String::from("asec"),
            "sinh⁻¹" => String::from("asinh"),
            "cosh⁻¹" => String::from("acosh"),
            "tanh⁻¹" => String::from("atanh"),
            "coth⁻¹" => String::from("acoth"),
            "cosech⁻¹" => String::from("acosech"),
            "sech⁻¹" => String::from("asech"),
            "∛" => String::from("cbrt"),
            "°" => String::from("deg"),
            _ => value, // things like log_2 are handled in the parser
        };

        if !subscript.is_empty() {
            build(
                kind,
                &format!(
                    "{}_{}",
                    value,
                    crate::text_utils::subscript_to_normal(subscript.chars())
                ),
                (start, end),
            )
        } else {
            build(kind, &value, (start, end))
        }
    }

    fn peek(&mut self) -> Option<char> {
        if self.has_backtracked {
            self.buffer
        } else {
            self.chars.peek().copied()
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.index += 1;
        if self.has_backtracked {
            self.has_backtracked = false;

            return self.buffer;
        }

        self.buffer = self.peek();
        self.chars.next()
    }

    fn backtrack(&mut self) {
        self.has_backtracked = true;
        self.index -= 1;
    }
}

fn build(kind: TokenKind, value: &str, span: (usize, usize)) -> Token {
    Token {
        kind,
        value: value.to_string(),
        span,
    }
}

fn is_valid_identifier(c: Option<char>) -> bool {
    if let Some(c) = c {
        match c {
            '+' | '-' | '/' | '*' | '%' | '^' | '!' | '(' | ')' | '=' | '.' | ',' | ';' | '|'
            | '⌊' | '⌋' | '⌈' | '⌉' | '[' | ']' | '{' | '}' | 'π' | '√' | 'τ' | 'ϕ' | 'Γ' | '<'
            | '>' | '≠' | '≥' | '≤' | '×' | '÷' | '⋅' | '⟦' | '⟧' | '∧' | '∨' | '¬' | ':' | 'ᵀ'
            | '\n' => false,
            _ => !c.is_ascii_digit() || is_superscript(&c) || is_subscript(&c),
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
        let tokens = Lexer::new("+-*/%^()|=!,").lex();
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
            TokenKind::Eof,
        ];

        match_tokens(tokens, expected);
    }

    #[test]
    #[wasm_bindgen_test]
    fn test_brackets() {
        let tokens = Lexer::new("[1 < 2]").lex();
        let expected = vec![
            TokenKind::OpenBracket,
            TokenKind::Literal,
            TokenKind::LessThan,
            TokenKind::Literal,
            TokenKind::ClosedBracket,
            TokenKind::Eof,
        ];

        match_tokens(tokens, expected);
    }

    #[test]
    #[wasm_bindgen_test]
    fn test_empty() {
        // test_case macro doesn't seem to work with spaces.
        let test_cases = vec![" ", "     ", "test ", " test     "];

        for input in test_cases {
            let tokens = Lexer::new(input).lex();

            if regex::Regex::new(r"^\s*$").unwrap().is_match(input) {
                let expected = vec![TokenKind::Eof];
                match_tokens(tokens, expected);
            } else {
                let expected = vec![TokenKind::Identifier, TokenKind::Eof];
                match_tokens(tokens, expected);
            }
        }
    }

    #[test_case("1")]
    #[test_case("24")]
    #[test_case("56.4")]
    fn test_number_literal(input: &str) {
        let tokens = Lexer::new(input).lex();
        let expected = vec![TokenKind::Literal, TokenKind::Eof];

        assert_eq!(&tokens[0].value, input);
        match_tokens(tokens, expected);
    }

    #[test_case("x")]
    #[test_case("xy")]
    fn test_identifier(input: &str) {
        let tokens = Lexer::new(input).lex();
        let expected = vec![TokenKind::Identifier, TokenKind::Eof];

        assert_eq!(&tokens[0].value, input);
        match_tokens(tokens, expected);
    }

    #[test]
    fn test_function_call() {
        let tokens = Lexer::new("f(x)").lex();
        let expected = vec![
            TokenKind::Identifier,
            TokenKind::OpenParenthesis,
            TokenKind::Identifier,
            TokenKind::ClosedParenthesis,
            TokenKind::Eof,
        ];

        match_tokens(tokens, expected);
    }
}
