use std::{collections::HashMap, mem};

use crate::{
    interpreter::Interpreter,
    lexer::{Lexer, Token, TokenKind},
    prelude,
};

#[derive(Debug, Clone)]
pub enum Stmt {
    VarDecl(String, Box<Expr>),
    FnDecl(String, Vec<String>, Box<Expr>),
    Expr(Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Box<Expr>, TokenKind, Box<Expr>),
    Unary(TokenKind, Box<Expr>),
    Unit(Box<Expr>, TokenKind),
    Var(String),
    Group(Box<Expr>),
    FnCall(String, Vec<Expr>),
    Literal(String),
}

#[derive(Debug, Clone)]
pub enum Unit {
    Radians,
    Degrees,
}

pub struct Parser {
    pub angle_unit: Unit,
    tokens: Vec<Token>,
    pos: usize,
    symbol_table: HashMap<String, Stmt>,
}

impl TokenKind {
    pub fn is_unit(&self) -> bool {
        match self {
            TokenKind::Deg | TokenKind::Rad => true,
            _ => false,
        }
    }

    pub fn compare(&self, second_token: &TokenKind) -> bool {
        mem::discriminant(self) == mem::discriminant(second_token)
    }
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            tokens: Vec::new(),
            pos: 0,
            symbol_table: HashMap::new(),
            angle_unit: prelude::DEFAULT_ANGLE_UNIT,
        }
    }

    pub fn parse(&mut self, input: &str) -> Option<f64> {
        self.tokens = Lexer::lex(input);
        self.pos = 0;

        let mut statements: Vec<Stmt> = Vec::new();
        while !self.is_at_end() {
            statements.push(self.parse_stmt());
        }

        Interpreter::new(self.angle_unit.clone(), &mut self.symbol_table).interpret(statements)
    }

    fn parse_stmt(&mut self) -> Stmt {
        if self.match_token(TokenKind::Identifier) {
            return match self.peek_next().kind {
                TokenKind::Equals => self.parse_var_decl_stmt(),
                TokenKind::OpenParenthesis => self.parse_identifier_stmt(),
                _ => Stmt::Expr(Box::new(self.parse_expr())),
            };
        }

        Stmt::Expr(Box::new(self.parse_expr()))
    }

    fn parse_identifier_stmt(&mut self) -> Stmt {
        let began_at = self.pos;
        let primary = self.parse_primary(); // Since function declarations and function calls look the same at first, simply parse a "function call", and re-use the data.

        // If `primary` is followed by an equal sign, it is a function declaration.
        if let TokenKind::Equals = self.peek().kind {
            self.advance();
            let expr = self.parse_expr();

            // Use the "function call" expression that was parsed, and put its values into a function declaration statement instead.
            if let Expr::FnCall(identifier, parameters) = primary {
                let mut parameter_identifiers = Vec::new();

                // All the "arguments" are expected to be parsed as variables,
                // since parameter definitions look the same as variable references.
                // Extract these.
                for parameter in parameters {
                    if let Expr::Var(parameter_identifier) = parameter {
                        parameter_identifiers.push(parameter_identifier);
                    }
                }

                return Stmt::FnDecl(identifier, parameter_identifiers, Box::new(expr));
            }

            panic!("Unexpected error.");
        } else {
            // It is a function call, not a function declaration.
            // Redo the parsing for this specific part.
            self.pos = began_at;
            Stmt::Expr(Box::new(self.parse_expr()))
        }
    }

    fn parse_var_decl_stmt(&mut self) -> Stmt {
        let identifier = self.advance().clone();
        self.advance(); // Equal sign
        let expr = self.parse_expr();

        Stmt::VarDecl(identifier.value, Box::new(expr))
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_sum()
    }

    fn parse_sum(&mut self) -> Expr {
        let mut left = self.parse_factor();

        while self.match_token(TokenKind::Plus) || self.match_token(TokenKind::Minus) {
            let op = self.peek().kind.clone();
            self.advance();
            let right = self.parse_factor();

            left = Expr::Binary(Box::new(left), op, Box::new(right));
        }

        left
    }

    fn parse_factor(&mut self) -> Expr {
        let mut left = self.parse_unary();

        while self.match_token(TokenKind::Star)
            || self.match_token(TokenKind::Slash)
            || self.match_token(TokenKind::Identifier)
        {
            let mut op = self.peek().kind.clone();

            // If the next token is an identifier, assume it's multiplication. Eg. 3y
            if let TokenKind::Identifier = op {
                op = TokenKind::Star;
            } else {
                self.advance();
            }

            let right = self.parse_unary();
            left = Expr::Binary(Box::new(left), op, Box::new(right));
        }

        left
    }

    fn parse_unary(&mut self) -> Expr {
        if self.match_token(TokenKind::Minus) {
            let op = self.advance().kind.clone();
            return Expr::Unary(op, Box::new(self.parse_unary()));
        }

        self.parse_exponent()
    }

    fn parse_exponent(&mut self) -> Expr {
        let left = self.parse_primary();

        if self.match_token(TokenKind::Power) {
            let op = self.advance().kind.clone();
            return Expr::Binary(Box::new(left), op, Box::new(self.parse_exponent()));
        }

        left
    }

    fn parse_primary(&mut self) -> Expr {
        let expr = match self.peek().kind {
            TokenKind::OpenParenthesis => self.parse_group(),
            TokenKind::Pipe => self.parse_abs(),
            TokenKind::Identifier => self.parse_identifier(),
            _ => Expr::Literal(self.advance().value.clone()),
        };

        if !self.is_at_end() && self.peek().kind.is_unit() {
            Expr::Unit(Box::new(expr), self.advance().kind.clone())
        } else {
            expr
        }
    }

    fn parse_group(&mut self) -> Expr {
        self.advance();
        let group_expr = Expr::Group(Box::new(self.parse_expr()));
        self.consume(TokenKind::ClosedParenthesis);

        group_expr
    }

    fn parse_abs(&mut self) -> Expr {
        self.advance();
        let group_expr = Expr::Group(Box::new(self.parse_expr()));
        self.consume(TokenKind::Pipe);

        Expr::FnCall(String::from("abs"), vec![group_expr])
    }

    fn parse_identifier(&mut self) -> Expr {
        let identifier = self.advance().clone();

        if self.match_token(TokenKind::OpenParenthesis) {
            self.advance();

            let mut parameters = Vec::new();
            parameters.push(self.parse_expr());

            while self.match_token(TokenKind::Comma) {
                self.advance();
                parameters.push(self.parse_expr());
            }

            self.consume(TokenKind::ClosedParenthesis);

            Expr::FnCall(identifier.value, parameters)
        } else {
            Expr::Var(identifier.value)
        }
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.pos]
    }

    fn peek_next(&self) -> &Token {
        &self.tokens[self.pos + 1]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.pos - 1]
    }

    fn match_token(&self, kind: TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().kind.compare(&kind)
    }

    fn advance(&mut self) -> &Token {
        self.pos += 1;
        self.previous()
    }

    fn consume(&mut self, kind: TokenKind) -> &Token {
        if self.match_token(kind) {
            return self.advance();
        }

        panic!("Unexpected token.");
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.tokens.len() || self.peek().kind.compare(&TokenKind::EOF)
    }
}
