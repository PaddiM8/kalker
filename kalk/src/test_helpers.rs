#![allow(dead_code)]
use crate::ast::Expr;
use crate::lexer::Token;
use crate::lexer::TokenKind;

pub fn token(kind: TokenKind, value: &str) -> Token {
    Token {
        kind,
        value: value.into(),
        span: (0, 0),
    }
}

pub fn literal(value: &str) -> Box<Expr> {
    Box::new(Expr::Literal(value.into()))
}

pub fn var(identifier: &str) -> Box<Expr> {
    Box::new(Expr::Var(identifier.into()))
}

pub fn binary(left: Box<Expr>, op: TokenKind, right: Box<Expr>) -> Box<Expr> {
    Box::new(Expr::Binary(left, op, right))
}

pub fn unary(op: TokenKind, expr: Box<Expr>) -> Box<Expr> {
    Box::new(Expr::Unary(op, expr))
}

pub fn group(expr: Box<Expr>) -> Box<Expr> {
    Box::new(Expr::Group(expr))
}
