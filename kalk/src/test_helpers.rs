#![allow(dead_code)]
use crate::ast::Expr;
use crate::ast::Stmt;
use crate::lexer::Token;
use crate::lexer::TokenKind;

pub fn token(kind: TokenKind, value: &str) -> Token {
    Token {
        kind,
        value: value.into(),
        span: (0, 0),
    }
}

pub fn literal(value: f64) -> Box<Expr> {
    Box::new(Expr::Literal(value))
}

pub fn var(identifier: &str) -> Box<Expr> {
    Box::new(Expr::Var(identifier.into()))
}

pub fn fn_call(identifier: &str, arguments: Vec<Expr>) -> Box<Expr> {
    Box::new(Expr::FnCall(identifier.into(), arguments))
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

pub fn unit(identifier: &str, expr: Box<Expr>) -> Box<Expr> {
    Box::new(Expr::Unit(identifier.into(), expr))
}

pub fn var_decl(identifier: &str, value: Box<Expr>) -> Stmt {
    Stmt::VarDecl(identifier.into(), value)
}

pub fn fn_decl(identifier: &str, parameters: Vec<String>, value: Box<Expr>) -> Stmt {
    Stmt::FnDecl(identifier.into(), parameters, value)
}

pub fn unit_decl(unit: &str, base_unit: &str, expr: Box<Expr>) -> Stmt {
    Stmt::UnitDecl(unit.into(), base_unit.into(), expr)
}
