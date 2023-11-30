#![allow(dead_code)]
use crate::ast::Expr;
use crate::ast::Identifier;
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

pub fn cmp(x: f64, y: f64) -> bool {
    (x - y).abs() < 0.0001
}

pub fn f64_to_float_literal(x: f64) -> Box<Expr> {
    literal(crate::float!(x))
}

#[cfg(feature = "rug")]
pub fn literal(value: rug::Float) -> Box<Expr> {
    Box::new(Expr::Literal(value))
}
#[cfg(not(feature = "rug"))]
pub fn literal(value: f64) -> Box<Expr> {
    Box::new(Expr::Literal(value))
}

pub fn var(identifier: &str) -> Box<Expr> {
    Box::new(Expr::Var(Identifier::from_full_name(identifier)))
}

pub fn param_var(function: &str, identifier: &str) -> Box<Expr> {
    Box::new(Expr::Var(Identifier::parameter_from_name(
        identifier, function,
    )))
}

pub fn fn_call(identifier: &str, arguments: Vec<Expr>) -> Box<Expr> {
    Box::new(Expr::FnCall(
        Identifier::from_full_name(identifier),
        arguments,
    ))
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
    Stmt::VarDecl(Identifier::from_full_name(identifier), value)
}

pub fn fn_decl(identifier: &str, parameters: Vec<String>, value: Box<Expr>) -> Stmt {
    Stmt::FnDecl(Identifier::from_full_name(identifier), parameters, value)
}

pub fn unit_decl(unit: &str, base_unit: &str, expr: Box<Expr>) -> Stmt {
    Stmt::UnitDecl(unit.into(), base_unit.into(), expr)
}
