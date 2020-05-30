use crate::lexer::TokenKind;
use std::mem;

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

impl TokenKind {
    pub fn is_unit(&self) -> bool {
        match self {
            TokenKind::Deg | TokenKind::Rad => true,
            _ => false,
        }
    }

    pub fn to_unit(&self) -> Result<Unit, String> {
        match self {
            TokenKind::Deg => Ok(Unit::Degrees),
            TokenKind::Rad => Ok(Unit::Radians),
            _ => Err(String::from("Invalid unit.")),
        }
    }
}

pub fn compare_enums<T>(first: &T, second: &T) -> bool {
    mem::discriminant(first) == mem::discriminant(second)
}
