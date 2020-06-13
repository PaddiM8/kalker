use crate::lexer::TokenKind;
use crate::parser::CalcError;
use crate::parser::Unit;

/// A tree structure of a statement.
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    VarDecl(String, Box<Expr>),
    FnDecl(String, Vec<String>, Box<Expr>),
    UnitDecl(String, String, Box<Expr>),
    /// For simplicity, expressions can be put into statements. This is the form in which expressions are passed to the interpreter.
    Expr(Box<Expr>),
}

/// A tree structure of an expression.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary(Box<Expr>, TokenKind, Box<Expr>),
    Unary(TokenKind, Box<Expr>),
    Unit(String, Box<Expr>),
    Var(String),
    Group(Box<Expr>),
    FnCall(String, Vec<Expr>),
    Literal(String),
}

impl TokenKind {
    pub fn is_unit(&self) -> bool {
        match self {
            TokenKind::Deg | TokenKind::Rad => true,
            _ => false,
        }
    }

    pub fn to_unit(&self) -> Result<Unit, CalcError> {
        match self {
            TokenKind::Deg => Ok(Unit::Degrees),
            TokenKind::Rad => Ok(Unit::Radians),
            _ => Err(CalcError::InvalidUnit),
        }
    }
}
