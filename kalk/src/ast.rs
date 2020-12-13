use crate::lexer::TokenKind;

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
    Literal(f64),
}
