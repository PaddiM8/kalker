use crate::lexer::TokenKind;

/// A tree structure of a statement.
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    VarDecl(Identifier, Box<Expr>),
    FnDecl(Identifier, Vec<String>, Box<Expr>),
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
    Var(Identifier),
    Group(Box<Expr>),
    FnCall(Identifier, Vec<Expr>),
    Literal(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub full_name: String,
    pub pure_name: String,
    pub prime_count: i32,
}

impl Identifier {
    pub fn from_full_name(full_name: &str) -> Self {
        let (pure_name, prime_count) = separate_identifier_and_prime(full_name);

        Identifier {
            full_name: full_name.to_string(),
            pure_name,
            prime_count,
        }
    }
}

fn separate_identifier_and_prime(identifier: &str) -> (String, i32) {
    let mut prim_count = 0;
    let mut pure_identifier = identifier.to_string();

    loop {
        if pure_identifier.ends_with("'") {
            pure_identifier.pop();
            prim_count += 1;
        } else {
            return (pure_identifier, prim_count);
        }
    }
}
