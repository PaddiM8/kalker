use crate::{kalk_value::{KalkFloat, KalkValue}, lexer::TokenKind};

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
    Literal(KalkFloat),
    Boolean(bool),
    Piecewise(Vec<ConditionalPiece>),
    Vector(Vec<Expr>),
    Matrix(Vec<Vec<Expr>>),
    Indexer(Box<Expr>, Vec<Expr>),
    Comprehension(Box<Expr>, Vec<Expr>, Vec<RangedVar>),
    Equation(Box<Expr>, Box<Expr>, Identifier),
    Preevaluated(KalkValue),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalPiece {
    pub expr: Expr,
    pub condition: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RangedVar {
    pub name: String,
    pub max: Expr,
    pub min: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub full_name: String,
    pub pure_name: String,
    pub parameter_of_function: Option<String>,
    pub prime_count: u32,
}

impl Identifier {
    pub fn from_full_name(full_name: &str) -> Self {
        let (pure_name, prime_count) = separate_identifier_and_prime(full_name);

        Identifier {
            full_name: full_name.to_string(),
            pure_name,
            parameter_of_function: None,
            prime_count,
        }
    }

    pub fn from_name_and_primes(pure_name: &str, prime_count: u32) -> Self {
        Identifier {
            full_name: format!("{}{}", pure_name, "'".repeat(prime_count as usize)),
            pure_name: pure_name.into(),
            parameter_of_function: None,
            prime_count,
        }
    }

    pub fn parameter_from_name(name: &str, function: &str) -> Self {
        Identifier {
            full_name: format!("{}-{}", function, name),
            pure_name: name.into(),
            parameter_of_function: Some(function.into()),
            prime_count: 0u32,
        }
    }

    pub fn get_name_without_lowered(&self) -> &str {
        if let Some(underscore_pos) = self.pure_name.find('_') {
            &self.pure_name[0..underscore_pos]
        } else {
            &self.pure_name
        }
    }

    pub fn get_lowered_part(&self) -> Option<&str> {
        if let Some(underscore_pos) = self.pure_name.find('_') {
            Some(&self.pure_name[underscore_pos + 1..])
        } else {
            None
        }
    }
}

pub fn build_literal_ast(kalk_value: &crate::kalk_value::KalkValue) -> Expr {
    if kalk_value.has_imaginary() {
        Expr::Binary(
            Box::new(Expr::Literal(kalk_value.to_float())),
            TokenKind::Plus,
            Box::new(Expr::Binary(
                Box::new(Expr::Literal(kalk_value.imaginary_to_float())),
                TokenKind::Star,
                Box::new(Expr::Var(Identifier::from_full_name("i"))),
            )),
        )
    } else {
        Expr::Literal(kalk_value.to_float())
    }
}

fn separate_identifier_and_prime(identifier: &str) -> (String, u32) {
    let mut prim_count = 0;
    let mut pure_identifier = identifier.to_string();

    loop {
        if pure_identifier.ends_with('\'') {
            pure_identifier.pop();
            prim_count += 1;
        } else {
            return (pure_identifier, prim_count);
        }
    }
}
