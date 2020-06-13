use crate::ast::{Expr, Stmt};
use crate::lexer::TokenKind;
use crate::parser::CalcError;
use crate::symbol_table::SymbolTable;

impl Expr {
    pub fn invert(&self, symbol_table: &mut SymbolTable) -> Result<Self, CalcError> {
        match self {
            Expr::Binary(left, op, right) => invert_binary(symbol_table, &left, op, &right),
            Expr::Unary(op, expr) => invert_unary(op, &expr),
            Expr::Unit(identifier, expr) => invert_unit(&identifier, &expr),
            Expr::Var(_) => invert_value(self),
            Expr::Group(expr) => invert_group(&expr),
            Expr::FnCall(identifier, arguments) => {
                invert_fn_call(symbol_table, &identifier, arguments)
            }
            Expr::Literal(_) => invert_value(self),
        }
    }
}

fn invert_binary(
    symbol_table: &mut SymbolTable,
    left: &Expr,
    op: &TokenKind,
    right: &Expr,
) -> Result<Expr, CalcError> {
    let op_inv = match op {
        TokenKind::Plus => TokenKind::Minus,
        TokenKind::Minus => TokenKind::Plus,
        TokenKind::Star => TokenKind::Slash,
        TokenKind::Slash => TokenKind::Star,
        _ => unreachable!(),
    };

    Ok(Expr::Binary(
        Box::new(right.invert(symbol_table)?),
        op_inv,
        Box::new(left.invert(symbol_table)?),
    ))
}

fn invert_unary(op: &TokenKind, expr: &Expr) -> Result<Expr, CalcError> {
    Ok(match op {
        TokenKind::Minus => expr.clone(),
        TokenKind::Exclamation => unimplemented!(),
        _ => unreachable!(),
    })
}

// Not necessary yet
fn invert_unit(_identifier: &str, _expr: &Expr) -> Result<Expr, CalcError> {
    unimplemented!()
}

fn invert_group(expr: &Expr) -> Result<Expr, CalcError> {
    invert_value(expr)
}

fn invert_fn_call(
    symbol_table: &mut SymbolTable,
    identifier: &str,
    arguments: &Vec<Expr>,
) -> Result<Expr, CalcError> {
    let (parameters, body) =
        if let Some(Stmt::FnDecl(_, parameters, body)) = symbol_table.get_fn(identifier).cloned() {
            (parameters, body)
        } else {
            return Err(CalcError::UndefinedFn(identifier.into()));
        };

    if parameters.len() != arguments.len() {
        return Err(CalcError::IncorrectAmountOfArguments(
            parameters.len(),
            identifier.into(),
            arguments.len(),
        ));
    }

    let mut parameters_iter = parameters.iter();
    for argument in arguments {
        symbol_table.insert(Stmt::VarDecl(
            parameters_iter.next().unwrap().to_string(),
            Box::new(argument.clone()),
        ));
    }

    body.invert(symbol_table)
}

fn invert_value(expr: &Expr) -> Result<Expr, CalcError> {
    Ok(Expr::Unary(TokenKind::Minus, Box::new(expr.clone())))
}
