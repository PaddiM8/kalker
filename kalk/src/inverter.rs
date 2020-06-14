use crate::ast::{Expr, Stmt};
use crate::lexer::TokenKind;
use crate::parser::CalcError;
use crate::parser::DECL_UNIT;
use crate::symbol_table::SymbolTable;

impl Expr {
    pub fn invert(&self, symbol_table: &mut SymbolTable) -> Result<Self, CalcError> {
        let target_expr = Expr::Var(DECL_UNIT.into());
        let result = invert(target_expr, symbol_table, self);
        Ok(result?.0)
    }
}

fn invert(
    target_expr: Expr,
    symbol_table: &mut SymbolTable,
    expr: &Expr,
) -> Result<(Expr, Expr), CalcError> {
    match expr {
        Expr::Binary(left, op, right) => {
            invert_binary(target_expr, symbol_table, &left, op, &right)
        }
        Expr::Unary(_, _) => Ok((target_expr, expr.clone())),
        Expr::Unit(identifier, expr) => invert_unit(target_expr, &identifier, &expr),
        Expr::Var(_) => Ok((target_expr, expr.clone())),
        Expr::Group(expr) => Ok((target_expr, *expr.clone())),
        Expr::FnCall(identifier, arguments) => {
            invert_fn_call(target_expr, symbol_table, &identifier, arguments)
        }
        Expr::Literal(_) => Ok((target_expr, expr.clone())),
    }
}

fn invert_binary(
    target_expr: Expr,
    symbol_table: &mut SymbolTable,
    left: &Expr,
    op: &TokenKind,
    right: &Expr,
) -> Result<(Expr, Expr), CalcError> {
    let op_inv = match op {
        TokenKind::Plus => TokenKind::Minus,
        TokenKind::Minus => TokenKind::Plus,
        TokenKind::Star => {
            if let Expr::Group(inside_group) = left {
                return invert(
                    target_expr,
                    symbol_table,
                    &multiply_in(right, inside_group)?,
                );
            }

            if let Expr::Group(inside_group) = right {
                return invert(target_expr, symbol_table, &multiply_in(left, inside_group)?);
            }

            TokenKind::Slash
        }
        TokenKind::Slash => {
            if let Expr::Group(inside_group) = left {
                return invert(
                    target_expr,
                    symbol_table,
                    &Expr::Binary(inside_group.clone(), op.clone(), Box::new(right.clone())),
                );
            }

            if let Expr::Group(inside_group) = right {
                return invert(
                    target_expr,
                    symbol_table,
                    &Expr::Binary(Box::new(left.clone()), op.clone(), inside_group.clone()),
                );
            }

            TokenKind::Star
        }
        _ => unreachable!(),
    };

    if contains_the_unit(left) {
        return Ok(invert(
            Expr::Binary(Box::new(target_expr), op_inv, Box::new(right.clone())),
            symbol_table,
            left,
        )?);
    }

    Ok(invert(
        Expr::Binary(Box::new(target_expr), op_inv, Box::new(left.clone())),
        symbol_table,
        right,
    )?)
}

// Not necessary yet
fn invert_unit(
    _target_expr: Expr,
    _identifier: &str,
    _expr: &Expr,
) -> Result<(Expr, Expr), CalcError> {
    unimplemented!()
}

fn invert_fn_call(
    target_expr: Expr,
    symbol_table: &mut SymbolTable,
    identifier: &str,
    arguments: &Vec<Expr>,
) -> Result<(Expr, Expr), CalcError> {
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

    invert(target_expr, symbol_table, &body)
}

fn contains_the_unit(expr: &Expr) -> bool {
    match expr {
        Expr::Binary(left, _, right) => contains_the_unit(left) || contains_the_unit(right),
        Expr::Unary(_, expr) => contains_the_unit(expr),
        Expr::Unit(_, expr) => contains_the_unit(expr),
        Expr::Var(identifier) => identifier == DECL_UNIT,
        Expr::Group(expr) => contains_the_unit(expr),
        Expr::FnCall(_, args) => {
            for arg in args {
                if contains_the_unit(arg) {
                    return true;
                }
            }

            false
        }
        Expr::Literal(_) => false,
    }
}

fn multiply_in(expr: &Expr, base_expr: &Expr) -> Result<Expr, CalcError> {
    match base_expr {
        Expr::Binary(left, op, right) => match op {
            TokenKind::Plus | TokenKind::Minus => Ok(Expr::Binary(
                Box::new(multiply_in(expr, &left)?),
                op.clone(),
                Box::new(multiply_in(expr, &right)?),
            )),
            TokenKind::Star | TokenKind::Slash => Ok(Expr::Binary(
                Box::new(multiply_in(expr, &left)?),
                op.clone(),
                right.clone(),
            )),
            _ => unimplemented!(),
        },
        Expr::Literal(_) | Expr::Var(_) => Ok(Expr::Binary(
            Box::new(expr.clone()),
            TokenKind::Star,
            Box::new(base_expr.clone()),
        )),
        _ => unimplemented!(),
    }
}
