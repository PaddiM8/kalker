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
        Expr::Unary(op, expr) => invert_unary(target_expr, op, &expr),
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
        TokenKind::Minus => {
            // Eg. a-(b+c)
            // Multiply "-1" into the group, resulting in it becoming a normal expression. Then invert it normally.
            if let Expr::Group(inside_group) = right {
                return invert_binary(
                    target_expr,
                    symbol_table,
                    left,
                    op,
                    &multiply_in(&Expr::Literal(String::from("-1")), inside_group)?,
                );
            }

            TokenKind::Plus
        }
        TokenKind::Star => {
            // If the left expression is a group, multiply the right expression into it, dissolving the group.
            // It can then be inverted normally.
            if let Expr::Group(inside_group) = left {
                return invert(
                    target_expr,
                    symbol_table,
                    &multiply_in(right, inside_group)?,
                );
            }

            // Same as above but left/right switched.
            if let Expr::Group(inside_group) = right {
                return invert(target_expr, symbol_table, &multiply_in(left, inside_group)?);
            }

            TokenKind::Slash
        }
        TokenKind::Slash => {
            // Eg. (a+b)/c
            // Just dissolve the group. Nothing more needs to be done mathematically.
            if let Expr::Group(inside_group) = left {
                return invert(
                    target_expr,
                    symbol_table,
                    &Expr::Binary(inside_group.clone(), op.clone(), Box::new(right.clone())),
                );
            }

            // Eg. a/(b+c)
            // Same as above.
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

    // If the left expression contains the unit, invert the right one instead,
    // since the unit should not be moved.
    if contains_the_unit(left) {
        return Ok(invert(
            Expr::Binary(Box::new(target_expr), op_inv, Box::new(right.clone())),
            symbol_table,
            left,
        )?);
    }

    // Otherwise, invert the left side.
    let final_target_expr = Expr::Binary(Box::new(target_expr), op_inv, Box::new(left.clone()));
    Ok(invert(
        // Eg. 2-a
        // If the operator is minus (and the left expression is being inverted),
        // make the target expression negative to keep balance.
        if let TokenKind::Minus = op {
            Expr::Unary(TokenKind::Minus, Box::new(final_target_expr))
        } else {
            final_target_expr
        },
        symbol_table,
        right, // Then invert the right expression.
    )?)
}

fn invert_unary(target_expr: Expr, op: &TokenKind, expr: &Expr) -> Result<(Expr, Expr), CalcError> {
    match op {
        TokenKind::Minus => Ok((
            Expr::Unary(TokenKind::Minus, Box::new(target_expr)),
            expr.clone(),
        )),
        _ => unimplemented!(),
    }
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
