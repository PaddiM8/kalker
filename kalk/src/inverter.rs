use crate::ast::{Expr, Stmt};
use crate::lexer::TokenKind;
use crate::parser::CalcError;
use crate::parser::DECL_UNIT;
use crate::prelude;
use crate::symbol_table::SymbolTable;

pub const INVERSE_UNARY_FUNCS: phf::Map<&'static str, &'static str> = phf::phf_map! {
    "cos" => "acos",
    "cosec" => "acosec",
    "cosech" => "cosech",
    "cosh" => "acosh",
    "cot" => "acot",
    "coth" => "acoth",
    "sec" => "asec",
    "sech" => "asech",
    "sin" => "asin",
    "sinh" => "asinh",
    "tan" => "atan",
    "tanh" => "atanh",

    "acos" => "cos",
    "acosec" => "cosec",
    "acosech" => "cosech",
    "acosh" => "cosh",
    "acot" => "cot",
    "acoth" => "coth",
    "asec" => "sec",
    "asech" => "sech",
    "asin" => "sin",
    "asinh" => "sinh",
    "atan" => "tan",
    "atanh" => "tanh",
};

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
        Expr::Var(identifier) => invert_var(target_expr, symbol_table, identifier),
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
                    &TokenKind::Plus,
                    &multiply_into(&Expr::Literal(String::from("-1")), inside_group)?,
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
                    &multiply_into(right, inside_group)?,
                );
            }

            // Same as above but left/right switched.
            if let Expr::Group(inside_group) = right {
                return invert(
                    target_expr,
                    symbol_table,
                    &multiply_into(left, inside_group)?,
                );
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
    if contains_the_unit(symbol_table, left) {
        // But if the right expression *also* contains the unit,
        // throw an error, since it can't handle this yet.
        if contains_the_unit(symbol_table, right) {
            return Err(CalcError::UnableToInvert(String::from(
                "Expressions with several instances of an unknown variable (this might be supported in the future). Try simplifying the expression.",
            )));
        }

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
            // Make the target expression negative
            Expr::Unary(TokenKind::Minus, Box::new(target_expr)),
            expr.clone(), // And then continue inverting the inner-expression.
        )),
        _ => unimplemented!(),
    }
}

fn invert_unit(
    _target_expr: Expr,
    _identifier: &str,
    _expr: &Expr,
) -> Result<(Expr, Expr), CalcError> {
    Err(CalcError::UnableToInvert(String::from(
        "Expressions containing other units (this should be supported in the future).",
    )))
}

fn invert_var(
    target_expr: Expr,
    symbol_table: &mut SymbolTable,
    identifier: &str,
) -> Result<(Expr, Expr), CalcError> {
    if let Some(Stmt::VarDecl(_, var_expr)) = symbol_table.get_var(identifier).cloned() {
        invert(target_expr, symbol_table, &var_expr)
    } else {
        Ok((target_expr, Expr::Var(identifier.into())))
    }
}

fn invert_fn_call(
    target_expr: Expr,
    symbol_table: &mut SymbolTable,
    identifier: &str,
    arguments: &Vec<Expr>,
) -> Result<(Expr, Expr), CalcError> {
    // If prelude function
    match arguments.len() {
        1 => {
            if prelude::UNARY_FUNCS.contains_key(identifier) {
                if let Some(fn_inv) = INVERSE_UNARY_FUNCS.get(identifier) {
                    return Ok((
                        Expr::FnCall(fn_inv.to_string(), vec![target_expr]),
                        arguments[0].clone(),
                    ));
                } else {
                    match identifier {
                        "sqrt" => {
                            return Ok((
                                Expr::Binary(
                                    Box::new(target_expr),
                                    TokenKind::Power,
                                    Box::new(Expr::Literal(String::from("2"))),
                                ),
                                arguments[0].clone(),
                            ));
                        }
                        _ => {
                            return Err(CalcError::UnableToInvert(format!(
                                "Function '{}'",
                                identifier
                            )));
                        }
                    }
                }
            }
        }
        2 => {
            if prelude::BINARY_FUNCS.contains_key(identifier) {
                return Err(CalcError::UnableToInvert(format!(
                    "Function '{}'",
                    identifier
                )));
            }
        }
        _ => (),
    }

    // Get the function definition from the symbol table.
    let (parameters, body) =
        if let Some(Stmt::FnDecl(_, parameters, body)) = symbol_table.get_fn(identifier).cloned() {
            (parameters, body)
        } else {
            return Err(CalcError::UndefinedFn(identifier.into()));
        };

    // Make sure the input is valid.
    if parameters.len() != arguments.len() {
        return Err(CalcError::IncorrectAmountOfArguments(
            parameters.len(),
            identifier.into(),
            arguments.len(),
        ));
    }

    // Make the parameters usable as variables inside the function.
    let mut parameters_iter = parameters.iter();
    for argument in arguments {
        symbol_table.insert(Stmt::VarDecl(
            parameters_iter.next().unwrap().to_string(),
            Box::new(argument.clone()),
        ));
    }

    // Invert everything in the function body.
    invert(target_expr, symbol_table, &body)
}

fn contains_the_unit(symbol_table: &SymbolTable, expr: &Expr) -> bool {
    // Recursively scan the expression for the unit.
    match expr {
        Expr::Binary(left, _, right) => {
            contains_the_unit(symbol_table, left) || contains_the_unit(symbol_table, right)
        }
        Expr::Unary(_, expr) => contains_the_unit(symbol_table, expr),
        Expr::Unit(_, expr) => contains_the_unit(symbol_table, expr),
        Expr::Var(identifier) => {
            identifier == DECL_UNIT
                || if let Some(Stmt::VarDecl(_, var_expr)) = symbol_table.get_var(identifier) {
                    contains_the_unit(symbol_table, var_expr)
                } else {
                    false
                }
        }
        Expr::Group(expr) => contains_the_unit(symbol_table, expr),
        Expr::FnCall(_, args) => {
            for arg in args {
                if contains_the_unit(symbol_table, arg) {
                    return true;
                }
            }

            false
        }
        Expr::Literal(_) => false,
    }
}

/// Multiply an expression into a group.
fn multiply_into(expr: &Expr, base_expr: &Expr) -> Result<Expr, CalcError> {
    match base_expr {
        Expr::Binary(left, op, right) => match op {
            // If + or -, multiply the expression with each term.
            TokenKind::Plus | TokenKind::Minus => Ok(Expr::Binary(
                Box::new(multiply_into(expr, &left)?),
                op.clone(),
                Box::new(multiply_into(expr, &right)?),
            )),
            // If * or /, only multiply with the first factor.
            TokenKind::Star | TokenKind::Slash => Ok(Expr::Binary(
                Box::new(multiply_into(expr, &left)?),
                op.clone(),
                right.clone(),
            )),
            _ => unimplemented!(),
        },
        // If it's a literal, just multiply them together.
        Expr::Literal(_) | Expr::Var(_) => Ok(Expr::Binary(
            Box::new(expr.clone()),
            TokenKind::Star,
            Box::new(base_expr.clone()),
        )),
        Expr::Group(_) => Err(CalcError::UnableToInvert(String::from(
            "Parenthesis multiplied with parenthesis (this should be possible in the future).",
        ))),
        _ => unimplemented!(),
    }
}

#[allow(unused_imports, dead_code)] // Getting warnings for some reason
mod tests {
    use crate::ast::Expr;
    use crate::lexer::TokenKind::*;
    use crate::symbol_table::SymbolTable;
    use crate::test_helpers::*;

    fn decl_unit() -> Box<Expr> {
        Box::new(Expr::Var(crate::parser::DECL_UNIT.into()))
    }

    #[test]
    fn test_binary() {
        let ladd = binary(decl_unit(), Plus, literal("1"));
        let lsub = binary(decl_unit(), Minus, literal("1"));
        let lmul = binary(decl_unit(), Star, literal("1"));
        let ldiv = binary(decl_unit(), Slash, literal("1"));

        let radd = binary(literal("1"), Plus, decl_unit());
        let rsub = binary(literal("1"), Minus, decl_unit());
        let rmul = binary(literal("1"), Star, decl_unit());
        let rdiv = binary(literal("1"), Slash, decl_unit());

        let mut symbol_table = SymbolTable::new();
        assert_eq!(
            ladd.invert(&mut symbol_table).unwrap(),
            *binary(decl_unit(), Minus, literal("1"))
        );
        assert_eq!(
            lsub.invert(&mut symbol_table).unwrap(),
            *binary(decl_unit(), Plus, literal("1"))
        );
        assert_eq!(
            lmul.invert(&mut symbol_table).unwrap(),
            *binary(decl_unit(), Slash, literal("1"))
        );
        assert_eq!(
            ldiv.invert(&mut symbol_table).unwrap(),
            *binary(decl_unit(), Star, literal("1"))
        );

        assert_eq!(
            radd.invert(&mut symbol_table).unwrap(),
            *binary(decl_unit(), Minus, literal("1"))
        );
        assert_eq!(
            rsub.invert(&mut symbol_table).unwrap(),
            *unary(Minus, binary(decl_unit(), Plus, literal("1")))
        );
        assert_eq!(
            rmul.invert(&mut symbol_table).unwrap(),
            *binary(decl_unit(), Slash, literal("1"))
        );
        assert_eq!(
            rdiv.invert(&mut symbol_table).unwrap(),
            *binary(decl_unit(), Star, literal("1"))
        );
    }

    #[test]
    fn test_unary() {
        let neg = unary(Minus, decl_unit());

        let mut symbol_table = SymbolTable::new();
        assert_eq!(neg.invert(&mut symbol_table).unwrap(), *neg);
    }

    #[test]
    fn test_fn_call() {
        let call_with_literal = binary(fn_call("f", vec![*literal("2")]), Plus, decl_unit());
        let call_with_decl_unit = fn_call("f", vec![*decl_unit()]);
        let call_with_decl_unit_and_literal =
            fn_call("f", vec![*binary(decl_unit(), Plus, literal("2"))]);
        let decl = fn_decl(
            "f",
            vec![String::from("x")],
            binary(var("x"), Plus, literal("1")),
        );

        let mut symbol_table = SymbolTable::new();
        symbol_table.insert(decl);
        assert_eq!(
            call_with_literal.invert(&mut symbol_table).unwrap(),
            *binary(decl_unit(), Minus, fn_call("f", vec![*literal("2")])),
        );
        assert_eq!(
            call_with_decl_unit.invert(&mut symbol_table).unwrap(),
            *binary(decl_unit(), Minus, literal("1"))
        );
        assert_eq!(
            call_with_decl_unit_and_literal
                .invert(&mut symbol_table)
                .unwrap(),
            *binary(
                binary(decl_unit(), Minus, literal("1")),
                Minus,
                literal("2")
            )
        );
    }

    #[test]
    fn test_group() {
        let group_x = binary(
            group(binary(decl_unit(), Plus, literal("3"))),
            Star,
            literal("2"),
        );
        let group_unary_minus = binary(
            literal("2"),
            Minus,
            group(binary(decl_unit(), Plus, literal("3"))),
        );
        let x_group_add = binary(
            literal("2"),
            Star,
            group(binary(decl_unit(), Plus, literal("3"))),
        );
        let x_group_sub = binary(
            literal("2"),
            Star,
            group(binary(decl_unit(), Minus, literal("3"))),
        );
        let x_group_mul = binary(
            literal("2"),
            Star,
            group(binary(decl_unit(), Star, literal("3"))),
        );
        let x_group_div = binary(
            literal("2"),
            Star,
            group(binary(decl_unit(), Slash, literal("3"))),
        );

        let mut symbol_table = SymbolTable::new();
        assert_eq!(
            group_x.invert(&mut symbol_table).unwrap(),
            *binary(
                binary(decl_unit(), Minus, binary(literal("2"), Star, literal("3"))),
                Slash,
                literal("2")
            )
        );
        assert_eq!(
            group_unary_minus.invert(&mut symbol_table).unwrap(),
            *binary(
                binary(
                    binary(decl_unit(), Minus, literal("2")),
                    Minus,
                    binary(literal("-1"), Star, literal("3"))
                ),
                Slash,
                literal("-1")
            )
        );
        assert_eq!(
            x_group_add.invert(&mut symbol_table).unwrap(),
            *binary(
                binary(decl_unit(), Minus, binary(literal("2"), Star, literal("3"))),
                Slash,
                literal("2")
            )
        );
        assert_eq!(
            x_group_sub.invert(&mut symbol_table).unwrap(),
            *binary(
                binary(decl_unit(), Plus, binary(literal("2"), Star, literal("3"))),
                Slash,
                literal("2")
            )
        );
        assert_eq!(
            x_group_mul.invert(&mut symbol_table).unwrap(),
            *binary(
                binary(decl_unit(), Slash, literal("3")),
                Slash,
                literal("2")
            )
        );
        assert_eq!(
            x_group_div.invert(&mut symbol_table).unwrap(),
            *binary(binary(decl_unit(), Star, literal("3")), Slash, literal("2"))
        );
    }

    #[test]
    fn test_multiple_decl_units() {
        /*let add_two = binary(decl_unit(), Plus, decl_unit());

        let mut symbol_table = SymbolTable::new();
        assert_eq!(
            add_two.invert(&mut symbol_table).unwrap(),
            *binary(decl_unit(), Slash, literal("2"))
        );*/
    }
}
