use crate::ast::Identifier;
use crate::ast::{Expr, Stmt};
use crate::errors::KalkError;
use crate::lexer::TokenKind;
use crate::prelude;
use crate::symbol_table::SymbolTable;
use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    pub static ref INVERSE_UNARY_FUNCS: HashMap<&'static str, &'static str> = {
        let mut m = HashMap::new();
        m.insert("cos", "acos");
        m.insert("csc", "acsc");
        m.insert("csch", "csch");
        m.insert("cosh", "acosh");
        m.insert("cot", "acot");
        m.insert("coth", "acoth");
        m.insert("sec", "asec");
        m.insert("sech", "asech");
        m.insert("sin", "asin");
        m.insert("sinh", "asinh");
        m.insert("tan", "atan");
        m.insert("tanh", "atanh");

        m.insert("acos", "cos");
        m.insert("acsc", "csc");
        m.insert("acsch", "csch");
        m.insert("acosh", "cosh");
        m.insert("acot", "cot");
        m.insert("acoth", "coth");
        m.insert("asec", "sec");
        m.insert("asech", "sech");
        m.insert("asin", "sin");
        m.insert("asinh", "sinh");
        m.insert("atan", "tan");
        m.insert("atanh", "tanh");
        m
    };
}

impl Expr {
    pub fn invert(
        &self,
        symbol_table: &mut SymbolTable,
        unknown_var: &str,
    ) -> Result<Self, KalkError> {
        let target_expr = Expr::Var(Identifier::from_full_name(unknown_var));
        let result = invert(target_expr, symbol_table, self, unknown_var);

        Ok(result?.0)
    }

    pub fn invert_to_target(
        &self,
        symbol_table: &mut SymbolTable,
        target_expr: Expr,
        unknown_var: &str,
    ) -> Result<Self, KalkError> {
        let x = invert(target_expr, symbol_table, self, unknown_var)?;
        Ok(x.0)
    }
}

fn invert(
    target_expr: Expr,
    symbol_table: &mut SymbolTable,
    expr: &Expr,
    unknown_var: &str,
) -> Result<(Expr, Expr), KalkError> {
    match expr {
        Expr::Binary(left, op, right) => {
            invert_binary(target_expr, symbol_table, left, op, right, unknown_var)
        }
        Expr::Unary(op, expr) => invert_unary(target_expr, op, expr),
        Expr::Unit(identifier, expr) => {
            invert_unit(target_expr, symbol_table, identifier, expr, unknown_var)
        }
        Expr::Var(identifier) => invert_var(target_expr, symbol_table, identifier, unknown_var),
        Expr::Group(expr) => Ok((target_expr, *expr.clone())),
        Expr::FnCall(identifier, arguments) => invert_fn_call(
            target_expr,
            symbol_table,
            identifier,
            arguments,
            unknown_var,
        ),
        Expr::Literal(_) | Expr::Boolean(_) => Ok((target_expr, expr.clone())),
        Expr::Piecewise(_) => Err(KalkError::UnableToInvert(String::from("Piecewise"))),
        Expr::Vector(_) => Err(KalkError::UnableToInvert(String::from("Vector"))),
        Expr::Matrix(_) => Err(KalkError::UnableToInvert(String::from("Matrix"))),
        Expr::Indexer(_, _) => Err(KalkError::UnableToInvert(String::from("Inverter"))),
        Expr::Comprehension(_, _, _) => {
            Err(KalkError::UnableToInvert(String::from("Comprehension")))
        }
        Expr::Equation(_, _, _) => Err(KalkError::UnableToInvert(String::from("Equation"))),
        Expr::Preevaluated(_) => Err(KalkError::UnableToInvert(String::from("Equation"))),
    }
}

fn invert_binary(
    target_expr: Expr,
    symbol_table: &mut SymbolTable,
    left: &Expr,
    op: &TokenKind,
    right: &Expr,
    unknown_var: &str,
) -> Result<(Expr, Expr), KalkError> {
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
                    &multiply_into(&Expr::Literal(crate::float!(-1f64)), inside_group)?,
                    unknown_var,
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
                    unknown_var,
                );
            }

            // Same as above but left/right switched.
            if let Expr::Group(inside_group) = right {
                return invert(
                    target_expr,
                    symbol_table,
                    &multiply_into(left, inside_group)?,
                    unknown_var,
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
                    &Expr::Binary(inside_group.clone(), *op, Box::new(right.clone())),
                    unknown_var,
                );
            }

            // Eg. a/(b+c)
            // Same as above.
            if let Expr::Group(inside_group) = right {
                return invert(
                    target_expr,
                    symbol_table,
                    &Expr::Binary(Box::new(left.clone()), *op, inside_group.clone()),
                    unknown_var,
                );
            }

            TokenKind::Star
        }
        TokenKind::Power => {
            return if contains_var(symbol_table, left, unknown_var) {
                invert(
                    Expr::FnCall(
                        Identifier::from_full_name("root"),
                        vec![target_expr, right.clone()],
                    ),
                    symbol_table,
                    right,
                    unknown_var,
                )
            } else {
                invert(
                    Expr::FnCall(
                        Identifier::from_full_name("log"),
                        vec![target_expr, left.clone()],
                    ),
                    symbol_table,
                    right,
                    unknown_var,
                )
            };
        }
        _ => return Err(KalkError::UnableToInvert(String::new())),
    };

    // If the left expression contains the unit, invert the right one instead,
    // since the unit should not be moved.
    if contains_var(symbol_table, left, unknown_var) {
        // But if the right expression *also* contains the unit,
        // throw an error, since it can't handle this yet.
        if contains_var(symbol_table, right, unknown_var) {
            return Err(KalkError::UnableToInvert(String::from(
                "Expressions with several instances of an unknown variable (this might be supported in the future). Try simplifying the expression.",
            )));
        }

        return invert(
            Expr::Binary(Box::new(target_expr), op_inv, Box::new(right.clone())),
            symbol_table,
            left,
            unknown_var,
        );
    }

    // Otherwise, invert the left side.
    let final_target_expr = Expr::Binary(Box::new(target_expr), op_inv, Box::new(left.clone()));
    invert(
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
        unknown_var,
    )
}

fn invert_unary(target_expr: Expr, op: &TokenKind, expr: &Expr) -> Result<(Expr, Expr), KalkError> {
    match op {
        TokenKind::Minus => Ok((
            // Make the target expression negative
            Expr::Unary(TokenKind::Minus, Box::new(target_expr)),
            expr.clone(), // And then continue inverting the inner-expression.
        )),
        _ => Err(KalkError::UnableToInvert(String::new())),
    }
}

fn invert_unit(
    target_expr: Expr,
    symbol_table: &mut SymbolTable,
    identifier: &str,
    expr: &Expr,
    unknown_var: &str,
) -> Result<(Expr, Expr), KalkError> {
    let x = Expr::Binary(
        Box::new(target_expr),
        TokenKind::ToKeyword,
        Box::new(Expr::Var(Identifier::from_full_name(identifier))),
    );
    invert(x, symbol_table, expr, unknown_var)
}

fn invert_var(
    target_expr: Expr,
    symbol_table: &mut SymbolTable,
    identifier: &Identifier,
    unknown_var: &str,
) -> Result<(Expr, Expr), KalkError> {
    if identifier.full_name == unknown_var {
        Ok((target_expr, Expr::Var(identifier.clone())))
    } else if let Some(Stmt::VarDecl(_, var_expr)) =
        symbol_table.get_var(&identifier.full_name).cloned()
    {
        invert(target_expr, symbol_table, &var_expr, unknown_var)
    } else {
        Ok((target_expr, Expr::Var(identifier.clone())))
    }
}

fn invert_fn_call(
    target_expr: Expr,
    symbol_table: &mut SymbolTable,
    identifier: &Identifier,
    arguments: &[Expr],
    unknown_var: &str,
) -> Result<(Expr, Expr), KalkError> {
    // If prelude function
    match arguments.len() {
        1 => {
            if prelude::UNARY_FUNCS.contains_key(identifier.full_name.as_ref() as &str) {
                if let Some(fn_inv) = INVERSE_UNARY_FUNCS.get(identifier.full_name.as_ref() as &str)
                {
                    return invert(
                        Expr::FnCall(Identifier::from_full_name(fn_inv), vec![target_expr]),
                        symbol_table,
                        &arguments[0],
                        unknown_var,
                    );
                } else {
                    match identifier.full_name.as_ref() {
                        "sqrt" => {
                            return invert(
                                Expr::Binary(
                                    Box::new(target_expr),
                                    TokenKind::Power,
                                    Box::new(Expr::Literal(crate::float!(2f64))),
                                ),
                                symbol_table,
                                &arguments[0],
                                unknown_var,
                            );
                        }
                        _ => {
                            return Err(KalkError::UnableToInvert(format!(
                                "Function '{}'",
                                identifier.full_name
                            )));
                        }
                    }
                }
            }
        }
        2 => {
            if prelude::BINARY_FUNCS.contains_key(identifier.full_name.as_ref() as &str) {
                return Err(KalkError::UnableToInvert(format!(
                    "Function '{}'",
                    identifier.full_name
                )));
            }
        }
        _ => (),
    }

    // Get the function definition from the symbol table.
    let (parameters, body) = if let Some(Stmt::FnDecl(_, parameters, body)) =
        symbol_table.get_fn(&identifier.full_name).cloned()
    {
        (parameters, body)
    } else {
        return Err(KalkError::UndefinedFn(identifier.full_name.clone()));
    };

    // Make sure the input is valid.
    if parameters.len() != arguments.len() {
        return Err(KalkError::IncorrectAmountOfArguments(
            parameters.len(),
            identifier.full_name.clone(),
            arguments.len(),
        ));
    }

    // Make the parameters usable as variables inside the function.
    let mut parameters_iter = parameters.iter();
    for argument in arguments {
        symbol_table.insert(Stmt::VarDecl(
            Identifier::from_full_name(parameters_iter.next().unwrap()),
            Box::new(argument.clone()),
        ));
    }

    // Invert everything in the function body.
    invert(target_expr, symbol_table, &body, unknown_var)
}

pub fn contains_var(symbol_table: &SymbolTable, expr: &Expr, var_name: &str) -> bool {
    // Recursively scan the expression for the variable.
    match expr {
        Expr::Binary(left, _, right) => {
            contains_var(symbol_table, left, var_name)
                || contains_var(symbol_table, right, var_name)
        }
        Expr::Unary(_, expr) => contains_var(symbol_table, expr, var_name),
        Expr::Unit(_, expr) => contains_var(symbol_table, expr, var_name),
        Expr::Var(identifier) => {
            identifier.full_name == var_name
                || if let Some(Stmt::VarDecl(_, var_expr)) =
                    symbol_table.get_var(&identifier.full_name)
                {
                    contains_var(symbol_table, var_expr, var_name)
                } else {
                    false
                }
        }
        Expr::Group(expr) => contains_var(symbol_table, expr, var_name),
        Expr::FnCall(_, args) => {
            for arg in args {
                if contains_var(symbol_table, arg, var_name) {
                    return true;
                }
            }

            false
        }
        Expr::Literal(_) | Expr::Boolean(_) => false,
        Expr::Piecewise(_) => true, // Let it try to invert this. It will just display the error message.
        Expr::Vector(items) => items
            .iter()
            .any(|x| contains_var(symbol_table, x, var_name)),
        Expr::Matrix(rows) => rows
            .iter()
            .any(|row| row.iter().any(|x| contains_var(symbol_table, x, var_name))),
        Expr::Indexer(_, _) => false,
        Expr::Comprehension(_, _, _) => false,
        Expr::Equation(_, _, _) => false,
        Expr::Preevaluated(_) => false,
    }
}

/// Multiply an expression into a group.
fn multiply_into(expr: &Expr, base_expr: &Expr) -> Result<Expr, KalkError> {
    match base_expr {
        Expr::Binary(left, op, right) => match op {
            // If + or -, multiply the expression with each term.
            TokenKind::Plus | TokenKind::Minus => Ok(Expr::Binary(
                Box::new(multiply_into(expr, left)?),
                *op,
                Box::new(multiply_into(expr, right)?),
            )),
            // If * or /, only multiply with the first factor.
            TokenKind::Star | TokenKind::Slash => Ok(Expr::Binary(
                Box::new(multiply_into(expr, left)?),
                *op,
                right.clone(),
            )),
            _ => Err(KalkError::UnableToInvert(String::new())),
        },
        // If it's a literal, just multiply them together.
        Expr::Literal(_) | Expr::Var(_) => Ok(Expr::Binary(
            Box::new(expr.clone()),
            TokenKind::Star,
            Box::new(base_expr.clone()),
        )),
        Expr::Group(_) => Err(KalkError::UnableToInvert(String::from(
            "Parenthesis multiplied with parenthesis (this should be possible in the future).",
        ))),
        _ => Err(KalkError::UnableToInvert(String::new())),
    }
}

#[allow(unused_imports, dead_code)] // Getting warnings for some reason
#[cfg(test)]
mod tests {
    use crate::ast::Expr;
    use crate::ast::Identifier;
    use crate::lexer::TokenKind::*;
    use crate::parser::DECL_UNIT;
    use crate::symbol_table::SymbolTable;
    use crate::test_helpers::*;
    use wasm_bindgen_test::*;

    fn decl_unit() -> Box<Expr> {
        Box::new(Expr::Var(Identifier::from_full_name(
            crate::parser::DECL_UNIT,
        )))
    }

    #[test]
    #[wasm_bindgen_test]
    fn test_binary() {
        let ladd = binary(decl_unit(), Plus, f64_to_float_literal(1f64));
        let lsub = binary(decl_unit(), Minus, f64_to_float_literal(1f64));
        let lmul = binary(decl_unit(), Star, f64_to_float_literal(1f64));
        let ldiv = binary(decl_unit(), Slash, f64_to_float_literal(1f64));

        let radd = binary(f64_to_float_literal(1f64), Plus, decl_unit());
        let rsub = binary(f64_to_float_literal(1f64), Minus, decl_unit());
        let rmul = binary(f64_to_float_literal(1f64), Star, decl_unit());
        let rdiv = binary(f64_to_float_literal(1f64), Slash, decl_unit());

        let mut symbol_table = SymbolTable::new();
        assert_eq!(
            ladd.invert(&mut symbol_table, DECL_UNIT).unwrap(),
            *binary(decl_unit(), Minus, f64_to_float_literal(1f64))
        );
        assert_eq!(
            lsub.invert(&mut symbol_table, DECL_UNIT).unwrap(),
            *binary(decl_unit(), Plus, f64_to_float_literal(1f64))
        );
        assert_eq!(
            lmul.invert(&mut symbol_table, DECL_UNIT).unwrap(),
            *binary(decl_unit(), Slash, f64_to_float_literal(1f64))
        );
        assert_eq!(
            ldiv.invert(&mut symbol_table, DECL_UNIT).unwrap(),
            *binary(decl_unit(), Star, f64_to_float_literal(1f64))
        );

        assert_eq!(
            radd.invert(&mut symbol_table, DECL_UNIT).unwrap(),
            *binary(decl_unit(), Minus, f64_to_float_literal(1f64))
        );
        assert_eq!(
            rsub.invert(&mut symbol_table, DECL_UNIT).unwrap(),
            *unary(Minus, binary(decl_unit(), Plus, f64_to_float_literal(1f64)))
        );
        assert_eq!(
            rmul.invert(&mut symbol_table, DECL_UNIT).unwrap(),
            *binary(decl_unit(), Slash, f64_to_float_literal(1f64))
        );
        assert_eq!(
            rdiv.invert(&mut symbol_table, DECL_UNIT).unwrap(),
            *binary(decl_unit(), Star, f64_to_float_literal(1f64))
        );
    }

    #[test]
    #[wasm_bindgen_test]
    fn test_unary() {
        let neg = unary(Minus, decl_unit());

        let mut symbol_table = SymbolTable::new();
        assert_eq!(neg.invert(&mut symbol_table, DECL_UNIT).unwrap(), *neg);
    }

    #[test]
    #[wasm_bindgen_test]
    fn test_fn_call() {
        let call_with_literal = binary(fn_call("f", vec![*f64_to_float_literal(2f64)]), Plus, decl_unit());
        let call_with_decl_unit = fn_call("f", vec![*decl_unit()]);
        let call_with_decl_unit_and_literal =
            fn_call("f", vec![*binary(decl_unit(), Plus, f64_to_float_literal(2f64))]);
        let decl = fn_decl(
            "f",
            vec![String::from("x")],
            binary(var("x"), Plus, f64_to_float_literal(1f64)),
        );

        let mut symbol_table = SymbolTable::new();
        symbol_table.insert(decl);
        assert_eq!(
            call_with_literal
                .invert(&mut symbol_table, DECL_UNIT)
                .unwrap(),
            *binary(decl_unit(), Minus, fn_call("f", vec![*f64_to_float_literal(2f64)])),
        );
        assert_eq!(
            call_with_decl_unit
                .invert(&mut symbol_table, DECL_UNIT)
                .unwrap(),
            *binary(decl_unit(), Minus, f64_to_float_literal(1f64))
        );
        assert_eq!(
            call_with_decl_unit_and_literal
                .invert(&mut symbol_table, DECL_UNIT)
                .unwrap(),
            *binary(
                binary(decl_unit(), Minus, f64_to_float_literal(1f64)),
                Minus,
                f64_to_float_literal(2f64)
            )
        );
    }

    #[test]
    #[wasm_bindgen_test]
    fn test_group() {
        let group_x = binary(
            group(binary(decl_unit(), Plus, f64_to_float_literal(3f64))),
            Star,
            f64_to_float_literal(2f64),
        );
        let group_unary_minus = binary(
            f64_to_float_literal(2f64),
            Minus,
            group(binary(decl_unit(), Plus, f64_to_float_literal(3f64))),
        );
        let x_group_add = binary(
            f64_to_float_literal(2f64),
            Star,
            group(binary(decl_unit(), Plus, f64_to_float_literal(3f64))),
        );
        let x_group_sub = binary(
            f64_to_float_literal(2f64),
            Star,
            group(binary(decl_unit(), Minus, f64_to_float_literal(3f64))),
        );
        let x_group_mul = binary(
            f64_to_float_literal(2f64),
            Star,
            group(binary(decl_unit(), Star, f64_to_float_literal(3f64))),
        );
        let x_group_div = binary(
            f64_to_float_literal(2f64),
            Star,
            group(binary(decl_unit(), Slash, f64_to_float_literal(3f64))),
        );

        let mut symbol_table = SymbolTable::new();
        assert_eq!(
            group_x.invert(&mut symbol_table, DECL_UNIT).unwrap(),
            *binary(
                binary(
                    decl_unit(),
                    Minus,
                    binary(f64_to_float_literal(2f64), Star, f64_to_float_literal(3f64))
                ),
                Slash,
                f64_to_float_literal(2f64)
            )
        );
        assert_eq!(
            group_unary_minus
                .invert(&mut symbol_table, DECL_UNIT)
                .unwrap(),
            *binary(
                binary(
                    binary(decl_unit(), Minus, f64_to_float_literal(2f64)),
                    Minus,
                    binary(f64_to_float_literal(-1f64), Star, f64_to_float_literal(3f64))
                ),
                Slash,
                f64_to_float_literal(-1f64)
            )
        );
        assert_eq!(
            x_group_add.invert(&mut symbol_table, DECL_UNIT).unwrap(),
            *binary(
                binary(
                    decl_unit(),
                    Minus,
                    binary(f64_to_float_literal(2f64), Star, f64_to_float_literal(3f64))
                ),
                Slash,
                f64_to_float_literal(2f64)
            )
        );
        assert_eq!(
            x_group_sub.invert(&mut symbol_table, DECL_UNIT).unwrap(),
            *binary(
                binary(
                    decl_unit(),
                    Plus,
                    binary(f64_to_float_literal(2f64), Star, f64_to_float_literal(3f64))
                ),
                Slash,
                f64_to_float_literal(2f64)
            )
        );
        assert_eq!(
            x_group_mul.invert(&mut symbol_table, DECL_UNIT).unwrap(),
            *binary(
                binary(decl_unit(), Slash, f64_to_float_literal(3f64)),
                Slash,
                f64_to_float_literal(2f64)
            )
        );
        assert_eq!(
            x_group_div.invert(&mut symbol_table, DECL_UNIT).unwrap(),
            *binary(
                binary(decl_unit(), Star, f64_to_float_literal(3f64)),
                Slash,
                f64_to_float_literal(2f64)
            )
        );
    }

    #[test]
    #[wasm_bindgen_test]
    fn test_multiple_decl_units() {
        /*let add_two = binary(decl_unit(), Plus, decl_unit());

        let mut symbol_table = SymbolTable::new();
        assert_eq!(
            add_two.invert(&mut symbol_table).unwrap(),
            *binary(decl_unit(), Slash, literal("2"))
        );*/
    }
}
