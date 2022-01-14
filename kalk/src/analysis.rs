use crate::{
    ast::{ConditionalPiece, Expr, Identifier, Stmt},
    inverter,
    lexer::TokenKind,
    parser::{self, CalcError},
    prelude,
    symbol_table::SymbolTable,
};

pub(crate) struct Context<'a> {
    pub(crate) symbol_table: &'a mut SymbolTable,
    current_function_name: Option<String>,
    current_function_parameters: Option<Vec<String>>,
    equation_variable: Option<String>,
    in_integral: bool,
    in_unit_decl: bool,
    in_conditional: bool,
    in_equation: bool,
}

pub(crate) fn analyse_stmt(
    symbol_table: &mut SymbolTable,
    statement: Stmt,
) -> Result<Stmt, CalcError> {
    let mut context = Context {
        symbol_table,
        current_function_name: None,
        current_function_parameters: None,
        equation_variable: None,
        in_integral: false,
        in_unit_decl: false,
        in_conditional: false,
        in_equation: false,
    };

    Ok(match statement {
        Stmt::VarDecl(identifier, value) => {
            let var_decl = Stmt::VarDecl(identifier, Box::new(analyse_expr(&mut context, *value)?));
            context.symbol_table.insert(var_decl.clone());

            var_decl
        }
        Stmt::FnDecl(identifier, parameters, body) => {
            context.current_function_name = Some(identifier.pure_name.clone());
            context.current_function_parameters = Some(parameters.clone());
            let fn_decl = Stmt::FnDecl(
                identifier,
                parameters,
                Box::new(analyse_expr(&mut context, *body)?),
            );
            context.symbol_table.insert(fn_decl.clone());
            context.current_function_name = None;
            context.current_function_parameters = None;

            fn_decl
        }
        Stmt::UnitDecl(identifier, unit, value) => {
            context.in_unit_decl = true;
            let result = Stmt::UnitDecl(
                identifier,
                unit,
                Box::new(analyse_expr(&mut context, *value)?),
            );
            context.in_unit_decl = false;

            result
        }
        Stmt::Expr(value) => analyse_stmt_expr(&mut context, *value)?,
    })
}

fn analyse_stmt_expr(context: &mut Context, value: Expr) -> Result<Stmt, CalcError> {
    // Apparently you can't pattern match boxed types in the stable compiler.
    // This is a mess...
    let value = if let Expr::Binary(left, TokenKind::Equals, right) = value {
        if let Expr::Binary(identifier_expr, TokenKind::Star, parameter_expr) = *left {
            match *identifier_expr {
                Expr::Var(identifier) if !prelude::is_prelude_func(&identifier.full_name) => {
                    let mut parameters = Vec::new();
                    match *parameter_expr {
                        Expr::Vector(exprs) => {
                            for expr in exprs {
                                if let Expr::Var(argument_identifier) = expr {
                                    parameters.push(format!(
                                        "{}-{}",
                                        identifier.pure_name, argument_identifier.pure_name
                                    ));
                                }
                            }
                        }
                        Expr::Group(expr) => {
                            if let Expr::Var(argument_identifier) = *expr {
                                parameters.push(format!(
                                    "{}-{}",
                                    identifier.pure_name, argument_identifier.pure_name
                                ));
                            }
                        }
                        _ => unreachable!(),
                    }

                    context.current_function_name = Some(identifier.pure_name.clone());
                    context.current_function_parameters = Some(parameters.clone());
                    let fn_decl = Stmt::FnDecl(
                        identifier,
                        parameters,
                        Box::new(analyse_expr(context, *right)?),
                    );
                    context.symbol_table.insert(fn_decl.clone());
                    context.current_function_name = None;
                    context.current_function_parameters = None;

                    return Ok(fn_decl);
                }
                _ => Expr::Binary(
                    Box::new(Expr::Binary(
                        identifier_expr,
                        TokenKind::Star,
                        parameter_expr,
                    )),
                    TokenKind::Equals,
                    right,
                ),
            }
        } else {
            Expr::Binary(left, TokenKind::Equals, right)
        }
    } else {
        value
    };

    Ok(Stmt::Expr(Box::new(analyse_expr(context, value)?)))
}

fn analyse_expr(context: &mut Context, expr: Expr) -> Result<Expr, CalcError> {
    Ok(match expr {
        Expr::Binary(left, op, right) => analyse_binary(context, *left, op, *right)?,
        Expr::Unary(op, value) => Expr::Unary(op, Box::new(analyse_expr(context, *value)?)),
        Expr::Unit(name, value) => Expr::Unit(name, Box::new(analyse_expr(context, *value)?)),
        Expr::Var(identifier) => analyse_var(context, identifier, None, None)?,
        Expr::Group(value) => Expr::Group(Box::new(analyse_expr(context, *value)?)),
        Expr::FnCall(identifier, arguments) => {
            let mut analysed_arguments = Vec::new();
            for argument in arguments {
                analysed_arguments.push(analyse_expr(context, argument)?);
            }

            Expr::FnCall(identifier, analysed_arguments)
        }
        Expr::Literal(_) => expr,
        Expr::Piecewise(pieces) => {
            let mut analysed_pieces = Vec::new();
            for piece in pieces {
                context.in_conditional = true;
                let condition = analyse_expr(context, piece.condition)?;
                context.in_conditional = false;

                analysed_pieces.push(ConditionalPiece {
                    condition,
                    expr: analyse_expr(context, piece.expr)?,
                });
            }

            Expr::Piecewise(analysed_pieces)
        }
        Expr::Vector(values) => {
            let mut analysed_values = Vec::new();
            for value in values {
                analysed_values.push(analyse_expr(context, value)?);
            }

            Expr::Vector(analysed_values)
        }
        Expr::Matrix(rows) => {
            let mut analysed_rows = Vec::new();
            for row in rows {
                let mut analysed_values = Vec::new();
                for value in row {
                    analysed_values.push(analyse_expr(context, value)?);
                }

                analysed_rows.push(analysed_values);
            }

            Expr::Matrix(analysed_rows)
        }
        Expr::Indexer(value, indexes) => {
            let mut analysed_indexes = Vec::new();
            for index in indexes {
                analysed_indexes.push(analyse_expr(context, index)?);
            }

            Expr::Indexer(Box::new(analyse_expr(context, *value)?), analysed_indexes)
        }
    })
}

fn analyse_binary<'a>(
    context: &'a mut Context,
    left: Expr,
    op: TokenKind,
    right: Expr,
) -> Result<Expr, CalcError> {
    let right = analyse_expr(context, right)?;
    match (&left, &op) {
        (_, TokenKind::Equals) if !context.in_conditional => {
            // Equation
            context.in_equation = true;
            let left = analyse_expr(context, left)?;
            context.in_equation = false;

            let var_name = if let Some(var_name) = &context.equation_variable {
                var_name
            } else {
                return Err(CalcError::UnableToSolveEquation);
            };

            let inverted = if inverter::contains_var(&mut context.symbol_table, &left, var_name) {
                left.invert_to_target(context.symbol_table, right, var_name)?
            } else {
                right.invert_to_target(context.symbol_table, left, var_name)?
            };

            // If the inverted expression still contains the variable,
            // the equation solving failed.
            if inverter::contains_var(context.symbol_table, &inverted, var_name) {
                return Err(CalcError::UnableToSolveEquation);
            }

            context.symbol_table.insert(Stmt::VarDecl(
                Identifier::from_full_name(var_name),
                Box::new(inverted.clone()),
            ));

            return Ok(inverted);
        }
        (Expr::Var(_), TokenKind::Star) => {
            if let Expr::Var(identifier) = left {
                analyse_var(context, identifier, Some(right), None)
            } else {
                unreachable!()
            }
        }
        (Expr::Var(_), TokenKind::Power) => {
            if let Expr::Var(identifier) = left {
                analyse_var(context, identifier, None, Some(right))
            } else {
                unreachable!()
            }
        }
        _ => Ok(Expr::Binary(
            Box::new(analyse_expr(context, left)?),
            op,
            Box::new(right),
        )),
    }
}

fn analyse_var(
    context: &mut Context,
    identifier: Identifier,
    adjacent_factor: Option<Expr>,
    adjacent_exponent: Option<Expr>,
) -> Result<Expr, CalcError> {
    let mut log_base = None;
    if identifier.full_name.starts_with("log") {
        if let Some(lowered) = identifier.get_lowered_part() {
            if let Ok(lowered_float) = lowered.parse::<f64>() {
                log_base = Some(Expr::Literal(lowered_float));
            }
        }
    }

    // Eg. f(1, 2, 3), f(3) or f3
    let exists_as_fn = context.symbol_table.contains_fn(&identifier.pure_name)
        || context.current_function_name.as_ref() == Some(&identifier.pure_name)
        || log_base.is_some();
    if exists_as_fn {
        if let Some(adjacent_expr) = adjacent_factor {
            return with_adjacent(
                build_fn_call(context, identifier, adjacent_expr, log_base)?,
                None,
                adjacent_exponent,
            );
        }
    }

    if context.symbol_table.contains_var(&identifier.pure_name)
        || (identifier.pure_name.len() == 1 && !context.in_equation)
    {
        with_adjacent(
            build_var(context, &identifier.full_name),
            adjacent_factor,
            adjacent_exponent,
        )
    } else if context
        .symbol_table
        .contains_var(&identifier.get_name_without_lowered())
    {
        with_adjacent(
            build_indexed_var(context, identifier)?,
            adjacent_factor,
            adjacent_exponent,
        )
    } else if context.in_unit_decl {
        with_adjacent(
            build_var(context, parser::DECL_UNIT),
            adjacent_factor,
            adjacent_exponent,
        )
    } else {
        if let Some(equation_var) = &context.equation_variable {
            if &identifier.full_name == equation_var {
                return with_adjacent(
                    build_var(context, &identifier.full_name),
                    adjacent_factor,
                    adjacent_exponent,
                );
            }
        }

        if context.in_equation {
            context.equation_variable = Some(identifier.full_name.clone());
            return with_adjacent(
                build_var(context, &identifier.full_name),
                adjacent_factor,
                adjacent_exponent,
            );
        }

        let mut identifier_without_dx: Vec<char> = identifier.full_name.chars().collect();
        let last_char = identifier_without_dx.pop().unwrap_or_default();
        let second_last_char = identifier_without_dx.pop().unwrap_or_default();

        if context.in_integral && second_last_char == 'd' && identifier.pure_name.len() > 2 {
            let new_identifier: String = identifier_without_dx.iter().collect();
            with_adjacent(
                build_dx(context, &new_identifier, last_char)?,
                adjacent_factor,
                adjacent_exponent,
            )
        } else {
            build_split_up_vars(context, identifier, adjacent_exponent)
        }
    }
}

fn with_adjacent(
    expr: Expr,
    factor: Option<Expr>,
    exponent: Option<Expr>,
) -> Result<Expr, CalcError> {
    if let Some(factor) = factor {
        Ok(Expr::Binary(
            Box::new(expr),
            TokenKind::Star,
            Box::new(factor),
        ))
    } else if let Some(exponent) = exponent {
        Ok(Expr::Binary(
            Box::new(expr),
            TokenKind::Power,
            Box::new(exponent),
        ))
    } else {
        Ok(expr)
    }
}

fn build_fn_call(
    context: &mut Context,
    identifier: Identifier,
    adjacent_expr: Expr,
    log_base: Option<Expr>,
) -> Result<Expr, CalcError> {
    let is_integral = identifier.full_name == "integrate";
    if is_integral {
        context.in_integral = true;
    }

    let arguments = match adjacent_expr {
        Expr::Vector(arguments) => {
            let mut new_arguments = Vec::new();
            for argument in arguments {
                new_arguments.push(analyse_expr(context, argument)?);
            }

            new_arguments
        }
        _ => {
            let argument = if let Expr::Group(argument) = adjacent_expr {
                *argument
            } else {
                adjacent_expr
            };
            if let Some(log_base) = log_base {
                return Ok(Expr::FnCall(
                    Identifier::from_full_name("log"),
                    vec![analyse_expr(context, argument)?, log_base],
                ));
            } else {
                vec![analyse_expr(context, argument)?]
            }
        }
    };

    if is_integral {
        context.in_integral = false;
    }

    return Ok(Expr::FnCall(identifier, arguments));
}

fn build_indexed_var(context: &mut Context, identifier: Identifier) -> Result<Expr, CalcError> {
    let underscore_pos = identifier.pure_name.find('_').unwrap();
    let var_name = &identifier.pure_name[0..underscore_pos];
    let lowered = &identifier.pure_name[underscore_pos + 1..];
    let lowered_expr = if lowered.len() > 0 && lowered.chars().nth(0).unwrap_or('\0').is_digit(10) {
        Expr::Literal(lowered.parse::<f64>().unwrap_or(f64::NAN))
    } else {
        build_var(context, lowered)
    };

    Ok(Expr::Indexer(
        Box::new(build_var(context, &var_name)),
        vec![lowered_expr],
    ))
}

fn build_dx(
    context: &mut Context,
    name_without_dx: &str,
    char_after_d: char,
) -> Result<Expr, CalcError> {
    Ok(Expr::Binary(
        Box::new(build_var(context, name_without_dx)),
        TokenKind::Star,
        Box::new(build_var(context, &char_after_d.to_string())),
    ))
}

fn build_split_up_vars(
    context: &mut Context,
    identifier: Identifier,
    adjacent_exponent: Option<Expr>,
) -> Result<Expr, CalcError> {
    let mut chars: Vec<char> = identifier.pure_name.chars().collect();
    let last_char = chars.pop().unwrap_or_default();
    let identifier_without_last: String = chars.iter().collect();

    // Temporarily remove the last character and check if a function
    // without that character exists. If it does,
    // create a function call expression, where that last character
    // is the argument.
    if context.symbol_table.contains_fn(&identifier_without_last) {
        return Ok(Expr::FnCall(
            Identifier::from_full_name(&identifier_without_last),
            vec![build_var(context, &last_char.to_string())],
        ));
    } else {
        // Otherwise, re-add the character.
        chars.push(last_char);
    }

    // Turn each individual character into its own variable reference.
    // This parses eg `xy` as `x*y` instead of *one* variable.
    let mut left = build_var(context, &chars.first().unwrap().to_string());
    let mut chars_iter = chars.iter().skip(1).peekable();
    let mut adjacent_exponent = adjacent_exponent;
    while let Some(c) = chars_iter.next() {
        let mut right = build_var(context, &c.to_string());

        // If last iteration
        if chars_iter.peek().is_none() {
            if let Some(exponent) = adjacent_exponent {
                right = Expr::Binary(Box::new(right), TokenKind::Power, Box::new(exponent));
                adjacent_exponent = None;
            }
        }

        left = Expr::Binary(Box::new(left), TokenKind::Star, Box::new(right))
    }

    Ok(left)
}

fn build_var(context: &Context, name: &str) -> Expr {
    if let (Some(function_name), Some(params)) = (
        context.current_function_name.as_ref(),
        context.current_function_parameters.as_ref(),
    ) {
        let identifier = Identifier::parameter_from_name(name, &function_name);
        if params.contains(&identifier.full_name) {
            return Expr::Var(identifier);
        }
    }
    return Expr::Var(Identifier::from_full_name(name));
}
