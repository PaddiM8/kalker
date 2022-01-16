use crate::{
    ast::{ConditionalPiece, Expr, Identifier, RangedVar, Stmt},
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
    in_sum_prod: bool,
    in_unit_decl: bool,
    in_conditional: bool,
    in_equation: bool,
    in_comprehension: bool,
    comprehension_vars: Option<Vec<RangedVar>>,
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
        in_sum_prod: false,
        in_unit_decl: false,
        in_conditional: false,
        in_equation: false,
        in_comprehension: false,
        comprehension_vars: None,
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
    Ok(
        if let Expr::Binary(left, TokenKind::Equals, right) = value {
            match *left {
                Expr::Binary(identifier_expr, TokenKind::Star, parameter_expr) => {
                    build_fn_decl(context, *identifier_expr, *parameter_expr, *right)?
                }
                Expr::Var(identifier) if !context.in_conditional => {
                    if inverter::contains_var(context.symbol_table, &right, &identifier.full_name) {
                        return Err(CalcError::VariableReferencesItself);
                    }

                    if prelude::is_constant(&identifier.full_name) {
                        return Err(CalcError::UnableToOverrideConstant(identifier.pure_name));
                    }

                    let result =
                        Stmt::VarDecl(identifier, Box::new(analyse_expr(context, *right)?));
                    context.symbol_table.insert(result.clone());

                    result
                }
                _ => Stmt::Expr(Box::new(Expr::Binary(
                    Box::new(analyse_expr(context, *left)?),
                    TokenKind::Equals,
                    right,
                ))),
            }
        } else {
            Stmt::Expr(Box::new(analyse_expr(context, value)?))
        },
    )
}

fn build_fn_decl(
    context: &mut Context,
    identifier_expr: Expr,
    parameter_expr: Expr,
    right: Expr,
) -> Result<Stmt, CalcError> {
    Ok(match identifier_expr {
        Expr::Var(identifier) if !prelude::is_prelude_func(&identifier.full_name) => {
            // Check if all the expressions in the parameter_expr are
            // variables. If not, it can't be turned into a function declaration.
            let all_are_vars = match &parameter_expr {
                Expr::Vector(exprs) => {
                    exprs
                        .iter()
                        .any(|x| matches!(x, Expr::Var(_)))
                }
                Expr::Group(expr) => {
                    matches!(&**expr, Expr::Var(_))
                }
                _ => false,
            };

            if !all_are_vars {
                // Analyse it as a function call instead
                return Ok(Stmt::Expr(Box::new(analyse_expr(
                    context,
                    Expr::Binary(
                        Box::new(Expr::Binary(
                            Box::new(Expr::Var(identifier)),
                            TokenKind::Star,
                            Box::new(parameter_expr),
                        )),
                        TokenKind::Equals,
                        Box::new(right),
                    ),
                )?)));
            }

            let exprs = match parameter_expr {
                Expr::Vector(exprs) => exprs,
                Expr::Group(expr) => vec![*expr],
                _ => unreachable!(),
            };

            let mut parameters = Vec::new();
            for expr in exprs {
                if let Expr::Var(argument_identifier) = expr {
                    parameters.push(format!(
                        "{}-{}",
                        identifier.pure_name, argument_identifier.pure_name
                    ));
                }
            }

            context.current_function_name = Some(identifier.pure_name.clone());
            context.current_function_parameters = Some(parameters.clone());
            let fn_decl = Stmt::FnDecl(
                identifier,
                parameters,
                Box::new(analyse_expr(context, right)?),
            );
            context.symbol_table.insert(fn_decl.clone());
            context.current_function_name = None;
            context.current_function_parameters = None;

            fn_decl
        }
        _ => {
            let new_binary = Expr::Binary(
                Box::new(Expr::Binary(
                    Box::new(identifier_expr),
                    TokenKind::Star,
                    Box::new(parameter_expr),
                )),
                TokenKind::Equals,
                Box::new(right),
            );

            Stmt::Expr(Box::new(analyse_expr(context, new_binary)?))
        }
    })
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
        Expr::Comprehension(left, right, vars) => Expr::Comprehension(left, right, vars),
    })
}

fn analyse_binary(
    context: &mut Context,
    left: Expr,
    op: TokenKind,
    right: Expr,
) -> Result<Expr, CalcError> {
    let previous_in_conditional = context.in_conditional;
    if op == TokenKind::And || op == TokenKind::Or {
        context.in_conditional = true;
    }

    let result = match (&left, &op, &right) {
        (_, TokenKind::Equals, _) if !context.in_conditional => {
            // Equation
            context.in_equation = true;
            let left = analyse_expr(context, left)?;
            let right = analyse_expr(context, right)?;

            // If it has already been set to false manually somewhere else,
            // abort and analyse as a comparison instead.
            if !context.in_equation {
                context.in_conditional = true;
                let result = analyse_binary(context, left, op, right);
                context.in_conditional = previous_in_conditional;

                return result;
            }

            context.in_equation = false;

            let var_name = if let Some(var_name) = &context.equation_variable {
                var_name
            } else {
                context.in_conditional = true;
                let result = analyse_binary(context, left, op, right);
                context.in_conditional = previous_in_conditional;

                return result;
            };

            let inverted = if inverter::contains_var(context.symbol_table, &left, var_name) {
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
            context.equation_variable = None;

            return Ok(inverted);
        }
        (Expr::Var(_), TokenKind::Star, _) => {
            if let Expr::Var(identifier) = left {
                analyse_var(context, identifier, Some(right), None)
            } else {
                unreachable!()
            }
        }
        (Expr::Var(_), TokenKind::Power, _) => {
            if let Expr::Var(identifier) = left {
                analyse_var(context, identifier, None, Some(right))
            } else {
                unreachable!()
            }
        }
        (_, TokenKind::Colon, _) => {
            context.in_comprehension = true;
            context.in_conditional = true;
            context.comprehension_vars = Some(Vec::new());

            let mut conditions = vec![right];
            let mut has_comma = false;
            while let Expr::Binary(_, TokenKind::Comma, _) = conditions.last().unwrap() {
                has_comma = true;
                if let Expr::Binary(left_condition, _, right_condition) = conditions.pop().unwrap()
                {
                    conditions.push(analyse_expr(context, *left_condition.to_owned())?);
                    conditions.push(analyse_expr(context, *right_condition.to_owned())?);
                }
            }

            if !has_comma {
                let analysed_condition = analyse_expr(context, conditions.pop().unwrap())?;
                conditions.push(analysed_condition);
            }

            context.in_comprehension = false;
            context.in_conditional = false;
            let left = analyse_expr(context, left)?;

            let result = Expr::Comprehension(
                Box::new(left),
                conditions,
                context.comprehension_vars.take().unwrap(),
            );

            Ok(result)
        }
        (
            Expr::Var(_),
            TokenKind::GreaterThan
            | TokenKind::LessThan
            | TokenKind::GreaterOrEquals
            | TokenKind::LessOrEquals,
            _,
        ) => analyse_comparison_with_var(context, left, op, right),
        (
            _,
            TokenKind::GreaterThan
            | TokenKind::LessThan
            | TokenKind::GreaterOrEquals
            | TokenKind::LessOrEquals,
            Expr::Var(_),
        ) => {
            let inv_op = match op {
                TokenKind::GreaterThan => TokenKind::LessThan,
                TokenKind::LessThan => TokenKind::GreaterThan,
                TokenKind::GreaterOrEquals => TokenKind::LessOrEquals,
                TokenKind::LessOrEquals => TokenKind::GreaterOrEquals,
                _ => unreachable!(),
            };
            analyse_comparison_with_var(context, right, inv_op, left)
        }
        _ => Ok(Expr::Binary(
            Box::new(analyse_expr(context, left)?),
            op,
            Box::new(analyse_expr(context, right)?),
        )),
    };

    context.in_conditional = previous_in_conditional;

    result
}

fn analyse_comparison_with_var(
    context: &mut Context,
    var: Expr,
    op: TokenKind,
    right: Expr,
) -> Result<Expr, CalcError> {
    let right = analyse_expr(context, right)?;

    if context.comprehension_vars.is_none() {
        return Ok(Expr::Binary(
            Box::new(analyse_expr(context, var)?),
            op,
            Box::new(right),
        ));
    }

    // Make sure any comprehension variables
    // are added to context.comprehension_variables.
    let analysed_var = analyse_expr(context, var)?;
    let var_name = if let Expr::Var(identifier) = &analysed_var {
        &identifier.pure_name
    } else {
        unreachable!("Expected Expr::Var");
    };

    let vars = context.comprehension_vars.as_mut().unwrap();
    for ranged_var in vars {
        if &ranged_var.name == var_name {
            match op {
                TokenKind::GreaterThan => {
                    ranged_var.min = Expr::Binary(
                        Box::new(right),
                        TokenKind::Plus,
                        Box::new(Expr::Literal(1f64)),
                    );
                }
                TokenKind::LessThan => {
                    ranged_var.max = right;
                }
                TokenKind::GreaterOrEquals => {
                    ranged_var.min = right;
                }
                TokenKind::LessOrEquals => {
                    ranged_var.max = Expr::Binary(
                        Box::new(right),
                        TokenKind::Plus,
                        Box::new(Expr::Literal(1f64)),
                    );
                }
                _ => unreachable!(),
            }

            break;
        }
    }

    Ok(Expr::Binary(
        Box::new(Expr::Literal(0f64)),
        TokenKind::Equals,
        Box::new(Expr::Literal(0f64)),
    ))
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

    let adjacent_factor = if let Some(adjacent_factor) = adjacent_factor {
        Some(analyse_expr(context, adjacent_factor)?)
    } else {
        None
    };

    let is_comprehension_var = if let Some(vars) = &context.comprehension_vars {
        vars.iter().any(|x| x.name == identifier.pure_name)
    } else {
        false
    };

    if is_comprehension_var {
        with_adjacent(Expr::Var(identifier), adjacent_factor, adjacent_exponent)
    } else if context.symbol_table.contains_var(&identifier.pure_name)
        || (identifier.pure_name.len() == 1 && !context.in_equation)
    {
        with_adjacent(
            build_var(context, &identifier.full_name),
            adjacent_factor,
            adjacent_exponent,
        )
    } else if context
        .symbol_table
        .contains_var(identifier.get_name_without_lowered())
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

        if context.in_integral && second_last_char == 'd' && identifier.pure_name.len() >= 2 {
            let new_identifier: String = identifier_without_dx.iter().collect();
            with_adjacent(
                build_dx(context, &new_identifier, last_char)?,
                adjacent_factor,
                adjacent_exponent,
            )
        } else {
            build_split_up_vars(context, identifier, adjacent_factor, adjacent_exponent)
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
    let is_integral = identifier.pure_name == "integrate";
    if is_integral {
        context.in_integral = true;
    }

    let is_sum_prod = identifier.pure_name == "sum" || identifier.pure_name == "prod";
    if is_sum_prod {
        context.in_sum_prod = true;
    }

    // Don't perform equation solving on special functions
    if is_integral || is_sum_prod {
        context.in_equation = false;
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

    if is_sum_prod {
        context.in_sum_prod = false;
    }

    Ok(Expr::FnCall(identifier, arguments))
}

fn build_indexed_var(context: &mut Context, identifier: Identifier) -> Result<Expr, CalcError> {
    let underscore_pos = identifier.pure_name.find('_').unwrap();
    let var_name = &identifier.pure_name[0..underscore_pos];
    let lowered = &identifier.pure_name[underscore_pos + 1..];
    let lowered_expr = if !lowered.is_empty() && lowered.chars().next().unwrap_or('\0').is_digit(10)
    {
        Expr::Literal(lowered.parse::<f64>().unwrap_or(f64::NAN))
    } else {
        build_var(context, lowered)
    };

    Ok(Expr::Indexer(
        Box::new(build_var(context, var_name)),
        vec![lowered_expr],
    ))
}

fn build_dx(
    context: &mut Context,
    name_without_dx: &str,
    char_after_d: char,
) -> Result<Expr, CalcError> {
    if name_without_dx.is_empty() {
        Ok(Expr::Var(Identifier::from_full_name(&format!(
            "d{}",
            char_after_d
        ))))
    } else {
        Ok(Expr::Binary(
            Box::new(analyse_var(
                context,
                Identifier::from_full_name(name_without_dx),
                None,
                None,
            )?),
            TokenKind::Star,
            Box::new(Expr::Var(Identifier::from_full_name(&format!(
                "d{}",
                char_after_d
            )))),
        ))
    }
}

fn build_split_up_vars(
    context: &mut Context,
    identifier: Identifier,
    adjacent_factor: Option<Expr>,
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
        return with_adjacent(
            Expr::FnCall(
                Identifier::from_full_name(&identifier_without_last),
                vec![build_var(context, &last_char.to_string())],
            ),
            adjacent_factor,
            adjacent_exponent,
        );
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

fn build_var(context: &mut Context, name: &str) -> Expr {
    if let (Some(function_name), Some(params)) = (
        context.current_function_name.as_ref(),
        context.current_function_parameters.as_ref(),
    ) {
        let identifier = Identifier::parameter_from_name(name, function_name);
        if params.contains(&identifier.full_name) {
            return Expr::Var(identifier);
        }
    }

    if context.in_sum_prod && name == "n" {
        return Expr::Var(Identifier::from_full_name(name));
    }

    let var_exists = context.symbol_table.contains_var(name);
    if context.in_equation && !var_exists {
        context.equation_variable = Some(name.to_string());
    }

    if context.in_comprehension && !var_exists {
        if let Some(vars) = context.comprehension_vars.as_mut() {
            vars.push(RangedVar {
                name: name.to_string(),
                max: Expr::Literal(0f64),
                min: Expr::Literal(0f64),
            });
        }
    }

    Expr::Var(Identifier::from_full_name(name))
}
