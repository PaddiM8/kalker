use crate::{
    ast::{ConditionalPiece, Expr, Identifier, RangedVar, Stmt},
    errors::KalkError,
    float, inverter,
    lexer::TokenKind,
    parser, prelude,
    symbol_table::SymbolTable,
};

pub(crate) struct Context<'a> {
    pub(crate) symbol_table: &'a mut SymbolTable,
    current_function_name: Option<String>,
    current_function_parameters: Option<Vec<String>>,
    equation_variable: Option<String>,
    in_integral: bool,
    in_sum_prod: bool,
    sum_variable_names: Option<Vec<String>>,
    in_unit_decl: bool,
    in_conditional: bool,
    in_equation: bool,
    in_comprehension: bool,
    comprehension_vars: Option<Vec<RangedVar>>,
}

pub(crate) fn analyse_stmt(
    symbol_table: &mut SymbolTable,
    statement: Stmt,
) -> Result<Stmt, KalkError> {
    let mut context = Context {
        symbol_table,
        current_function_name: None,
        current_function_parameters: None,
        equation_variable: None,
        in_integral: false,
        in_sum_prod: false,
        sum_variable_names: None,
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

fn analyse_stmt_expr(context: &mut Context, value: Expr) -> Result<Stmt, KalkError> {
    Ok(
        if let Expr::Binary(left, TokenKind::Equals, right) = value {
            if let Some((identifier, parameters)) = is_fn_decl(&left) {
                return build_fn_decl_from_scratch(context, identifier, parameters, *right);
            }

            match *left {
                Expr::FnCall(identifier, arguments)
                    if !prelude::is_prelude_func(&identifier.full_name) =>
                {
                    // First loop through with a reference
                    // to arguments, to be able to back-track if
                    // one of the arguments can't be made into a parameter.
                    if identifier.prime_count != 0
                        || arguments
                            .iter()
                            .any(|argument| !matches!(argument, Expr::Var(_)))
                    {
                        // Analyse as 0f64 + fn_call = right so that
                        // it won't come here again.
                        return analyse_stmt_expr(
                            context,
                            Expr::Binary(
                                Box::new(Expr::Binary(
                                    Box::new(Expr::Literal(float!(0f64))),
                                    TokenKind::Plus,
                                    Box::new(Expr::FnCall(identifier, arguments)),
                                )),
                                TokenKind::Equals,
                                right,
                            ),
                        );
                    }

                    let mut parameters = Vec::new();
                    for argument in arguments {
                        if let Expr::Var(parameter_identifier) = argument {
                            parameters.push(parameter_identifier.full_name);
                        } else {
                            unreachable!()
                        }
                    }

                    let fn_decl = Stmt::FnDecl(identifier, parameters, right);
                    context.symbol_table.insert(fn_decl.clone());

                    fn_decl
                }
                Expr::Var(identifier) if !context.in_conditional => {
                    if inverter::contains_var(context.symbol_table, &right, &identifier.full_name) {
                        return Err(KalkError::VariableReferencesItself);
                    }

                    if prelude::is_constant(&identifier.full_name) {
                        return Err(KalkError::UnableToOverrideConstant(identifier.pure_name));
                    }

                    let result =
                        Stmt::VarDecl(identifier, Box::new(analyse_expr(context, *right)?));
                    context.symbol_table.insert(result.clone());

                    result
                }
                _ => Stmt::Expr(Box::new(analyse_expr(
                    context,
                    Expr::Binary(left, TokenKind::Equals, right),
                )?)),
            }
        } else {
            Stmt::Expr(Box::new(analyse_expr(context, value)?))
        },
    )
}

pub fn is_fn_decl(expr: &Expr) -> Option<(Identifier, Vec<String>)> {
    if let Expr::Binary(left, TokenKind::Star, right) = expr {
        let identifier = if let Expr::Var(identifier) = &**left {
            identifier
        } else {
            return None;
        };

        let exprs = match &**right {
            Expr::Vector(exprs) => exprs.iter().collect(),
            Expr::Group(expr) => vec![&**expr],
            _ => return None,
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

        if !prelude::is_prelude_func(&identifier.full_name) {
            return Some((identifier.clone(), parameters));
        }
    }

    None
}

fn build_fn_decl_from_scratch(
    context: &mut Context,
    identifier: Identifier,
    parameters: Vec<String>,
    right: Expr,
) -> Result<Stmt, KalkError> {
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

    Ok(fn_decl)
}

fn analyse_expr(context: &mut Context, expr: Expr) -> Result<Expr, KalkError> {
    Ok(match expr {
        Expr::Binary(left, op, right) => analyse_binary(context, *left, op, *right)?,
        Expr::Unary(op, value) => Expr::Unary(op, Box::new(analyse_expr(context, *value)?)),
        Expr::Unit(name, value) => Expr::Unit(name, Box::new(analyse_expr(context, *value)?)),
        Expr::Var(identifier) => analyse_var(context, identifier, None, None)?,
        Expr::Group(value) => Expr::Group(Box::new(analyse_expr(context, *value)?)),
        Expr::FnCall(identifier, arguments) => analyse_fn(context, identifier, arguments)?,
        Expr::Literal(_) | Expr::Boolean(_) => expr,
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

            if analysed_values.len() == 1 && matches!(analysed_values[0], Expr::Comprehension(_, _, _)) {
                analysed_values.pop().unwrap()
            } else {
                Expr::Vector(analysed_values)
            }
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
        Expr::Equation(left, right, identifier) => Expr::Equation(left, right, identifier),
        Expr::Preevaluated(_) => expr,
    })
}

fn analyse_binary(
    context: &mut Context,
    left: Expr,
    op: TokenKind,
    right: Expr,
) -> Result<Expr, KalkError> {
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

            // If it has already been set to false manually somewhere else
            // or if there is no equation variable,
            // abort and analyse as a comparison instead.
            if !context.in_equation || context.equation_variable.is_none() {
                context.in_conditional = true;
                let result = analyse_binary(context, left, op, right);
                context.in_conditional = previous_in_conditional;

                return result;
            }

            context.in_equation = false;

            let var_name = context.equation_variable.as_ref().unwrap();
            let identifier = Identifier::from_full_name(var_name);
            context.equation_variable = None;

            Ok(Expr::Equation(Box::new(left), Box::new(right), identifier))
        }
        (Expr::Var(_), TokenKind::Star, _) => {
            if let Expr::Var(identifier) = left {
                analyse_var(context, identifier, Some(right), None)
            } else {
                unreachable!()
            }
        }
        (_, TokenKind::Power, _) => match (left, right) {
            (left, Expr::Var(identifier)) if &identifier.full_name == "T" => Ok(Expr::FnCall(
                Identifier::from_full_name("transpose"),
                vec![analyse_expr(context, left)?],
            )),
            (Expr::Var(identifier), right) => {
                let right = analyse_expr(context, right)?;
                analyse_var(context, identifier, None, Some(right))
            }
            (left, right) => Ok(Expr::Binary(
                Box::new(analyse_expr(context, left)?),
                TokenKind::Power,
                Box::new(analyse_expr(context, right)?),
            )),
        },
        (_, TokenKind::Colon, _) => {
            context.in_comprehension = true;
            context.in_conditional = true;

            let comprehension_vars_index =
                if let Some(existing_comprehension_vars) = &context.comprehension_vars {
                    existing_comprehension_vars.len()
                } else {
                    context.comprehension_vars = Some(Vec::new());
                    0
                };

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
            let mut all_vars = context.comprehension_vars.take().unwrap();
            let vars = all_vars.drain(comprehension_vars_index..).collect();
            context.comprehension_vars = Some(all_vars);

            Ok(Expr::Comprehension(Box::new(left), conditions, vars))
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
) -> Result<Expr, KalkError> {
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
                        Box::new(Expr::Literal(float!(1f64))),
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
                        Box::new(Expr::Literal(float!(1f64))),
                    );
                }
                _ => unreachable!(),
            }

            break;
        }
    }

    Ok(Expr::Binary(
        Box::new(Expr::Literal(float!(0f64))),
        TokenKind::Equals,
        Box::new(Expr::Literal(float!(0f64))),
    ))
}

fn analyse_var(
    context: &mut Context,
    identifier: Identifier,
    adjacent_factor: Option<Expr>,
    adjacent_exponent: Option<Expr>,
) -> Result<Expr, KalkError> {
    let adjacent_factor = if let Some(adjacent_factor) = adjacent_factor {
        Some(analyse_expr(context, adjacent_factor)?)
    } else {
        None
    };

    if context.symbol_table.contains_fn(&identifier.full_name) {
        if let Some(Expr::Group(arg)) = adjacent_factor {
            return Ok(Expr::FnCall(identifier, vec![*arg]));
        }

        if let Some(Expr::Vector(args)) = adjacent_factor {
            return Ok(Expr::FnCall(identifier, args));
        }

        return Ok(Expr::FnCall(identifier, vec![adjacent_factor.unwrap()]));
    }

    let is_comprehension_var = if let Some(vars) = &context.comprehension_vars {
        vars.iter().any(|x| x.name == identifier.pure_name)
    } else {
        false
    };

    if is_comprehension_var {
        with_adjacent(Expr::Var(identifier), adjacent_factor, adjacent_exponent)
    } else if context.symbol_table.contains_var(&identifier.pure_name)
        || (identifier.pure_name.len() == 1 && !context.in_equation)
        || context.current_function_parameters.is_some()
            && context
                .current_function_parameters
                .as_ref()
                .unwrap()
                .iter()
                .any(|param| param[2..] == identifier.pure_name)
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
            let is_parameter = if let (Some(fn_name), Some(parameters)) = (
                &context.current_function_name,
                &context.current_function_parameters,
            ) {
                parameters.contains(&identifier.full_name)
                    || parameters.contains(
                        &Identifier::parameter_from_name(&identifier.full_name, fn_name).full_name,
                    )
            } else {
                false
            };

            if !is_parameter {
                context.equation_variable = Some(identifier.full_name.clone());

                return with_adjacent(
                    build_var(context, &identifier.full_name),
                    adjacent_factor,
                    adjacent_exponent,
                );
            }
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
) -> Result<Expr, KalkError> {
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

#[cfg(feature = "rug")]
fn parse_float_from_str(s: &str) -> rug::Float {
    rug::Float::parse(s).map_or(float!(f64::NAN), |valid| float!(valid))
}

#[cfg(not(feature = "rug"))]
fn parse_float_from_str(s: &str) -> f64 {
    s.parse::<f64>().unwrap_or(f64::NAN)
}

fn build_indexed_var(context: &mut Context, identifier: Identifier) -> Result<Expr, KalkError> {
    let underscore_pos = identifier.pure_name.find('_').unwrap();
    let var_name = &identifier.pure_name[0..underscore_pos];
    let lowered = &identifier.pure_name[underscore_pos + 1..];
    let lowered_expr =
        if !lowered.is_empty() && lowered.chars().next().unwrap_or('\0').is_ascii_digit() {
            Expr::Literal(parse_float_from_str(lowered))
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
) -> Result<Expr, KalkError> {
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
) -> Result<Expr, KalkError> {
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

    with_adjacent(left, adjacent_factor, adjacent_exponent)
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

    if context.in_sum_prod
        && context
            .sum_variable_names
            .as_ref()
            .unwrap()
            .contains(&name.to_string())
    {
        return Expr::Var(Identifier::from_full_name(name));
    }

    let var_exists = context.symbol_table.contains_var(name);
    let fn_exists = context.symbol_table.contains_fn(name);
    if context.in_equation && !var_exists && !fn_exists {
        context.equation_variable = Some(name.to_string());
    }

    if context.in_comprehension && !var_exists {
        if let Some(vars) = context.comprehension_vars.as_mut() {
            vars.push(RangedVar {
                name: name.to_string(),
                max: Expr::Literal(float!(0f64)),
                min: Expr::Literal(float!(0f64)),
            });
        }
    }

    Expr::Var(Identifier::from_full_name(name))
}

fn analyse_fn(
    context: &mut Context,
    identifier: Identifier,
    arguments: Vec<Expr>,
) -> Result<Expr, KalkError> {
    let is_integral = identifier.pure_name == "integrate";
    let prev_in_integral = context.in_integral;
    if is_integral {
        context.in_integral = true;
    }

    let prev_in_sum_prod = context.in_sum_prod;
    let is_sum_prod = identifier.pure_name == "sum" || identifier.pure_name == "prod";
    if is_sum_prod {
        context.in_sum_prod = true;
        if context.sum_variable_names.is_none() {
            context.sum_variable_names = Some(Vec::new());
        }
    }

    // Don't perform equation solving on special functions
    if is_integral || is_sum_prod {
        context.in_equation = false;
    }

    let mut analysed_arguments = Vec::new();
    for (i, argument) in arguments.iter().enumerate() {
        if i == 0 && context.in_sum_prod {
            context.in_conditional = true;
            let vars = context.sum_variable_names.as_mut().unwrap();
            if let Expr::Binary(left, TokenKind::Equals, _) = argument {
                if let Expr::Var(var_identifier) = &**left {
                    vars.push(var_identifier.pure_name.clone());
                } else {
                    vars.push(String::from("n"));
                }
            } else {
                vars.push(String::from("n"));
            }
        }

        analysed_arguments.push(analyse_expr(context, argument.to_owned())?);
        context.in_conditional = false;
    }

    context.in_integral = prev_in_integral;

    if is_sum_prod {
        context.in_sum_prod = prev_in_sum_prod;
        let vars = context.sum_variable_names.as_mut().unwrap();
        vars.pop();
    }

    Ok(Expr::FnCall(identifier, analysed_arguments))
}

// #[cfg(test)]
// mod test {
//     use test_case::test_case;
//     #[test_case("1")]
//     #[test_case("500000000000000002")]
//     #[test_case("-5000000000000000002")]
//     fn test_float_from_str(s: &str) {
//         // failing assertion to test this
//         assert_eq!(crate::analysis::parse_float_from_str(s), crate::float!(1f64));
//     }
// }
