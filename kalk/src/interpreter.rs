use crate::ast::{Expr, Stmt};
use crate::ast::{Identifier, RangedVar};
use crate::calculation_result::CalculationResult;
use crate::errors::KalkError;
use crate::kalk_value::KalkFloat;
use crate::kalk_value::KalkValue;
use crate::lexer::TokenKind;
use crate::parser::DECL_UNIT;
use crate::symbol_table::SymbolTable;
use crate::{as_number_or_zero, numerical};
use crate::{float, prelude};

pub const DEFAULT_MAX_RECURSION_DEPTH: u32 = 256;

pub struct Context<'a> {
    pub symbol_table: &'a mut SymbolTable,
    angle_unit: String,
    #[cfg(feature = "rug")]
    precision: u32,
    sum_variables: Option<Vec<SumVar>>,
    #[cfg(not(target_arch = "wasm32"))]
    timeout: Option<u128>,
    #[cfg(not(target_arch = "wasm32"))]
    start_time: std::time::SystemTime,
    is_approximation: bool,
    recursion_depth: u32,
    max_recursion_depth: u32,
    equation_variable: Option<String>,
}

impl<'a> Context<'a> {
    pub fn new(
        symbol_table: &'a mut SymbolTable,
        angle_unit: &str,
        #[cfg(feature = "rug")] precision: u32,
        timeout: Option<u128>,
    ) -> Self {
        Context {
            angle_unit: angle_unit.into(),
            symbol_table,
            #[cfg(feature = "rug")]
            precision,
            sum_variables: None,
            #[cfg(not(target_arch = "wasm32"))]
            timeout,
            #[cfg(not(target_arch = "wasm32"))]
            start_time: std::time::SystemTime::now(),
            is_approximation: false,
            recursion_depth: 0,
            max_recursion_depth: DEFAULT_MAX_RECURSION_DEPTH,
            equation_variable: None,
        }
    }

    pub fn set_max_recursion_depth(mut self, depth: u32) -> Self {
        self.max_recursion_depth = depth;

        self
    }

    pub fn interpret(
        &mut self,
        statements: Vec<Stmt>,
    ) -> Result<Option<CalculationResult>, KalkError> {
        for (i, stmt) in statements.iter().enumerate() {
            self.is_approximation = false;
            self.equation_variable = None;

            let num = eval_stmt(self, stmt)?;

            // Insert the last value into the `ans` variable.
            self.symbol_table.set(if num.has_unit() {
                Stmt::VarDecl(
                    Identifier::from_full_name("ans"),
                    Box::new(Expr::Unit(
                        num.get_unit().unwrap().to_string(),
                        Box::new(crate::ast::build_literal_ast(&num)),
                    )),
                )
            } else {
                Stmt::VarDecl(
                    Identifier::from_full_name("ans"),
                    Box::new(crate::ast::build_literal_ast(&num)),
                )
            });

            if i == statements.len() - 1 {
                if let Stmt::Expr(_) = stmt {
                    return Ok(Some(CalculationResult::new(
                        num,
                        10,
                        self.is_approximation,
                        self.equation_variable.clone(),
                    )));
                }
            }
        }

        Ok(None)
    }
}

struct SumVar {
    name: String,
    value: i128,
}

fn eval_stmt(context: &mut Context, stmt: &Stmt) -> Result<KalkValue, KalkError> {
    match stmt {
        Stmt::VarDecl(_, _) => eval_var_decl_stmt(context, stmt),
        Stmt::FnDecl(_, _, _) => eval_fn_decl_stmt(),
        Stmt::UnitDecl(_, _, _) => eval_unit_decl_stmt(),
        Stmt::Expr(expr) => eval_expr_stmt(context, expr),
    }
}

fn eval_var_decl_stmt(context: &mut Context, stmt: &Stmt) -> Result<KalkValue, KalkError> {
    context.symbol_table.insert(stmt.clone());
    Ok(KalkValue::from(1))
}

fn eval_fn_decl_stmt() -> Result<KalkValue, KalkError> {
    Ok(KalkValue::from(1)) // Nothing needs to happen here, since the parser will already have added the FnDecl's to the symbol table.
}

fn eval_unit_decl_stmt() -> Result<KalkValue, KalkError> {
    Ok(KalkValue::from(1))
}

fn eval_expr_stmt(context: &mut Context, expr: &Expr) -> Result<KalkValue, KalkError> {
    eval_expr(context, expr, None)
}

pub(crate) fn eval_expr(
    context: &mut Context,
    expr: &Expr,
    unit: Option<&String>,
) -> Result<KalkValue, KalkError> {
    #[cfg(not(target_arch = "wasm32"))]
    if let (Ok(elapsed), Some(timeout)) = (context.start_time.elapsed(), context.timeout) {
        if elapsed.as_millis() >= timeout {
            return Err(KalkError::TimedOut);
        }
    }

    match expr {
        Expr::Binary(left, op, right) => eval_binary_expr(context, left, op, right, unit),
        Expr::Unary(op, expr) => eval_unary_expr(context, op, expr, unit),
        Expr::Unit(identifier, expr) => eval_unit_expr(context, identifier, expr),
        Expr::Var(identifier) => eval_var_expr(context, identifier, unit),
        // copy trait for Float?
        Expr::Literal(value) => eval_literal_expr(context, value.clone(), unit),
        Expr::Boolean(value) => Ok(KalkValue::Boolean(*value)),
        Expr::Group(expr) => eval_group_expr(context, expr, unit),
        Expr::FnCall(identifier, expressions) => {
            context.recursion_depth += 1;
            let res = eval_fn_call_expr(context, identifier, expressions, unit);
            context.recursion_depth -= 1;
            res
        }
        Expr::Piecewise(pieces) => eval_piecewise(context, pieces, unit),
        Expr::Vector(values) => eval_vector(context, values),
        Expr::Matrix(rows) => eval_matrix(context, rows),
        Expr::Indexer(var, indexes) => eval_indexer(context, var, indexes, unit),
        Expr::Comprehension(left, conditions, vars) => Ok(KalkValue::Vector(eval_comprehension(
            context, left, conditions, vars,
        )?)),
        Expr::Equation(left, right, identifier) => eval_equation(context, left, right, identifier),
        Expr::Preevaluated(value) => Ok(value.clone()),
    }
}

fn eval_binary_expr(
    context: &mut Context,
    left_expr: &Expr,
    op: &TokenKind,
    right_expr: &Expr,
    unit: Option<&String>,
) -> Result<KalkValue, KalkError> {
    if let TokenKind::ToKeyword = op {
        // TODO: When the unit conversion function takes a Float instead of Expr,
        // move this to the match statement further down.
        if let Expr::Var(right_unit) = right_expr {
            let left_unit = eval_expr(context, left_expr, None)?.get_unit().cloned();
            return convert_unit(
                context,
                left_expr,
                left_unit.as_ref(),
                Some(&right_unit.full_name),
            );
            // TODO: Avoid evaluating this twice.
        }
    }

    let left = eval_expr(context, left_expr, None)?;
    let mut right = eval_expr(context, right_expr, None)?;
    if let Expr::Unary(TokenKind::Percent, _) = right_expr {
        right = right.mul(context, left.clone())?;
        if let TokenKind::Star = op {
            return Ok(right);
        }
    }

    let result = match op {
        TokenKind::Plus => left.add(context, right),
        TokenKind::Minus => left.sub(context, right),
        TokenKind::Star => left.mul(context, right),
        TokenKind::Slash => left.div(context, right),
        TokenKind::Percent => left.rem(context, right),
        TokenKind::Power => left.pow(context, right),
        TokenKind::Equals => left.eq(context, right),
        TokenKind::NotEquals => left.not_eq(context, right),
        TokenKind::GreaterThan => left.greater_than(context, right),
        TokenKind::LessThan => left.less_than(context, right),
        TokenKind::GreaterOrEquals => left.greater_or_equals(context, right),
        TokenKind::LessOrEquals => left.less_or_equals(context, right),
        TokenKind::And => left.and(&right),
        TokenKind::Or => left.or(&right),
        _ => Ok(KalkValue::from(1f64)),
    }?;

    if unit.is_some() {
        if let KalkValue::Number(real, imaginary, _) = result {
            return Ok(KalkValue::Number(real, imaginary, unit.cloned()));
        }
    };

    Ok(result)
}

fn eval_unary_expr(
    context: &mut Context,
    op: &TokenKind,
    expr: &Expr,
    unit: Option<&String>,
) -> Result<KalkValue, KalkError> {
    let num = eval_expr(context, expr, unit)?;

    match op {
        TokenKind::Minus => num.mul(context, KalkValue::from(-1f64)),
        TokenKind::Not => match num {
            KalkValue::Boolean(boolean) => Ok(KalkValue::Boolean(!boolean)),
            _ => Err(KalkError::InvalidOperator),
        },
        TokenKind::Percent => num.mul(context, KalkValue::from(0.01f64)),
        TokenKind::Exclamation => prelude::special_funcs::factorial(num),
        _ => Err(KalkError::InvalidOperator),
    }
}

fn eval_unit_expr(
    context: &mut Context,
    identifier: &str,
    expr: &Expr,
) -> Result<KalkValue, KalkError> {
    let angle_unit = &context.angle_unit.clone();
    if (identifier == "rad" || identifier == "deg") && angle_unit != identifier {
        return convert_unit(
            context,
            expr,
            Some(&identifier.to_string()),
            Some(angle_unit),
        );
    }

    eval_expr(context, expr, Some(&identifier.to_string()))
}

pub fn convert_unit(
    context: &mut Context,
    expr: &Expr,
    from_unit: Option<&String>,
    to_unit: Option<&String>,
) -> Result<KalkValue, KalkError> {
    let (from_unit, to_unit) = if let (Some(from_unit), Some(to_unit)) = (from_unit, to_unit) {
        (from_unit, to_unit)
    } else {
        return Err(KalkError::InvalidUnit);
    };

    if let Some(Stmt::UnitDecl(_, _, unit_def)) =
        context.symbol_table.get_unit(to_unit, from_unit).cloned()
    {
        context.symbol_table.insert(Stmt::VarDecl(
            Identifier::from_full_name(DECL_UNIT),
            Box::new(expr.clone()),
        ));

        let (real, imaginary, _) = as_number_or_zero!(eval_expr(context, &unit_def, None)?);
        Ok(KalkValue::Number(real, imaginary, Some(to_unit.clone())))
    } else {
        Err(KalkError::InvalidUnit)
    }
}

fn eval_var_expr(
    context: &mut Context,
    identifier: &Identifier,
    unit: Option<&String>,
) -> Result<KalkValue, KalkError> {
    // If there is a constant with this name, return a literal expression with its value
    if let Some(value) = prelude::CONSTANTS.get(identifier.full_name.as_ref() as &str) {
        return eval_expr(context, &Expr::Literal(float!(*value)), unit);
    }

    if let Some(sum_variables) = &context.sum_variables {
        let sum_variable = sum_variables
            .iter()
            .find(|x| x.name == identifier.full_name);
        if let Some(sum_variable) = sum_variable {
            return Ok(KalkValue::from(sum_variable.value));
        }
    }

    // Look for the variable in the symbol table
    let var_decl = context
        .symbol_table
        .get_var(identifier.full_name.as_ref() as &str)
        .cloned();
    if let Some(Stmt::VarDecl(_, expr)) = var_decl {
        eval_expr(context, &expr, unit)
    } else {
        Err(KalkError::UndefinedVar(identifier.full_name.clone()))
    }
}

#[allow(unused_variables)]
fn eval_literal_expr(
    context: &mut Context,
    value: KalkFloat,
    unit: Option<&String>,
) -> Result<KalkValue, KalkError> {

    #[allow(unused_mut)]
    let mut float = float!(value);
    #[cfg(feature = "rug")]
    float.set_prec(context.precision);

    Ok(KalkValue::Number(float, float!(0), unit.cloned()))
}

fn eval_group_expr(
    context: &mut Context,
    expr: &Expr,
    unit: Option<&String>,
) -> Result<KalkValue, KalkError> {
    eval_expr(context, expr, unit)
}

pub(crate) fn eval_fn_call_expr(
    context: &mut Context,
    identifier: &Identifier,
    expressions: &[Expr],
    unit: Option<&String>,
) -> Result<KalkValue, KalkError> {
    if context.recursion_depth > context.max_recursion_depth {
        return Err(KalkError::StackOverflow);
    }

    if identifier.prime_count > 0 {
        context.is_approximation = true;
    }

    // Special functions
    match identifier.full_name.as_ref() {
        "sum" | "prod" => {
            // If loop, eg. sum(k=1, 100, k)
            if let Expr::Binary(left, TokenKind::Equals, right) = &expressions[0] {
                if let Expr::Var(var_identifier) = &**left {
                    // Make sure exactly 3 arguments were supplied.
                    if expressions.len() != 3 {
                        return Err(KalkError::IncorrectAmountOfArguments(
                            3,
                            "sum/prod".into(),
                            expressions.len(),
                        ));
                    }

                    //(var_identifier.pure_name.as_ref(), &**right);
                    return eval_loop(
                        context,
                        identifier,
                        &var_identifier.pure_name,
                        right,
                        &expressions[1],
                        &expressions[2],
                        unit.cloned(),
                    );
                }
            }
        }
        "integrate" => {
            context.is_approximation = true;

            return match expressions.len() {
                3 => numerical::integrate_with_unknown_variable(
                    context,
                    &expressions[0],
                    &expressions[1],
                    &expressions[2],
                ),
                4 => numerical::integrate(
                    context,
                    &expressions[0],
                    &expressions[1],
                    &expressions[2],
                    if let Expr::Var(integration_variable) = &expressions[3] {
                        &integration_variable.full_name[1..]
                    } else {
                        return Err(KalkError::ExpectedDx);
                    },
                ),
                _ => Err(KalkError::IncorrectAmountOfArguments(
                    3,
                    "integrate".into(),
                    expressions.len(),
                )),
            };
        }
        _ => (),
    }

    // Prelude vector function
    if prelude::is_vector_func(&identifier.full_name) {
        let mut values = Vec::new();
        for expression in expressions {
            let value = eval_expr(context, expression, None)?;
            if expressions.len() == 1 {
                if let KalkValue::Vector(internal_values) = value {
                    values = internal_values;
                    break;
                }
            }

            values.push(value);
        }

        return prelude::call_vector_func(&identifier.full_name, KalkValue::Vector(values))
            .unwrap_or_else(|| Ok(KalkValue::nan()));
    }

    // Prelude
    let prelude_func = match expressions.len() {
        1 => {
            let x = eval_expr(context, &expressions[0], None)?;
            if identifier.prime_count > 0 {
                return numerical::derive_func(context, identifier, x);
            } else {
                prelude::call_unary_func(
                    context,
                    &identifier.full_name,
                    x,
                    &context.angle_unit.clone(),
                )
            }
        }
        2 => {
            let x = eval_expr(context, &expressions[0], None)?;
            let y = eval_expr(context, &expressions[1], None)?;
            prelude::call_binary_func(
                context,
                &identifier.full_name,
                x,
                y,
                &context.angle_unit.clone(),
            )
        }
        _ => None,
    };

    if let Some((result, _)) = prelude_func {
        if result.is_err() && expressions.len() == 1 {
            let x = eval_expr(context, &expressions[0], None)?;

            // If a vector/matrix was given, call the function on every item
            // in the vector/matrix.
            if let KalkValue::Vector(values) = x {
                let mut new_values = Vec::new();
                let mut success = true;
                for value in values {
                    if let Some(result) = prelude::call_unary_func(
                        context,
                        &identifier.full_name,
                        value,
                        &context.angle_unit.clone(),
                    ) {
                        new_values.push(result.0?);
                    } else {
                        success = false;
                        break;
                    }
                }

                if success {
                    return Ok(KalkValue::Vector(new_values));
                }
            } else if let KalkValue::Matrix(rows) = x {
                let mut new_rows = Vec::new();
                let mut success = true;
                for row in rows {
                    let mut new_row = Vec::new();
                    for value in row {
                        if let Some(result) = prelude::call_unary_func(
                            context,
                            &identifier.full_name,
                            value,
                            &context.angle_unit.clone(),
                        ) {
                            new_row.push(result.0?);
                        } else {
                            success = false;
                            break;
                        }
                    }

                    new_rows.push(new_row);
                }

                if success {
                    return Ok(KalkValue::Matrix(new_rows));
                }
            }
        }

        return result;
    }

    // Symbol Table
    let stmt_definition = context.symbol_table.get_fn(&identifier.full_name).cloned();

    match stmt_definition {
        Some(Stmt::FnDecl(_, arguments, fn_body)) => {
            if arguments.len() != expressions.len() {
                return Err(KalkError::IncorrectAmountOfArguments(
                    arguments.len(),
                    identifier.full_name.clone(),
                    expressions.len(),
                ));
            }

            // Initialise the arguments as their own variables.
            let mut new_argument_values = Vec::new();
            for (i, argument) in arguments.iter().enumerate() {
                let argument_identifier = if argument.contains('-') {
                    let identifier_parts: Vec<&str> = argument.split('-').collect();
                    Identifier::parameter_from_name(identifier_parts[1], identifier_parts[0])
                } else {
                    Identifier::from_full_name(argument)
                };
                let var_decl = Stmt::VarDecl(
                    argument_identifier,
                    Box::new(Expr::Preevaluated(eval_expr(
                        context,
                        &expressions[i],
                        None,
                    )?)),
                );

                // Don't set these values just yet,
                // to avoid affecting the value of arguments
                // during recursion.
                new_argument_values.push((argument, var_decl));
            }

            let mut old_argument_values = Vec::new();
            for (name, value) in new_argument_values {
                // Save the original argument values,
                // so that they can be reverted to after
                // the function call is evaluated.
                // This is necessary since recursive
                // function calls have the same argument names.
                old_argument_values.push(context.symbol_table.get_and_remove_var(name));

                // Now set the new variable value
                eval_stmt(context, &value)?;
            }

            let fn_value = eval_expr(context, &fn_body, unit);

            // Revert to original argument values
            for old_argument_value in old_argument_values.into_iter().flatten() {
                context.symbol_table.insert(old_argument_value);
            }

            fn_value
        }
        _ => Err(KalkError::UndefinedFn(identifier.full_name.clone())),
    }
}

fn eval_loop(
    context: &mut Context,
    identifier: &Identifier,
    var_name: &str,
    start_expr: &Expr,
    end_expr: &Expr,
    expression: &Expr,
    unit: Option<String>,
) -> Result<KalkValue, KalkError> {
    if context.sum_variables.is_none() {
        context.sum_variables = Some(Vec::new());
    }

    {
        let sum_variables = context.sum_variables.as_mut().unwrap();
        sum_variables.push(SumVar {
            name: var_name.into(),
            value: 0,
        });
    }

    let start = eval_expr(context, start_expr, None)?.to_f64() as i128;
    let end = eval_expr(context, end_expr, None)?.to_f64() as i128;
    let sum_else_prod = match identifier.full_name.as_ref() {
        "sum" => true,
        "prod" => false,
        _ => unreachable!(),
    };
    let mut sum = if sum_else_prod {
        KalkValue::from(0f64)
    } else {
        KalkValue::from(1f64)
    };

    for n in start..=end {
        let sum_variables = context.sum_variables.as_mut().unwrap();
        sum_variables.last_mut().unwrap().value = n;

        let eval = eval_expr(context, expression, None)?;
        if sum_else_prod {
            sum = sum.add(context, eval)?;
        } else {
            sum = sum.mul(context, eval)?;
        }
    }

    let sum_variables = context.sum_variables.as_mut().unwrap();
    sum_variables.pop();

    let (sum_real, sum_imaginary, _) = as_number_or_zero!(sum);

    Ok(KalkValue::Number(sum_real, sum_imaginary, unit))
}

fn eval_piecewise(
    context: &mut Context,
    pieces: &[crate::ast::ConditionalPiece],
    unit: Option<&String>,
) -> Result<KalkValue, KalkError> {
    for piece in pieces {
        if let KalkValue::Boolean(condition_is_true) = eval_expr(context, &piece.condition, unit)? {
            if condition_is_true {
                return eval_expr(context, &piece.expr, unit);
            }
        }
    }

    Err(KalkError::PiecewiseConditionsAreFalse)
}

fn eval_vector(context: &mut Context, values: &[Expr]) -> Result<KalkValue, KalkError> {
    let mut eval_values = Vec::new();
    for value in values {
        eval_values.push(eval_expr(context, value, None)?);
    }

    Ok(KalkValue::Vector(eval_values))
}

fn eval_matrix(context: &mut Context, rows: &[Vec<Expr>]) -> Result<KalkValue, KalkError> {
    let mut eval_rows = Vec::new();
    for row in rows {
        let mut eval_row = Vec::new();
        for value in row {
            eval_row.push(eval_expr(context, value, None)?)
        }

        eval_rows.push(eval_row);
    }

    Ok(KalkValue::Matrix(eval_rows))
}

fn eval_indexer(
    context: &mut Context,
    var: &Expr,
    index_expressions: &[Expr],
    unit: Option<&String>,
) -> Result<KalkValue, KalkError> {
    let var_value = eval_expr(context, var, unit)?;
    match var_value {
        KalkValue::Vector(values) => {
            if index_expressions.len() != 1 {
                return Err(KalkError::IncorrectAmountOfIndexes(
                    index_expressions.len(),
                    1,
                ));
            }

            let index = as_indices(context, index_expressions)?[0];
            if let Some(value) = values.get(index - 1) {
                Ok(value.clone())
            } else {
                Err(KalkError::ItemOfIndexDoesNotExist(vec![index]))
            }
        }
        KalkValue::Matrix(rows) => {
            let indices = as_indices(context, index_expressions)?;
            if indices.len() == 1 {
                let row_index = indices[0];
                return if let Some(row) = rows.get(row_index - 1) {
                    Ok(KalkValue::Vector(row.clone()))
                } else {
                    Err(KalkError::ItemOfIndexDoesNotExist(vec![row_index]))
                };
            }

            if indices.len() != 2 {
                return Err(KalkError::IncorrectAmountOfIndexes(indices.len(), 2));
            }

            let row_index = indices[0];
            let column_index = indices[1];
            if row_index == 0 || column_index == 0 {
                return Err(KalkError::ItemOfIndexDoesNotExist(vec![
                    row_index,
                    column_index,
                ]));
            }

            if let Some(row) = rows.get(row_index - 1) {
                if let Some(value) = row.get(column_index - 1) {
                    return Ok(value.clone());
                }
            }

            Err(KalkError::ItemOfIndexDoesNotExist(vec![
                row_index,
                column_index,
            ]))
        }
        _ => Err(KalkError::CanOnlyIndexX),
    }
}

fn as_indices(context: &mut Context, expressions: &[Expr]) -> Result<Vec<usize>, KalkError> {
    let mut indices = Vec::new();
    for expr in expressions {
        let value = eval_expr(context, expr, None)?;
        if value.has_imaginary() {
            return Err(KalkError::CannotIndexByImaginary);
        }

        let index = value.to_f64() as usize;
        if index == 0 {
            return Err(KalkError::ItemOfIndexDoesNotExist(vec![index]));
        }

        indices.push(index);
    }

    Ok(indices)
}

fn eval_comprehension(
    context: &mut Context,
    left: &Expr,
    conditions: &[Expr],
    vars: &[RangedVar],
) -> Result<Vec<KalkValue>, KalkError> {
    if vars.len() != conditions.len() {
        return Err(KalkError::InvalidComprehension(String::from("Expected a new variable to be introduced for every condition (conditions are comma separated).")));
    }

    let condition = conditions.first().unwrap();
    let var = vars.first().unwrap();
    context.symbol_table.insert(Stmt::VarDecl(
        Identifier::from_full_name(&var.name),
        Box::new(Expr::Literal(float!(0f64))),
    ));

    let min = eval_expr(context, &var.min, None)?.to_f64() as i32;
    let max = eval_expr(context, &var.max, None)?.to_f64() as i32;

    let mut values = Vec::new();
    for i in min..max {
        context.symbol_table.set(Stmt::VarDecl(
            Identifier::from_full_name(&var.name),
            Box::new(Expr::Literal(float!(i))),
        ));

        if conditions.len() > 1 {
            let x = eval_comprehension(context, left, &conditions[1..], &vars[1..]);
            if let Err(err) = x {
                context.symbol_table.get_and_remove_var(&var.name);
                return Err(err);
            }

            for value in x.unwrap() {
                values.push(Ok(value));
            }
        }

        let condition = eval_expr(context, condition, None);
        match condition {
            Ok(KalkValue::Boolean(boolean)) => {
                if boolean && vars.len() == 1 {
                    values.push(eval_expr(context, left, None));
                }
            }
            Err(err) => values.push(Err(err)),
            _ => (),
        }
    }

    context.symbol_table.get_and_remove_var(&var.name);

    let mut unwrapped_values = Vec::new();
    for value in values {
        match value {
            Ok(unwrapped_value) => unwrapped_values.push(unwrapped_value),
            Err(err) => return Err(err),
        }
    }

    Ok(unwrapped_values)
}

fn eval_equation(
    context: &mut Context,
    left: &Expr,
    right: &Expr,
    unknown_var: &Identifier,
) -> Result<KalkValue, KalkError> {
    context.is_approximation = true;
    context.equation_variable = Some(unknown_var.full_name.clone());

    let expr = Expr::Binary(
        Box::new(left.clone()),
        TokenKind::Minus,
        Box::new(right.clone()),
    );
    numerical::find_root(context, &expr, &unknown_var.full_name)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::TokenKind::*;
    use crate::test_helpers::*;
    use test_case::test_case;

    lazy_static::lazy_static! {
        static ref DEG_RAD_UNIT: Stmt = unit_decl(
            "deg",
            "rad",
            binary(
                binary(
                    var(crate::parser::DECL_UNIT),
                    TokenKind::Star,
                    f64_to_float_literal(180f64),
                ),
                TokenKind::Slash,
                var("pi"),
            ),
        );
        static ref RAD_DEG_UNIT: Stmt = unit_decl(
            "rad",
            "deg",
            binary(
                binary(var(crate::parser::DECL_UNIT), TokenKind::Star, var("pi")),
                TokenKind::Slash,
                f64_to_float_literal(180f64),
            ),
        );
    }

    fn interpret_with_unit(stmt: Stmt) -> Result<Option<CalculationResult>, KalkError> {
        let mut symbol_table = SymbolTable::new();
        symbol_table
            .insert(DEG_RAD_UNIT.clone())
            .insert(RAD_DEG_UNIT.clone());

        context(&mut symbol_table, "rad").interpret(vec![stmt])
    }

    fn interpret(stmt: Stmt) -> Result<Option<KalkValue>, KalkError> {
        if let Some(result) = interpret_with_unit(stmt)? {
            Ok(Some(result.get_value()))
        } else {
            Ok(None)
        }
    }

    #[cfg(feature = "rug")]
    fn context<'a>(symbol_table: &'a mut SymbolTable, angle_unit: &str) -> Context<'a> {
        Context::new(symbol_table, angle_unit, 63, None)
    }

    #[cfg(not(feature = "rug"))]
    fn context<'a>(symbol_table: &'a mut SymbolTable, angle_unit: &str) -> Context<'a> {
        Context::new(symbol_table, angle_unit, None)
    }

    fn cmp(x: KalkValue, y: f64) -> bool {
        (x.to_f64() - y).abs() < 0.0001
    }

    fn bool(x: &KalkValue) -> bool {
        if let KalkValue::Boolean(boolean_value) = x {
            *boolean_value
        } else {
            false
        }
    }

    #[test]
    fn test_literal() {
        let stmt = Stmt::Expr(f64_to_float_literal(1f64));

        assert_eq!(interpret(stmt).unwrap().unwrap().to_f64(), 1f64);
    }

    #[test]
    fn test_binary() {
        let add = Stmt::Expr(binary(
            f64_to_float_literal(2f64),
            Plus,
            f64_to_float_literal(3f64),
        ));
        let sub = Stmt::Expr(binary(
            f64_to_float_literal(2f64),
            Minus,
            f64_to_float_literal(3f64),
        ));
        let mul = Stmt::Expr(binary(
            f64_to_float_literal(2f64),
            Star,
            f64_to_float_literal(3f64),
        ));
        let div = Stmt::Expr(binary(
            f64_to_float_literal(2f64),
            Slash,
            f64_to_float_literal(4f64),
        ));
        let pow = Stmt::Expr(binary(
            f64_to_float_literal(2f64),
            Power,
            f64_to_float_literal(3f64),
        ));
        let equals = Stmt::Expr(binary(
            f64_to_float_literal(2f64),
            Equals,
            f64_to_float_literal(3f64),
        ));
        let not_equals = Stmt::Expr(binary(
            f64_to_float_literal(2f64),
            NotEquals,
            f64_to_float_literal(3f64),
        ));
        let greater_than = Stmt::Expr(binary(
            f64_to_float_literal(2f64),
            GreaterThan,
            f64_to_float_literal(3f64),
        ));
        let less_than = Stmt::Expr(binary(
            f64_to_float_literal(2f64),
            LessThan,
            f64_to_float_literal(3f64),
        ));
        let greater_or_equals = Stmt::Expr(binary(
            f64_to_float_literal(2f64),
            GreaterOrEquals,
            f64_to_float_literal(3f64),
        ));
        let less_or_equals = Stmt::Expr(binary(
            f64_to_float_literal(2f64),
            LessOrEquals,
            f64_to_float_literal(3f64),
        ));

        assert_eq!(interpret(add).unwrap().unwrap().to_f64(), 5f64);
        assert_eq!(interpret(sub).unwrap().unwrap().to_f64(), -1f64);
        assert_eq!(interpret(mul).unwrap().unwrap().to_f64(), 6f64);
        assert_eq!(interpret(div).unwrap().unwrap().to_f64(), 0.5f64);
        assert_eq!(interpret(pow).unwrap().unwrap().to_f64(), 8f64);

        let result = interpret(equals).unwrap().unwrap();
        assert!(!bool(&result));
        assert!(result.to_f64().is_nan());

        let result = interpret(not_equals).unwrap().unwrap();
        assert!(bool(&result));
        assert!(result.to_f64().is_nan());

        let result = interpret(greater_than).unwrap().unwrap();
        assert!(!bool(&result));
        assert!(result.to_f64().is_nan());

        let result = interpret(less_than).unwrap().unwrap();
        assert!(bool(&result));
        assert!(result.to_f64().is_nan());

        let result = interpret(greater_or_equals).unwrap().unwrap();
        assert!(!bool(&result));
        assert!(result.to_f64().is_nan());

        let result = interpret(less_or_equals).unwrap().unwrap();
        assert!(bool(&result));
        assert!(result.to_f64().is_nan());
    }

    #[test]
    fn test_percent() {
        let stmt = Stmt::Expr(binary(
            f64_to_float_literal(5f64),
            Percent,
            group(binary(
                f64_to_float_literal(3f64),
                Plus,
                unary(Percent, f64_to_float_literal(2f64)),
            )),
        ));

        assert!(cmp(interpret(stmt).unwrap().unwrap(), 1.94f64));
    }

    #[test]
    fn test_unary() {
        let neg = Stmt::Expr(unary(Minus, f64_to_float_literal(1f64)));
        let fact = Stmt::Expr(unary(Exclamation, f64_to_float_literal(5f64)));

        assert_eq!(interpret(neg).unwrap().unwrap().to_f64(), -1f64);
        assert_eq!(interpret(fact).unwrap().unwrap().to_f64(), 120f64);
    }

    #[test]
    fn test_angle_units() {
        let rad_explicit = Stmt::Expr(fn_call(
            "sin",
            vec![*unit("rad", f64_to_float_literal(1f64))],
        ));
        let deg_explicit = Stmt::Expr(fn_call(
            "sin",
            vec![*unit("deg", f64_to_float_literal(1f64))],
        ));
        let implicit = Stmt::Expr(fn_call("sin", vec![*f64_to_float_literal(1f64)]));

        assert!(cmp(interpret(rad_explicit).unwrap().unwrap(), 0.84147098));
        assert!(cmp(interpret(deg_explicit).unwrap().unwrap(), 0.01745240));

        let mut rad_symbol_table = SymbolTable::new();
        rad_symbol_table
            .insert(DEG_RAD_UNIT.clone())
            .insert(RAD_DEG_UNIT.clone());
        let mut deg_symbol_table = SymbolTable::new();
        deg_symbol_table
            .insert(DEG_RAD_UNIT.clone())
            .insert(RAD_DEG_UNIT.clone());
        let mut rad_context = context(&mut rad_symbol_table, "rad");
        let mut deg_context = context(&mut deg_symbol_table, "deg");

        assert!(cmp(
            rad_context
                .interpret(vec![implicit.clone()])
                .unwrap()
                .unwrap()
                .get_value(),
            0.84147098
        ));
        assert!(cmp(
            deg_context
                .interpret(vec![implicit])
                .unwrap()
                .unwrap()
                .get_value(),
            0.01745240
        ));
    }

    #[test]
    fn test_var() {
        let stmt = Stmt::Expr(var("x"));

        // Prepare by inserting a variable declaration in the symbol table.
        let mut symbol_table = SymbolTable::new();
        symbol_table.insert(var_decl("x", f64_to_float_literal(1f64)));

        let mut context = context(&mut symbol_table, "rad");
        assert_eq!(
            context.interpret(vec![stmt]).unwrap().unwrap().to_f64(),
            1f64
        );
    }

    #[test]
    fn test_undefined_var() {
        let stmt = Stmt::Expr(var("x"));

        assert_eq!(
            interpret(stmt),
            Err(KalkError::UndefinedVar(String::from("x")))
        );
    }

    #[test]
    fn test_var_decl() {
        let stmt = var_decl("x", f64_to_float_literal(1f64));
        let mut symbol_table = SymbolTable::new();
        context(&mut symbol_table, "rad")
            .interpret(vec![stmt])
            .unwrap();

        assert!(symbol_table.contains_var("x"));
    }

    #[test]
    fn test_fn() {
        let stmt = Stmt::Expr(fn_call("f", vec![*f64_to_float_literal(1f64)]));

        // Prepare by inserting a variable declaration in the symbol table.
        let mut symbol_table = SymbolTable::new();
        symbol_table.insert(fn_decl(
            "f",
            vec![String::from("x")],
            binary(var("x"), TokenKind::Plus, f64_to_float_literal(2f64)),
        ));

        let mut context = context(&mut symbol_table, "rad");
        assert_eq!(
            context.interpret(vec![stmt]).unwrap().unwrap().to_f64(),
            3f64
        );
    }

    #[test]
    fn test_undefined_fn() {
        let stmt = Stmt::Expr(fn_call("f", vec![*f64_to_float_literal(1f64)]));

        assert_eq!(
            interpret(stmt),
            Err(KalkError::UndefinedFn(String::from("f")))
        );
    }

    #[test_case(1f64, 2f64, 9f64)]
    #[test_case(1.2f64, 2.3f64, 9f64)]
    fn test_sum_fn(start: f64, to: f64, result: f64) {
        let stmt = Stmt::Expr(fn_call(
            "sum",
            vec![
                *binary(var("n"), TokenKind::Equals, f64_to_float_literal(start)),
                *f64_to_float_literal(to),
                *binary(var("n"), TokenKind::Plus, f64_to_float_literal(3f64)),
            ],
        ));

        assert_eq!(interpret(stmt).unwrap().unwrap().to_f64(), result);
    }

    #[test]
    fn test_integrate_fn() {
        let stmt = Stmt::Expr(fn_call(
            "integrate",
            vec![
                *f64_to_float_literal(2f64),
                *f64_to_float_literal(4f64),
                *binary(
                    binary(var("x"), TokenKind::Power, f64_to_float_literal(3f64)),
                    TokenKind::Star,
                    var("dx"),
                ),
            ],
        ));

        assert!((interpret(stmt).unwrap().unwrap().to_f64() - 60f64).abs() < 0.001f64);
    }
}
