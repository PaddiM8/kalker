use crate::ast::Identifier;
use crate::calculation_result::CalculationResult;
use crate::{
    ast::{Expr, Stmt},
    interpreter, inverter,
    lexer::{Lexer, Token, TokenKind},
    prelude,
    symbol_table::SymbolTable,
};
use wasm_bindgen::prelude::*;

pub const DECL_UNIT: &'static str = ".u";
pub const DEFAULT_ANGLE_UNIT: &'static str = "rad";

/// Struct containing the current state of the parser. It stores user-defined functions and variables.
#[wasm_bindgen]
pub struct Context {
    tokens: Vec<Token>,
    pos: usize,
    symbol_table: SymbolTable,
    angle_unit: String,
    timeout: Option<u32>,
    /// This is true whenever the parser is currently parsing a unit declaration.
    /// It is necessary to keep track of this in order to know when to find (figure out) units that haven't been defined yet.
    /// Unit names are instead treated as variables.
    parsing_unit_decl: bool,
    /// When a unit declaration is being parsed, this value will be set
    /// whenever a unit in the expression is found. Eg. unit a = 3b, it will be set to Some("b")
    unit_decl_base_unit: Option<String>,
    parsing_identifier_stmt: bool,
    equation_variable: Option<String>,
    contains_equation_equal_sign: bool,
    is_in_integral: bool,
    current_function: Option<String>,
    current_function_parameters: Option<Vec<String>>,
    other_radix: Option<u8>,
}

#[wasm_bindgen]
impl Context {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        let mut context = Self {
            tokens: Vec::new(),
            pos: 0,
            symbol_table: SymbolTable::new(),
            angle_unit: DEFAULT_ANGLE_UNIT.into(),
            timeout: None,
            parsing_unit_decl: false,
            unit_decl_base_unit: None,
            parsing_identifier_stmt: false,
            equation_variable: None,
            contains_equation_equal_sign: false,
            is_in_integral: false,
            current_function: None,
            current_function_parameters: None,
            other_radix: None,
        };

        parse(&mut context, crate::prelude::INIT).unwrap();

        context
    }

    pub fn set_angle_unit(mut self, unit: &str) -> Self {
        self.angle_unit = unit.into();

        self
    }

    /// Set the timeout in milliseconds.
    /// The calculation will stop after this amount of time has passed.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn set_timeout(mut self, timeout: Option<u32>) -> Self {
        self.timeout = timeout;

        self
    }

    #[wasm_bindgen(js_name = evaluate)]
    #[cfg(not(feature = "rug"))]
    pub fn js_eval(&mut self, input: &str) -> Result<Option<CalculationResult>, JsValue> {
        let result = eval(self, input);

        match result {
            Ok(Some(value)) => Ok(Some(value)),
            Ok(None) => Ok(None),
            Err(err) => Err(err.to_string().into()),
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

/// Error that occured during parsing or evaluation.
#[derive(Debug, Clone, PartialEq)]
pub enum CalcError {
    Expected(String),
    ExpectedDx,
    ExpectedIf,
    IncorrectAmountOfArguments(usize, String, usize),
    InvalidNumberLiteral(String),
    InvalidOperator,
    InvalidUnit,
    TimedOut,
    VariableReferencesItself,
    PiecewiseConditionsAreFalse,
    UnexpectedToken(TokenKind, TokenKind),
    UndefinedFn(String),
    UndefinedVar(String),
    UnableToInvert(String),
    UnableToSolveEquation,
    UnableToOverrideConstant(String),
    UnableToParseExpression,
    UnrecognizedBase,
    Unknown,
}

impl ToString for CalcError {
    fn to_string(&self) -> String {
        match self {
            CalcError::Expected(description) => format!("Expected: {}", description),
            CalcError::ExpectedDx => format!("Expected eg. dx, to specify for which variable the operation is being done to. Example with integration: ∫(0, 1, x dx) or ∫(0, 1, x, dx). You may need to put parenthesis around the expression before dx/dy/du/etc."),
            CalcError::ExpectedIf => format!("Expected 'if', with a condition after it."),
            CalcError::IncorrectAmountOfArguments(expected, func, got) => format!(
                "Expected {} arguments for function {}, but got {}.",
                expected, func, got
            ),
            CalcError::InvalidNumberLiteral(x) => format!("Invalid number literal: '{}'.", x),
            CalcError::InvalidOperator => format!("Invalid operator."),
            CalcError::InvalidUnit => format!("Invalid unit."),
            CalcError::TimedOut => format!("Operation took too long."),
            CalcError::VariableReferencesItself => format!("Variable references itself."),
            CalcError::PiecewiseConditionsAreFalse => format!("All the conditions in the piecewise are false."),
            CalcError::UnexpectedToken(got, expected) => {
                format!("Unexpected token: '{:?}', expected '{:?}'.", got, expected)
            }
            CalcError::UnableToInvert(msg) => format!("Unable to invert: {}", msg),
            CalcError::UndefinedFn(name) => format!("Undefined function: '{}'.", name),
            CalcError::UndefinedVar(name) => format!("Undefined variable: '{}'.", name),
            CalcError::UnableToParseExpression => format!("Unable to parse expression."),
            CalcError::UnableToSolveEquation => format!("Unable to solve equation."),
            CalcError::UnableToOverrideConstant(name) => format!("Unable to override constant: '{}'.", name),
            CalcError::UnrecognizedBase => format!("Unrecognized base."),
            CalcError::Unknown => format!("Unknown error."),
        }
    }
}

/// Evaluate expressions/declarations and return the answer.
///
/// `None` will be returned if the last statement is a declaration.
pub fn eval(
    context: &mut Context,
    input: &str,
    #[cfg(feature = "rug")] precision: u32,
) -> Result<Option<CalculationResult>, CalcError> {
    // Variable and function declaration parsers will set this to false
    // if the equal sign is for one of those instead.
    // It also should not contain an iverson bracket, since equal signs in there
    // mean something else. This is not super reliable, and should probably be improved in the future.
    context.contains_equation_equal_sign =
        input.contains("=") && !input.contains("[") && !input.contains("{");
    let statements = parse(context, input)?;

    let mut interpreter = interpreter::Context::new(
        &mut context.symbol_table,
        &context.angle_unit,
        #[cfg(feature = "rug")]
        precision,
        if let Some(timeout) = context.timeout {
            Some(timeout as u128)
        } else {
            None
        },
    );
    let result = interpreter.interpret(statements);
    if let Ok(Some(mut num)) = result {
        num.set_radix(context.other_radix.unwrap_or(10));
        Ok(Some(num))
    } else {
        result
    }
}

/// Parse expressions/declarations and return a syntax tree.
///
/// `None` will be returned if the last statement is a declaration.
pub fn parse(context: &mut Context, input: &str) -> Result<Vec<Stmt>, CalcError> {
    let mut lexer = Lexer::new(input);
    context.tokens = lexer.lex();
    context.pos = 0;
    context.parsing_unit_decl = false;
    context.unit_decl_base_unit = None;
    context.other_radix = lexer.get_other_radix();

    let mut statements: Vec<Stmt> = Vec::new();
    while !is_at_end(context) {
        statements.push(parse_stmt(context)?);

        if match_token(context, TokenKind::Semicolon) {
            advance(context);
        }

        skip_newlines(context);
    }

    Ok(statements)
}

fn parse_stmt(context: &mut Context) -> Result<Stmt, CalcError> {
    if match_token(context, TokenKind::Identifier) {
        return Ok(match peek_next(context).kind {
            TokenKind::Equals => parse_var_decl_stmt(context)?,
            TokenKind::OpenParenthesis => parse_identifier_stmt(context)?,
            _ => Stmt::Expr(Box::new(parse_expr(context)?)),
        });
    } else if match_token(context, TokenKind::UnitKeyword) {
        return parse_unit_decl_stmt(context);
    }

    Ok(Stmt::Expr(Box::new(parse_expr(context)?)))
}

fn parse_identifier_stmt(context: &mut Context) -> Result<Stmt, CalcError> {
    let began_at = context.pos;
    context.parsing_identifier_stmt = true;
    let primary = parse_primary(context)?; // Since function declarations and function calls look the same at first, simply parse a "function call", and re-use the data.
    context.parsing_identifier_stmt = false;

    // If `primary` is followed by an equal sign
    // treat it as a function declaration
    if let TokenKind::Equals = peek(context).kind {
        // Use the "function call" expression that was parsed, and put its values into a function declaration statement instead.
        if let Expr::FnCall(identifier, parameters) = primary {
            if !prelude::is_prelude_func(&identifier.full_name) {
                advance(context);

                // All the "arguments" are expected to be parsed as variables,
                // since parameter definitions look the same as variable references.
                // Extract these.
                let mut parameter_identifiers = Vec::new();
                for parameter in parameters {
                    if let Expr::Var(parameter_identifier) = parameter {
                        parameter_identifiers.push(format!(
                            "{}-{}",
                            &identifier.pure_name, &parameter_identifier.full_name
                        ));
                    }
                }

                context.current_function = Some(identifier.pure_name.clone());
                context.current_function_parameters = Some(parameter_identifiers.clone());
                context.contains_equation_equal_sign = false;
                context.equation_variable = None;

                // Piecewise
                let expr = if match_token(context, TokenKind::OpenBrace) {
                    parse_piecewise(context)?
                } else {
                    parse_expr(context)?
                };
                let fn_decl = Stmt::FnDecl(identifier, parameter_identifiers, Box::new(expr));
                context.current_function = None;
                context.current_function_parameters = None;

                // Insert the function declaration into the symbol table during parsing
                // so that the parser can find out if particular functions exist.
                context.symbol_table.insert(fn_decl.clone());

                return Ok(fn_decl);
            }
        }
    }

    // It is a function call or eg. x(x + 3), not a function declaration.
    // Redo the parsing for this specific part.
    context.pos = began_at;
    Ok(Stmt::Expr(Box::new(parse_expr(context)?)))
}

fn parse_piecewise(context: &mut Context) -> Result<Expr, CalcError> {
    advance(context);
    skip_newlines(context);

    let mut pieces = Vec::new();
    let mut reached_otherwise = false;
    // Parse `expr if condition`
    // do-while loop
    while {
        let left_expr = parse_expr(context)?;
        if match_token(context, TokenKind::IfKeyword) {
            advance(context);
            pieces.push(crate::ast::ConditionalPiece {
                expr: left_expr,
                condition: parse_expr(context)?,
            });
        } else if match_token(context, TokenKind::OtherwiseKeyword) {
            advance(context);
            // Yeah, a bit hacky, but there's no `true` keyword...
            let true_expr = Expr::Binary(
                Box::new(Expr::Literal(1f64)),
                TokenKind::Equals,
                Box::new(Expr::Literal(1f64)),
            );
            pieces.push(crate::ast::ConditionalPiece {
                expr: left_expr,
                condition: true_expr,
            });

            reached_otherwise = true;
        } else {
            return Err(CalcError::ExpectedIf);
        }

        if match_token(context, TokenKind::Semicolon) {
            advance(context);
        }
        skip_newlines(context);

        (previous(context).kind == TokenKind::Semicolon
            || previous(context).kind == TokenKind::Newline)
            && !reached_otherwise
    } {}

    advance(context);

    Ok(Expr::Piecewise(pieces))
}

fn parse_var_decl_stmt(context: &mut Context) -> Result<Stmt, CalcError> {
    let identifier = advance(context).clone();
    advance(context); // Equal sign
    context.contains_equation_equal_sign = false;
    context.equation_variable = None;
    let expr = parse_expr(context)?;
    if inverter::contains_var(&context.symbol_table, &expr, &identifier.value) {
        return Err(CalcError::VariableReferencesItself);
    }

    if prelude::is_constant(&identifier.value) {
        return Err(CalcError::UnableToOverrideConstant(identifier.value.into()));
    }

    Ok(Stmt::VarDecl(
        Identifier::from_full_name(&identifier.value),
        Box::new(expr),
    ))
}

fn parse_unit_decl_stmt(context: &mut Context) -> Result<Stmt, CalcError> {
    advance(context); // Unit keyword
    let identifier = advance(context).clone();
    consume(context, TokenKind::Equals)?;

    // Parse the mut definition
    context.unit_decl_base_unit = None;
    context.parsing_unit_decl = true;
    let def = parse_expr(context)?;
    context.parsing_unit_decl = false;

    let base_unit = if let Some(base_unit) = &context.unit_decl_base_unit {
        base_unit.clone()
    } else {
        return Err(CalcError::InvalidUnit);
    };

    // Automatically create a second unit decl with the expression inverted.
    // This will turn eg. unit a = 3b, into unit b = a/3
    // This is so that you only have to define `a`, and it will figure out the formula for `b` since it is used in the formula for `a`.
    let stmt_inv = Stmt::UnitDecl(
        base_unit.clone(),
        identifier.value.clone(),
        Box::new(def.invert(&mut context.symbol_table, DECL_UNIT)?),
    );
    let stmt = Stmt::UnitDecl(identifier.value, base_unit, Box::new(def));

    context.symbol_table.insert(stmt.clone());
    context.symbol_table.insert(stmt_inv);

    Ok(stmt)
}

fn parse_expr(context: &mut Context) -> Result<Expr, CalcError> {
    Ok(parse_equality(context)?)
}

fn parse_equality(context: &mut Context) -> Result<Expr, CalcError> {
    let mut left = parse_to(context)?;

    // Equation
    if match_token(context, TokenKind::Equals) && context.contains_equation_equal_sign {
        advance(context);
        let right = parse_to(context)?;
        let var_name = if let Some(var_name) = &context.equation_variable {
            var_name
        } else {
            return Err(CalcError::UnableToSolveEquation);
        };

        let inverted = if inverter::contains_var(&mut context.symbol_table, &left, var_name) {
            left.invert_to_target(&mut context.symbol_table, right, var_name)?
        } else {
            right.invert_to_target(&mut context.symbol_table, left, var_name)?
        };

        // If the inverted expression still contains the variable,
        // the equation solving failed.
        if inverter::contains_var(&mut context.symbol_table, &inverted, var_name) {
            return Err(CalcError::UnableToSolveEquation);
        }

        context.symbol_table.insert(Stmt::VarDecl(
            Identifier::from_full_name(var_name),
            Box::new(inverted.clone()),
        ));

        return Ok(inverted);
    }

    // Equality check
    while match_token(context, TokenKind::Equals)
        || match_token(context, TokenKind::NotEquals)
        || match_token(context, TokenKind::GreaterThan)
        || match_token(context, TokenKind::LessThan)
        || match_token(context, TokenKind::GreaterOrEquals)
        || match_token(context, TokenKind::LessOrEquals)
    {
        let op = peek(context).kind;
        advance(context);
        let right = parse_to(context)?;

        left = Expr::Binary(Box::new(left), op, Box::new(right));
    }

    Ok(left)
}

fn parse_to(context: &mut Context) -> Result<Expr, CalcError> {
    let left = parse_sum(context)?;

    if match_token(context, TokenKind::ToKeyword) {
        advance(context);
        let right = Expr::Var(Identifier::from_full_name(&advance(context).value)); // Parse this as a variable for now.

        return Ok(Expr::Binary(
            Box::new(left),
            TokenKind::ToKeyword,
            Box::new(right),
        ));
    }

    Ok(left)
}

fn parse_sum(context: &mut Context) -> Result<Expr, CalcError> {
    let mut left = parse_factor(context)?;

    while match_token(context, TokenKind::Plus) || match_token(context, TokenKind::Minus) {
        let op = peek(context).kind;
        advance(context);
        let right = parse_factor(context)?;

        left = Expr::Binary(Box::new(left), op, Box::new(right));
    }

    Ok(left)
}

fn parse_factor(context: &mut Context) -> Result<Expr, CalcError> {
    let mut left = parse_unit(context)?;

    if let Expr::Unary(TokenKind::Percent, percent_left) = left.clone() {
        let try_parse = parse_factor(context);
        if !try_parse.is_err() {
            left = Expr::Binary(
                percent_left,
                TokenKind::Percent,
                Box::new(try_parse.unwrap()),
            );
        }
    }

    while match_token(context, TokenKind::Star)
        || match_token(context, TokenKind::Slash)
        || match_token(context, TokenKind::Percent)
        || match_token(context, TokenKind::Identifier)
        || match_token(context, TokenKind::Literal)
        || match_token(context, TokenKind::OpenParenthesis)
        || match_token(context, TokenKind::OpenCeil)
        || match_token(context, TokenKind::OpenFloor)
        || match_token(context, TokenKind::OpenBracket)
    {
        // If the token is an identifier, literal, or open parenthesis,
        // assume it's multiplication. Eg. 3y or (3x + 2)(2 + 3)
        let op = match peek(context).kind {
            TokenKind::Identifier
            | TokenKind::Literal
            | TokenKind::OpenParenthesis
            | TokenKind::OpenCeil
            | TokenKind::OpenFloor
            | TokenKind::OpenBracket => TokenKind::Star,
            _ => advance(context).kind,
        };

        let right = parse_unit(context)?;
        left = Expr::Binary(Box::new(left), op, Box::new(right));
    }

    Ok(left)
}

fn parse_unit(context: &mut Context) -> Result<Expr, CalcError> {
    let expr = parse_unary(context)?;
    let peek = &peek(&context).value;

    if match_token(context, TokenKind::Identifier) && context.symbol_table.contains_unit(&peek) {
        return Ok(Expr::Unit(
            advance(context).value.to_string(),
            Box::new(expr),
        ));
    }

    Ok(expr)
}

fn parse_unary(context: &mut Context) -> Result<Expr, CalcError> {
    if match_token(context, TokenKind::Minus) {
        let op = advance(context).kind;
        let expr = Box::new(parse_unary(context)?);
        return Ok(Expr::Unary(op, expr));
    }

    let expr = parse_exponent(context)?;
    if match_token(context, TokenKind::Percent) {
        Ok(Expr::Unary(advance(context).kind, Box::new(expr)))
    } else {
        Ok(expr)
    }
}

fn parse_exponent(context: &mut Context) -> Result<Expr, CalcError> {
    let left = parse_factorial(context)?;

    if match_token(context, TokenKind::Power) {
        let op = advance(context).kind;
        let right = Box::new(parse_exponent(context)?);
        return Ok(Expr::Binary(Box::new(left), op, right));
    }

    Ok(left)
}

fn parse_factorial(context: &mut Context) -> Result<Expr, CalcError> {
    let expr = parse_primary(context)?;

    Ok(if match_token(context, TokenKind::Exclamation) {
        advance(context);
        Expr::Unary(TokenKind::Exclamation, Box::new(expr))
    } else {
        expr
    })
}

fn parse_primary(context: &mut Context) -> Result<Expr, CalcError> {
    let expr = match peek(context).kind {
        TokenKind::OpenParenthesis => parse_group(context)?,
        TokenKind::Pipe | TokenKind::OpenCeil | TokenKind::OpenFloor | TokenKind::OpenBracket => {
            parse_group_fn(context)?
        }
        TokenKind::Identifier => parse_identifier(context)?,
        TokenKind::Literal => Expr::Literal(string_to_num(&advance(context).value)?),
        _ => return Err(CalcError::UnableToParseExpression),
    };

    Ok(expr)
}

fn parse_group(context: &mut Context) -> Result<Expr, CalcError> {
    advance(context);
    let group_expr = Expr::Group(Box::new(parse_expr(context)?));
    consume(context, TokenKind::ClosedParenthesis)?;

    Ok(group_expr)
}

fn parse_group_fn(context: &mut Context) -> Result<Expr, CalcError> {
    let name = match &advance(context).kind {
        TokenKind::Pipe => "abs",
        TokenKind::OpenCeil => "ceil",
        TokenKind::OpenFloor => "floor",
        TokenKind::OpenBracket => "iverson",
        _ => unreachable!(),
    };

    let expr = parse_expr(context)?;

    if peek(context).kind == TokenKind::EOF {
        return Err(CalcError::Expected(String::from(
            "Closing group symbol, eg. ⌋",
        )));
    }

    advance(context);

    Ok(Expr::FnCall(Identifier::from_full_name(name), vec![expr]))
}

fn parse_identifier(context: &mut Context) -> Result<Expr, CalcError> {
    let identifier = Identifier::from_full_name(&advance(context).value);

    let mut log_base = None;
    if identifier.full_name.starts_with("log") {
        if let Some(c) = identifier.full_name.chars().nth(3) {
            if crate::text_utils::is_subscript(&c) {
                log_base = Some(Expr::Literal(get_base(&identifier.full_name)? as f64));
            }
        }
    }

    let exists_as_fn = context.symbol_table.contains_fn(&identifier.pure_name)
        || context.current_function.as_ref() == Some(&identifier.pure_name)
        || log_base.is_some();

    // Eg. sqrt64
    if exists_as_fn
        && (match_token(context, TokenKind::Literal) || match_token(context, TokenKind::Identifier))
    {
        // If there is a function with this name, parse it as a function, with the next token as the argument.
        let parameter = if identifier.full_name == "√" {
            parse_exponent(context)?
        } else {
            parse_factor(context)?
        };

        if let Some(log_base) = log_base {
            return Ok(Expr::FnCall(
                Identifier::from_full_name("log"),
                vec![parameter, log_base],
            ));
        }

        return Ok(Expr::FnCall(identifier, vec![parameter]));
    }

    let parse_as_var_instead = match_token(context, TokenKind::OpenParenthesis)
        && !context.parsing_identifier_stmt
        && !exists_as_fn;

    // Eg. sqrt(64)
    // If the function doesn't exist, parse it as a variable and multiplication instead.
    // Although, if the parse_identifier_stmt function called this function,
    // parse it as a function anyway, since it might be creating one.
    if !parse_as_var_instead && match_token(context, TokenKind::OpenParenthesis) {
        advance(context);

        let is_integral = identifier.full_name == "integrate"
            || identifier.full_name == "integral"
            || identifier.full_name == "∫";
        if is_integral {
            context.is_in_integral = true;
        }

        let mut parameters = Vec::new();
        parameters.push(parse_expr(context)?);

        while match_token(context, TokenKind::Comma) {
            advance(context);
            parameters.push(parse_expr(context)?);
        }

        consume(context, TokenKind::ClosedParenthesis)?;

        if is_integral {
            context.is_in_integral = false;
        }

        if let Some(log_base) = log_base {
            parameters.push(log_base);
            return Ok(Expr::FnCall(Identifier::from_full_name("log"), parameters));
        }

        return Ok(Expr::FnCall(identifier, parameters));
    }

    // Eg. x
    if parse_as_var_instead || context.symbol_table.contains_var(&identifier.pure_name) {
        Ok(build_var(context, &identifier.full_name))
    } else if context.parsing_unit_decl {
        context.unit_decl_base_unit = Some(identifier.full_name);
        Ok(Expr::Var(Identifier::from_full_name(DECL_UNIT)))
    } else {
        if let Some(equation_var) = &context.equation_variable {
            if &identifier.full_name == equation_var {
                return Ok(build_var(context, &identifier.full_name));
            }
        }
        if context.contains_equation_equal_sign {
            context.equation_variable = Some(identifier.full_name.clone());
            return Ok(build_var(context, &identifier.full_name));
        }

        if identifier.pure_name.len() == 1 {
            return Ok(build_var(context, &identifier.full_name));
        }

        // Eg. dx inside an integral, should be parsed as *one* identifier
        // Reverse the identifier and take two. This gets the last two characters (in reversed order).
        // Now reverse this to finally get the last two characters in correct order.
        // It's a bit weird, but it should work for more than ASCII.
        let mut identifier_without_dx: Vec<char> = identifier.full_name.chars().collect();
        let mut last_two_chars = String::new();
        let last_char = identifier_without_dx.pop().unwrap_or_default();
        let first_char = identifier_without_dx.pop().unwrap_or_default();
        last_two_chars.push(first_char);
        last_two_chars.push(last_char);

        if context.is_in_integral && last_two_chars.starts_with("d") {
            // If the token contains more than just "dx",
            // save the dx/dy/du/etc. in a variable, that can be
            // used further down when splitting the identifier into multiple variables.
            if identifier.full_name.len() > 2 {
                // This variable will be used further down in order to separate dx from the rest.
                let pos = context.pos - 1;
                context.pos = pos;
                context.tokens[pos] = Token {
                    kind: TokenKind::Identifier,
                    value: identifier_without_dx.iter().collect(),
                    span: (0, 0),
                };

                let left_expr = parse_exponent(context)?;

                // Revert back to how it was before.
                context.tokens[pos] = Token {
                    kind: TokenKind::Identifier,
                    value: identifier.full_name.to_string(),
                    span: (0, 0),
                };

                return Ok(Expr::Binary(
                    Box::new(left_expr),
                    TokenKind::Star,
                    Box::new(Expr::Var(Identifier::from_full_name(&last_two_chars))),
                ));
            } else {
                return Ok(Expr::Var(Identifier::from_full_name(&last_two_chars)));
            }
        }

        if identifier.pure_name.len() > 1 {
            split_into_variables(context, &identifier)
        } else {
            Err(CalcError::UndefinedVar(identifier.full_name))
        }
    }
}

fn split_into_variables(context: &mut Context, identifier: &Identifier) -> Result<Expr, CalcError> {
    let mut chars: Vec<char> = identifier.pure_name.chars().collect();
    let mut left = Expr::Var(Identifier::from_full_name(&chars[0].to_string()));

    // Temporarily remove the last character and check if a function
    // without that character exists. If it does,
    // create a function call expression, where that last character
    // is the argument.
    let last_char = chars.pop().unwrap_or_default();
    let identifier_without_last: String = chars.iter().collect();
    if context.symbol_table.contains_fn(&identifier_without_last) {
        return Ok(Expr::FnCall(
            Identifier::from_full_name(&identifier_without_last),
            vec![Expr::Var(Identifier::from_full_name(
                &last_char.to_string(),
            ))],
        ));
    } else {
        // Otherwise, re-add the character.
        chars.push(last_char);
    }

    // Turn each individual character into its own variable reference.
    // This parses eg `xy` as `x*y` instead of *one* variable.
    let mut right_chars = chars.iter().skip(1).peekable();
    while let Some(c) = right_chars.next() {
        // If last iteration
        let right = if right_chars.peek().is_none() {
            // Temporarily change the token content, so that
            // the parse_exponent step will parse it as its
            // new name. It will later be switched back,
            // since the parser sometimes rewinds a bit,
            // and may get confused by a sudden change.
            let pos = context.pos - 1;
            context.pos = pos;
            context.tokens[pos] = Token {
                kind: TokenKind::Identifier,
                value: c.to_string(),
                span: (0, 0),
            };

            let last_var = parse_exponent(context)?;

            // Revert back to how it was before.
            context.tokens[pos] = Token {
                kind: TokenKind::Identifier,
                value: identifier.full_name.to_string(),
                span: (0, 0),
            };

            last_var
        } else {
            build_var(context, &c.to_string())
        };

        left = Expr::Binary(Box::new(left), TokenKind::Star, Box::new(right));
    }

    Ok(left)
}

fn build_var(context: &Context, name: &str) -> Expr {
    if let (Some(function_name), Some(params)) = (
        context.current_function.as_ref(),
        context.current_function_parameters.as_ref(),
    ) {
        let identifier = Identifier::parameter_from_name(name, &function_name);
        if params.contains(&identifier.full_name) {
            return Expr::Var(identifier);
        }
    }
    return Expr::Var(Identifier::from_full_name(name));
}

fn peek(context: &Context) -> &Token {
    if context.pos >= context.tokens.len() {
        &context.tokens.last().unwrap() // EOF
    } else {
        &context.tokens[context.pos]
    }
}

fn peek_next(context: &Context) -> &Token {
    &context.tokens[context.pos + 1]
}

fn previous(context: &Context) -> &Token {
    &context.tokens[context.pos - 1]
}

fn match_token(context: &Context, kind: TokenKind) -> bool {
    if is_at_end(context) {
        return false;
    }

    peek(context).kind == kind
}

fn advance(context: &mut Context) -> &Token {
    context.pos += 1;
    previous(context)
}

fn consume(context: &mut Context, kind: TokenKind) -> Result<&Token, CalcError> {
    if match_token(context, kind) {
        return Ok(advance(context));
    }

    Err(CalcError::UnexpectedToken(peek(context).kind, kind))
}

fn is_at_end(context: &Context) -> bool {
    context.pos >= context.tokens.len() || peek(context).kind == TokenKind::EOF
}

fn skip_newlines(context: &mut Context) {
    while match_token(context, TokenKind::Newline) {
        advance(context);
    }
}

fn string_to_num(value: &str) -> Result<f64, CalcError> {
    let base = get_base(value)?;
    if let Some(result) = crate::radix::parse_float_radix(&value.replace(" ", ""), base) {
        Ok(result)
    } else {
        Err(CalcError::InvalidNumberLiteral(value.into()))
    }
}

fn get_base(value: &str) -> Result<u8, CalcError> {
    let underscore_pos = if let Some(i) = value.find('_') {
        i
    } else {
        return Ok(10);
    };

    let subscript = value.chars().skip(underscore_pos + 1usize);
    if let Some(base) = crate::text_utils::parse_subscript(subscript) {
        Ok(base)
    } else {
        Err(CalcError::UnrecognizedBase)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Identifier;
    use crate::lexer::{Token, TokenKind::*};
    use crate::test_helpers::*;
    use wasm_bindgen_test::*;

    fn parse_with_context(context: &mut Context, tokens: Vec<Token>) -> Result<Stmt, CalcError> {
        context.tokens = tokens;
        context.pos = 0;

        parse_stmt(context)
    }

    fn parse(tokens: Vec<Token>) -> Result<Stmt, CalcError> {
        let mut context = Context::new();
        context.tokens = tokens;
        context.pos = 0;

        parse_stmt(&mut context)
    }

    #[test]
    #[wasm_bindgen_test]
    fn test_var() {
        // x
        let tokens = vec![token(Identifier, "x"), token(EOF, "")];

        assert_eq!(parse(tokens).unwrap(), Stmt::Expr(var("x")));
    }

    #[test]
    #[wasm_bindgen_test]
    fn test_var_multiplication() {
        let mut context = Context::new();
        context.symbol_table.insert(Stmt::VarDecl(
            Identifier::from_full_name("x"),
            literal(1f64),
        ));
        context.symbol_table.insert(Stmt::VarDecl(
            Identifier::from_full_name("y"),
            literal(2f64),
        ));

        // xy²
        let tokens = vec![
            token(Identifier, "xy"),
            token(Power, ""),
            token(Literal, "2"),
            token(EOF, ""),
        ];

        assert_eq!(
            parse(tokens).unwrap(),
            Stmt::Expr(binary(
                var("x"),
                Star,
                binary(var("y"), Power, literal(2f64))
            ))
        );
    }

    #[test]
    #[wasm_bindgen_test]
    fn test_binary() {
        // 1+2*(3-4/5)
        let tokens = vec![
            token(Literal, "1"),
            token(Plus, ""),
            token(Literal, "2"),
            token(Star, ""),
            token(OpenParenthesis, ""),
            token(Literal, "3"),
            token(Minus, ""),
            token(Literal, "4"),
            token(Slash, ""),
            token(Literal, "5"),
            token(ClosedParenthesis, ""),
            token(EOF, ""),
        ];

        assert_eq!(
            parse(tokens).unwrap(),
            Stmt::Expr(binary(
                literal(1f64),
                Plus,
                binary(
                    literal(2f64),
                    Star,
                    group(binary(
                        literal(3f64),
                        Minus,
                        binary(literal(4f64), Slash, literal(5f64))
                    ))
                )
            ))
        );
    }

    #[test]
    #[wasm_bindgen_test]
    fn test_pow() {
        let tokens = vec![
            token(Literal, "1"),
            token(Star, ""),
            token(Literal, "2"),
            token(Power, ""),
            token(Literal, "3"),
            token(Power, ""),
            token(Literal, "4"),
            token(Plus, ""),
            token(Literal, "5"),
            token(EOF, ""),
        ];

        assert_eq!(
            parse(tokens).unwrap(),
            Stmt::Expr(binary(
                binary(
                    literal(1f64),
                    Star,
                    binary(
                        literal(2f64),
                        Power,
                        binary(literal(3f64), Power, literal(4f64)),
                    ),
                ),
                Plus,
                literal(5f64)
            )),
        );
    }

    #[test]
    #[wasm_bindgen_test]
    fn test_percent() {
        let tokens = vec![
            token(Literal, "1"),
            token(Percent, ""),
            token(Literal, "1"),
            token(Plus, ""),
            token(Literal, "5"),
            token(Percent, ""),
            token(EOF, ""),
        ];

        assert_eq!(
            parse(tokens).unwrap(),
            Stmt::Expr(binary(
                binary(literal(1f64), Percent, literal(1f64)),
                Plus,
                unary(Percent, literal(5f64))
            ))
        );
    }

    #[test]
    #[wasm_bindgen_test]
    fn test_unit() {
        let tokens = vec![token(Literal, "1"), token(Identifier, "a")];

        let mut context = Context::new();
        context
            .symbol_table
            .insert(unit_decl("a", "b", var(super::DECL_UNIT)));

        assert_eq!(
            parse_with_context(&mut context, tokens).unwrap(),
            Stmt::Expr(unit("a", literal(1f64)))
        );
    }

    #[test]
    #[wasm_bindgen_test]
    fn test_var_decl() {
        let tokens = vec![
            token(Identifier, "x"),
            token(Equals, ""),
            token(Literal, "1"),
            token(Plus, ""),
            token(Literal, "2"),
            token(EOF, ""),
        ];

        assert_eq!(
            parse(tokens).unwrap(),
            Stmt::VarDecl(
                Identifier::from_full_name("x"),
                binary(literal(1f64), Plus, literal(2f64))
            )
        );
    }

    #[test]
    #[wasm_bindgen_test]
    fn test_fn_decl() {
        let tokens = vec![
            token(Identifier, "f"),
            token(OpenParenthesis, ""),
            token(Identifier, "x"),
            token(ClosedParenthesis, ""),
            token(Equals, ""),
            token(Literal, "1"),
            token(Plus, ""),
            token(Identifier, "x"),
            token(EOF, ""),
        ];

        assert_eq!(
            parse(tokens).unwrap(),
            Stmt::FnDecl(
                Identifier::from_full_name("f"),
                vec![String::from("f-x")],
                binary(literal(1f64), Plus, param_var("f", "x"))
            )
        );
    }

    #[test]
    #[wasm_bindgen_test]
    fn test_fn_call() {
        let tokens = vec![
            token(Identifier, "f"),
            token(OpenParenthesis, ""),
            token(Literal, "1"),
            token(Plus, ""),
            token(Literal, "2"),
            token(ClosedParenthesis, ""),
            token(Plus, ""),
            token(Literal, "3"),
            token(EOF, ""),
        ];

        let mut context = Context::new();

        // Add the function to the symbol table first, in order to prevent errors.
        context.symbol_table.set(Stmt::FnDecl(
            Identifier::from_full_name("f"),
            vec![String::from("x")],
            literal(1f64),
        ));

        assert_eq!(
            parse_with_context(&mut context, tokens).unwrap(),
            Stmt::Expr(binary(
                Box::new(Expr::FnCall(
                    Identifier::from_full_name("f"),
                    vec![*binary(literal(1f64), Plus, literal(2f64))]
                )),
                Plus,
                literal(3f64)
            ))
        );
    }
}
