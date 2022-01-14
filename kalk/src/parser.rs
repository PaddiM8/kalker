use std::cell::Cell;

use crate::analysis;
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
    symbol_table: Cell<SymbolTable>,
    angle_unit: String,
    timeout: Option<u32>,
    /// This is true whenever the parser is currently parsing a unit declaration.
    /// It is necessary to keep track of this in order to know when to find (figure out) units that haven't been defined yet.
    /// Unit names are instead treated as variables.
    parsing_unit_decl: bool,
    /// When a unit declaration is being parsed, this value will be set
    /// whenever a unit in the expression is found. Eg. unit a = 3b, it will be set to Some("b")
    unit_decl_base_unit: Option<String>,
    equation_variable: Option<String>,
    contains_equation_equal_sign: bool,
    other_radix: Option<u8>,
}

#[wasm_bindgen]
impl Context {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        let mut context = Self {
            tokens: Vec::new(),
            pos: 0,
            symbol_table: Cell::from(SymbolTable::new()),
            angle_unit: DEFAULT_ANGLE_UNIT.into(),
            timeout: None,
            parsing_unit_decl: false,
            unit_decl_base_unit: None,
            equation_variable: None,
            contains_equation_equal_sign: false,
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
    CanOnlyIndexVectors,
    Expected(String),
    ExpectedDx,
    ExpectedIf,
    IncorrectAmountOfArguments(usize, String, usize),
    IncorrectAmountOfIndexes(usize, usize),
    ItemOfIndexDoesNotExist(Vec<usize>),
    InconsistentColumnWidths,
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
            CalcError::CanOnlyIndexVectors => format!("Indexing (getting an item with a specific index) is only possible on vectors."),
            CalcError::Expected(description) => format!("Expected: {}", description),
            CalcError::ExpectedDx => format!("Expected eg. dx, to specify for which variable the operation is being done to. Example with integration: ∫(0, 1, x dx) or ∫(0, 1, x, dx). You may need to put parenthesis around the expression before dx/dy/du/etc."),
            CalcError::ExpectedIf => format!("Expected 'if', with a condition after it."),
            CalcError::IncorrectAmountOfArguments(expected, func, got) => format!(
                "Expected {} arguments for function {}, but got {}.",
                expected, func, got
            ),
            CalcError::IncorrectAmountOfIndexes(expected,  got) => format!(
                "Expected {} indexes but got {}.",
                expected, got
            ),
            CalcError::ItemOfIndexDoesNotExist(indexes) => format!("Item of index ⟦{}⟧ does not exist.", indexes.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", ")),
            CalcError::InconsistentColumnWidths => format!("Inconsistent column widths. Matrix columns must be the same size."),
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

    let mut symbol_table = context.symbol_table.get_mut();
    let mut interpreter = interpreter::Context::new(
        &mut symbol_table,
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
        let parsed = parse_stmt(context)?;
        let mut symbol_table = context.symbol_table.get_mut();
        let analysed = analysis::analyse_stmt(&mut symbol_table, parsed)?;
        statements.push(analysed);

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
            _ => Stmt::Expr(Box::new(parse_expr(context)?)),
        });
    } else if match_token(context, TokenKind::UnitKeyword) {
        return parse_unit_decl_stmt(context);
    }

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
    if inverter::contains_var(
        &mut context.symbol_table.get_mut(),
        &expr,
        &identifier.value,
    ) {
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
        Box::new(def.invert(&mut context.symbol_table.get_mut(), DECL_UNIT)?),
    );
    let stmt = Stmt::UnitDecl(identifier.value, base_unit, Box::new(def));

    context.symbol_table.get_mut().insert(stmt.clone());
    context.symbol_table.get_mut().insert(stmt_inv);

    Ok(stmt)
}

fn parse_expr(context: &mut Context) -> Result<Expr, CalcError> {
    Ok(parse_equality(context)?)
}

fn parse_equality(context: &mut Context) -> Result<Expr, CalcError> {
    let mut left = parse_to(context)?;

    // Equation
    /*if match_token(context, TokenKind::Equals) && context.contains_equation_equal_sign {
        advance(context);
        let right = parse_to(context)?;
        let var_name = if let Some(var_name) = &context.equation_variable {
            var_name
        } else {
            return Err(CalcError::UnableToSolveEquation);
        };

        let inverted =
            if inverter::contains_var(&mut context.symbol_table.get_mut(), &left, var_name) {
                left.invert_to_target(&mut context.symbol_table.get_mut(), right, var_name)?
            } else {
                right.invert_to_target(&mut context.symbol_table.get_mut(), left, var_name)?
            };

        // If the inverted expression still contains the variable,
        // the equation solving failed.
        if inverter::contains_var(&mut context.symbol_table.get_mut(), &inverted, var_name) {
            return Err(CalcError::UnableToSolveEquation);
        }

        context.symbol_table.get_mut().insert(Stmt::VarDecl(
            Identifier::from_full_name(var_name),
            Box::new(inverted.clone()),
        ));

        return Ok(inverted);
    }*/

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
        let right = if op == TokenKind::Equals && match_token(context, TokenKind::OpenBrace) {
            parse_piecewise(context)?
        } else {
            parse_to(context)?
        };

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

    if match_token(context, TokenKind::Identifier) {
        let peek = &peek(context).value.clone();
        if context.symbol_table.get_mut().contains_unit(peek) {
            return Ok(Expr::Unit(
                advance(context).value.to_string(),
                Box::new(expr),
            ));
        }
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
    let left = parse_indexer(context)?;

    if match_token(context, TokenKind::Power) {
        let op = advance(context).kind;
        let right = Box::new(parse_exponent(context)?);
        return Ok(Expr::Binary(Box::new(left), op, right));
    }

    Ok(left)
}

fn parse_indexer(context: &mut Context) -> Result<Expr, CalcError> {
    let left = parse_factorial(context)?;

    if match_token(context, TokenKind::OpenDoubleBracket) {
        advance(context);
        let mut indexes = vec![parse_expr(context)?];
        while match_token(context, TokenKind::Comma) {
            advance(context);
            indexes.push(parse_expr(context)?);
        }

        consume(context, TokenKind::ClosedDoubleBracket)?;

        return Ok(Expr::Indexer(Box::new(left), indexes));
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
        TokenKind::OpenParenthesis | TokenKind::OpenBracket => parse_vector(context)?,
        TokenKind::Pipe | TokenKind::OpenCeil | TokenKind::OpenFloor => parse_group_fn(context)?,
        TokenKind::Identifier => parse_identifier(context)?,
        TokenKind::Literal => Expr::Literal(string_to_num(&advance(context).value)?),
        _ => return Err(CalcError::UnableToParseExpression),
    };

    Ok(expr)
}

fn parse_group_fn(context: &mut Context) -> Result<Expr, CalcError> {
    let name = match &peek(context).kind {
        TokenKind::Pipe => "abs",
        TokenKind::OpenCeil => "ceil",
        TokenKind::OpenFloor => "floor",
        _ => unreachable!(),
    };

    match parse_vector(context)? {
        Expr::Vector(arguments) => Ok(Expr::FnCall(Identifier::from_full_name(name), arguments)),
        Expr::Group(argument) => Ok(Expr::FnCall(
            Identifier::from_full_name(name),
            vec![*argument],
        )),
        _ => unreachable!(),
    }
}

fn parse_vector(context: &mut Context) -> Result<Expr, CalcError> {
    let kind = advance(context).kind;

    if kind == TokenKind::OpenBracket {
        skip_newlines(context);
    }

    let mut rows = vec![vec![parse_expr(context)?]];
    let mut column_count = None;
    let mut items_in_row = 1;
    while match_token(context, TokenKind::Comma)
        || match_token(context, TokenKind::Semicolon)
        || (match_token(context, TokenKind::Newline)
            && peek_next(context).kind != TokenKind::ClosedBracket)
    {
        if kind == TokenKind::OpenBracket
            && (match_token(context, TokenKind::Newline)
                || match_token(context, TokenKind::Semicolon))
        {
            if let Some(columns) = column_count {
                if columns != items_in_row {
                    return Err(CalcError::InconsistentColumnWidths);
                }
            } else {
                column_count = Some(items_in_row);
            }

            rows.push(Vec::new());
            items_in_row = 0;
        }

        advance(context);
        rows.last_mut().unwrap().push(parse_expr(context)?);
        items_in_row += 1;
    }

    if peek(context).kind == TokenKind::EOF {
        return Err(CalcError::Expected(String::from(
            "Closing group symbol, eg. )",
        )));
    }

    if kind == TokenKind::OpenBracket {
        skip_newlines(context);
    }

    advance(context);

    if rows.len() == 1 {
        let mut values = rows.pop().unwrap();
        if values.len() == 1 {
            Ok(Expr::Group(Box::new(values.pop().unwrap())))
        } else {
            Ok(Expr::Vector(values))
        }
    } else {
        Ok(Expr::Matrix(rows))
    }
}

fn parse_identifier(context: &mut Context) -> Result<Expr, CalcError> {
    let identifier = Identifier::from_full_name(&advance(context).value);
    if context.parsing_unit_decl
        && !context
            .symbol_table
            .get_mut()
            .contains_var(&identifier.full_name)
    {
        context.unit_decl_base_unit = Some(identifier.full_name);
        Ok(Expr::Var(Identifier::from_full_name(DECL_UNIT)))
    } else {
        Ok(Expr::Var(identifier))
    }
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

        let parsed = parse_stmt(context)?;
        let mut symbol_table = context.symbol_table.get_mut();
        analysis::analyse_stmt(&mut symbol_table, parsed)
    }

    fn parse(tokens: Vec<Token>) -> Result<Stmt, CalcError> {
        let mut context = Context::new();
        context.tokens = tokens;
        context.pos = 0;

        let parsed = parse_stmt(&mut context)?;
        let mut symbol_table = context.symbol_table.get_mut();
        analysis::analyse_stmt(&mut symbol_table, parsed)
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
        context.symbol_table.get_mut().insert(Stmt::VarDecl(
            Identifier::from_full_name("x"),
            literal(1f64),
        ));
        context.symbol_table.get_mut().insert(Stmt::VarDecl(
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
            .get_mut()
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
        context.symbol_table.get_mut().set(Stmt::FnDecl(
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
