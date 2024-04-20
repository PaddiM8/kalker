use std::cell::Cell;

use crate::analysis;
use crate::ast::Identifier;
use crate::calculation_result::CalculationResult;
use crate::errors::KalkError;
use crate::kalk_value::KalkFloat;
use crate::{
    ast::{Expr, Stmt},
    interpreter,
    lexer::{Lexer, Token, TokenKind},
    symbol_table::SymbolTable,
};
use wasm_bindgen::prelude::*;

pub const DECL_UNIT: &str = ".u";
pub const DEFAULT_ANGLE_UNIT: &str = "rad";

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
    other_radix: Option<u8>,
    current_stmt_start_pos: usize,
    max_recursion_depth: Option<u32>,
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
            other_radix: None,
            current_stmt_start_pos: 0,
            max_recursion_depth: None,
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

    pub fn set_max_recursion_depth(mut self, depth: u32) -> Self {
        self.max_recursion_depth = Some(depth);

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

/// Evaluate expressions/declarations and return the answer.
///
/// `None` will be returned if the last statement is a declaration.
pub fn eval(
    context: &mut Context,
    input: &str,
    #[cfg(feature = "rug")] precision: u32,
) -> Result<Option<CalculationResult>, KalkError> {
    let statements = parse(context, input)?;

    let symbol_table = context.symbol_table.get_mut();
    let mut interpreter = interpreter::Context::new(
        symbol_table,
        &context.angle_unit,
        #[cfg(feature = "rug")]
        precision,
        context.timeout.map(|timeout| timeout as u128),
    );

    if let Some(max_recursion_depth) = context.max_recursion_depth {
        interpreter = interpreter.set_max_recursion_depth(max_recursion_depth);
    }

    let result = interpreter.interpret(statements);
    if let Ok(Some(mut num)) = result {
        if !num.set_radix(context.other_radix.unwrap_or(10)) {
            Err(KalkError::InvalidBase)
        } else {
            Ok(Some(num))
        }
    } else {
        result
    }
}

/// Parse expressions/declarations and return a syntax tree.
///
/// `None` will be returned if the last statement is a declaration.
pub fn parse(context: &mut Context, input: &str) -> Result<Vec<Stmt>, KalkError> {
    let mut lexer = Lexer::new(input);
    context.tokens = lexer.lex();
    context.pos = 0;
    context.parsing_unit_decl = false;
    context.unit_decl_base_unit = None;
    context.other_radix = lexer.get_other_radix();

    let mut statements: Vec<Stmt> = Vec::new();
    while !is_at_end(context) {
        context.current_stmt_start_pos = context.pos;
        let parsed = match parse_stmt(context) {
            Ok(stmt) => stmt,
            Err(KalkError::WasStmt(stmt)) => stmt,
            Err(err) => return Err(err),
        };
        let symbol_table = context.symbol_table.get_mut();
        let analysed = analysis::analyse_stmt(symbol_table, parsed)?;
        statements.push(analysed);

        if match_token(context, TokenKind::Semicolon) {
            advance(context);
        }

        skip_newlines(context);
    }

    Ok(statements)
}

fn parse_stmt(context: &mut Context) -> Result<Stmt, KalkError> {
    if match_token(context, TokenKind::UnitKeyword) {
        parse_unit_decl_stmt(context)
    } else {
        Ok(Stmt::Expr(Box::new(parse_expr(context)?)))
    }
}

fn parse_piecewise(context: &mut Context) -> Result<Expr, KalkError> {
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
                Box::new(Expr::Literal(crate::float!(1f64))),
                TokenKind::Equals,
                Box::new(Expr::Literal(crate::float!(1f64))),
            );
            pieces.push(crate::ast::ConditionalPiece {
                expr: left_expr,
                condition: true_expr,
            });

            reached_otherwise = true;
        } else {
            return Err(KalkError::ExpectedIf);
        }

        if match_token(context, TokenKind::Semicolon) {
            advance(context);
        }
        skip_newlines(context);

        (previous(context).kind == TokenKind::Semicolon
            || previous(context).kind == TokenKind::Newline)
            && !reached_otherwise
            && !match_token(context, TokenKind::ClosedBrace)
    } {}

    advance(context);

    Ok(Expr::Piecewise(pieces))
}

fn parse_unit_decl_stmt(context: &mut Context) -> Result<Stmt, KalkError> {
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
        return Err(KalkError::InvalidUnit);
    };

    // Automatically create a second unit decl with the expression inverted.
    // This will turn eg. unit a = 3b, into unit b = a/3
    // This is so that you only have to define `a`, and it will figure out the formula for `b` since it is used in the formula for `a`.
    let stmt_inv = Stmt::UnitDecl(
        base_unit.clone(),
        identifier.value.clone(),
        Box::new(def.invert(context.symbol_table.get_mut(), DECL_UNIT)?),
    );
    let stmt = Stmt::UnitDecl(identifier.value, base_unit, Box::new(def));

    context.symbol_table.get_mut().insert(stmt.clone());
    context.symbol_table.get_mut().insert(stmt_inv);

    Ok(stmt)
}

fn parse_expr(context: &mut Context) -> Result<Expr, KalkError> {
    parse_or(context)
}

fn parse_comprehension(context: &mut Context) -> Result<Expr, KalkError> {
    let left = parse_or(context)?;

    if match_token(context, TokenKind::Colon) {
        let op = advance(context).kind;
        skip_newlines(context);
        let right = Box::new(parse_comprehension_comma(context)?);
        return Ok(Expr::Binary(Box::new(left), op, right));
    }

    Ok(left)
}

fn parse_comprehension_comma(context: &mut Context) -> Result<Expr, KalkError> {
    let left = parse_or(context)?;

    if match_token(context, TokenKind::Comma) {
        let op = advance(context).kind;
        skip_newlines(context);
        let right = Box::new(parse_comprehension_comma(context)?);
        return Ok(Expr::Binary(Box::new(left), op, right));
    }

    Ok(left)
}

fn parse_or(context: &mut Context) -> Result<Expr, KalkError> {
    let left = parse_and(context)?;

    if match_token(context, TokenKind::Or) {
        let op = advance(context).kind;
        skip_newlines(context);
        let right = Box::new(parse_or(context)?);
        return Ok(Expr::Binary(Box::new(left), op, right));
    }

    Ok(left)
}

fn parse_and(context: &mut Context) -> Result<Expr, KalkError> {
    let left = parse_comparison(context)?;

    if match_token(context, TokenKind::And) {
        let op = advance(context).kind;
        skip_newlines(context);
        let right = Box::new(parse_and(context)?);
        return Ok(Expr::Binary(Box::new(left), op, right));
    }

    Ok(left)
}

fn parse_comparison(context: &mut Context) -> Result<Expr, KalkError> {
    let at_start_of_line = context.current_stmt_start_pos == context.pos;
    let mut left = parse_to(context)?;

    // Equality check
    while match_token(context, TokenKind::Equals)
        || match_token(context, TokenKind::NotEquals)
        || match_token(context, TokenKind::GreaterThan)
        || match_token(context, TokenKind::LessThan)
        || match_token(context, TokenKind::GreaterOrEquals)
        || match_token(context, TokenKind::LessOrEquals)
    {
        let op = advance(context).kind;

        if let (true, Some((identifier, parameters))) =
            (at_start_of_line, analysis::is_fn_decl(&left))
        {
            context.symbol_table.get_mut().set(Stmt::FnDecl(
                identifier.clone(),
                parameters.clone(),
                Box::new(Expr::Literal(crate::float!(1f64))),
            ));
            let right = if match_token(context, TokenKind::OpenBrace) {
                parse_piecewise(context)?
            } else {
                parse_expr(context)?
            };
            let fn_decl = Stmt::FnDecl(identifier, parameters, Box::new(right));

            // Hack to return a statement...
            return Err(KalkError::WasStmt(fn_decl));
        };

        let right = parse_comparison(context)?;

        left = match right {
            Expr::Binary(
                inner_left,
                inner_op @ (TokenKind::Equals
                | TokenKind::NotEquals
                | TokenKind::GreaterThan
                | TokenKind::LessThan
                | TokenKind::GreaterOrEquals
                | TokenKind::LessOrEquals),
                inner_right,
            ) => Expr::Binary(
                Box::new(Expr::Binary(
                    Box::new(left),
                    op,
                    Box::new(*inner_left.clone()),
                )),
                TokenKind::And,
                Box::new(Expr::Binary(
                    Box::new(*inner_left),
                    inner_op,
                    Box::new(*inner_right),
                )),
            ),
            _ => Expr::Binary(Box::new(left), op, Box::new(right)),
        }
    }

    Ok(left)
}

fn parse_to(context: &mut Context) -> Result<Expr, KalkError> {
    let left = parse_term(context)?;

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

fn parse_term(context: &mut Context) -> Result<Expr, KalkError> {
    let mut left = parse_factor(context)?;

    while match_token(context, TokenKind::Plus) || match_token(context, TokenKind::Minus) {
        let op = peek(context).kind;
        advance(context);
        let right = parse_factor(context)?;

        left = Expr::Binary(Box::new(left), op, Box::new(right));
    }

    Ok(left)
}

fn parse_factor(context: &mut Context) -> Result<Expr, KalkError> {
    let mut left = parse_unit(context)?;

    if let Expr::Unary(TokenKind::Percent, percent_left) = left.clone() {
        let try_parse = parse_unit(context);
        if try_parse.is_ok() {
            left = Expr::Binary(percent_left, TokenKind::Percent, Box::new(try_parse?));
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

fn parse_unit(context: &mut Context) -> Result<Expr, KalkError> {
    let expr = parse_exponent(context)?;

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

fn parse_exponent(context: &mut Context) -> Result<Expr, KalkError> {
    let left = parse_unary(context)?;

    if match_token(context, TokenKind::Power) {
        let op = advance(context).kind;
        let right = Box::new(parse_exponent(context)?);

        return Ok(Expr::Binary(Box::new(left), op, right));
    }

    Ok(left)
}

fn parse_unary(context: &mut Context) -> Result<Expr, KalkError> {
    if match_token(context, TokenKind::Minus) || match_token(context, TokenKind::Not) {
        let op = advance(context).kind;
        let expr = Box::new(parse_unary(context)?);

        return Ok(Expr::Unary(op, expr));
    }

    let expr = parse_indexer(context)?;
    if match_token(context, TokenKind::Percent) {
        Ok(Expr::Unary(advance(context).kind, Box::new(expr)))
    } else {
        Ok(expr)
    }
}

fn parse_indexer(context: &mut Context) -> Result<Expr, KalkError> {
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

fn parse_factorial(context: &mut Context) -> Result<Expr, KalkError> {
    let expr = parse_primary(context)?;

    Ok(if match_token(context, TokenKind::Exclamation) {
        advance(context);
        Expr::Unary(TokenKind::Exclamation, Box::new(expr))
    } else {
        expr
    })
}

fn parse_primary(context: &mut Context) -> Result<Expr, KalkError> {
    let expr = match peek(context).kind {
        TokenKind::OpenParenthesis | TokenKind::OpenBracket => parse_vector(context)?,
        TokenKind::Pipe | TokenKind::OpenCeil | TokenKind::OpenFloor => parse_group_fn(context)?,
        TokenKind::Identifier => parse_identifier(context)?,
        TokenKind::Literal => Expr::Literal(string_to_num(&advance(context).value)?),
        TokenKind::True => {
            advance(context);
            Expr::Boolean(true)
        }
        TokenKind::False => {
            advance(context);
            Expr::Boolean(false)
        }
        _ => return Err(KalkError::UnexpectedToken(peek(context).kind, None)),
    };

    Ok(expr)
}

fn parse_group_fn(context: &mut Context) -> Result<Expr, KalkError> {
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

fn parse_vector(context: &mut Context) -> Result<Expr, KalkError> {
    let kind = advance(context).kind;
    if match_token(context, TokenKind::ClosedBracket) {
        advance(context);

        return Ok(Expr::Vector(vec![]));
    }

    if kind == TokenKind::OpenBracket {
        skip_newlines(context);
    }

    let first_expr = if kind == TokenKind::OpenBracket {
        parse_comprehension(context)?
    } else {
        parse_expr(context)?
    };
    let mut rows = vec![vec![first_expr]];
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
                    return Err(KalkError::InconsistentColumnWidths);
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

    if peek(context).kind == TokenKind::Eof {
        return Err(KalkError::Expected(String::from(
            "Closing group symbol, eg. )",
        )));
    }

    if kind == TokenKind::OpenBracket {
        skip_newlines(context);
    }

    advance(context);

    if rows.len() == 1 {
        let mut values = rows.pop().unwrap();
        if values.len() == 1 && kind == TokenKind::OpenParenthesis {
            Ok(Expr::Group(Box::new(values.pop().unwrap())))
        } else {
            Ok(Expr::Vector(values))
        }
    } else {
        Ok(Expr::Matrix(rows))
    }
}

fn parse_identifier(context: &mut Context) -> Result<Expr, KalkError> {
    let at_start_of_line = context.current_stmt_start_pos == context.pos;
    let identifier = Identifier::from_full_name(&advance(context).value);

    let mut log_base = None;
    if identifier.full_name.starts_with("log") {
        if let Some(lowered) = identifier.get_lowered_part() {
            #[cfg(feature = "rug")]
            fn parse_float_from_str(s: &str) -> Option<rug::Float> {
                rug::Float::parse(s).map(|valid| crate::float!(valid)).ok()
            }

            #[cfg(not(feature = "rug"))]
            fn parse_float_from_str(s: &str) -> Option<f64> {
                s.parse::<f64>().ok()
            }
            if let Some(lowered_float) = parse_float_from_str(lowered) {
                log_base = Some(Expr::Literal(lowered_float));
            }
        }
    }

    if context.parsing_unit_decl
        && !context
            .symbol_table
            .get_mut()
            .contains_var(&identifier.full_name)
    {
        context.unit_decl_base_unit = Some(identifier.full_name);
        Ok(Expr::Var(Identifier::from_full_name(DECL_UNIT)))
    } else if log_base.is_some()
        || context
            .symbol_table
            .get_mut()
            .contains_fn(&identifier.pure_name)
    {
        let identifier_pos = context.pos;

        // Function call
        // If there is a parenthesis/brace, parse that as a
        // vector/group, otherwise it's an expression like sqrt4,
        // which should be parsed as a factor, to allow eg. sqrt2x.
        let mut arguments = if match_token(context, TokenKind::OpenBrace)
            || match_token(context, TokenKind::OpenParenthesis)
        {
            match parse_primary(context)? {
                Expr::Vector(arguments) => arguments,
                Expr::Group(argument) => vec![*argument],
                argument => vec![argument],
            }
        } else {
            vec![parse_factor(context)?]
        };

        // If it's a re-definition, revert and parse as a declaration
        if at_start_of_line
            && match_token(context, TokenKind::Equals)
            && arguments.iter().all(|x| matches!(x, Expr::Var(_)))
        {
            for argument in &arguments {
                let mut all_vars_exist = true;
                if let Expr::Var(argument_identifier) = argument {
                    if !context
                        .symbol_table
                        .get_mut()
                        .contains_var(&argument_identifier.full_name)
                    {
                        all_vars_exist = false;
                    }
                }

                if !all_vars_exist {
                    context
                        .symbol_table
                        .get_mut()
                        .get_and_remove_fn(&identifier.full_name);
                    context.pos = identifier_pos;

                    return Ok(Expr::Var(identifier));
                }
            }
        }

        if let Some(log_base) = log_base {
            let log_arg = arguments.remove(0);
            Ok(Expr::FnCall(
                Identifier::from_full_name("log"),
                vec![log_arg, log_base],
            ))
        } else {
            Ok(Expr::FnCall(identifier, arguments))
        }
    } else {
        Ok(Expr::Var(identifier))
    }
}

fn peek(context: &Context) -> &Token {
    if context.pos >= context.tokens.len() {
        context.tokens.last().unwrap() // Eof
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

fn consume(context: &mut Context, kind: TokenKind) -> Result<&Token, KalkError> {
    if match_token(context, kind) {
        return Ok(advance(context));
    }

    Err(KalkError::UnexpectedToken(peek(context).kind, Some(kind)))
}

fn is_at_end(context: &Context) -> bool {
    context.pos >= context.tokens.len() || peek(context).kind == TokenKind::Eof
}

fn skip_newlines(context: &mut Context) {
    while match_token(context, TokenKind::Newline) {
        advance(context);
    }
}

fn string_to_num(value: &str) -> Result<KalkFloat, KalkError> {
    #[cfg(feature = "rug")]
    use rug::ops::Pow;

    if value.contains('E') {
        let parts = value.split('E').collect::<Vec<_>>();
        let left = crate::float!(string_to_num(parts[0])?);
        let right = crate::float!(string_to_num(parts[1])?);

        #[cfg(feature = "rug")]
        return Ok(left * 10.pow(right));

        #[cfg(not(feature = "rug"))]
        return Ok(left * 10_f64.powf(right));
    }

    let base = get_base(value)?;
    if let Some(result) = crate::radix::parse_float_radix(&value.replace(' ', ""), base) {
        Ok(crate::float!(result))
    } else {
        Err(KalkError::InvalidNumberLiteral(value.into()))
    }
}

fn get_base(value: &str) -> Result<u8, KalkError> {
    let underscore_pos = if let Some(i) = value.find('_') {
        i
    } else {
        return Ok(10);
    };

    let subscript = value.chars().skip(underscore_pos + 1usize);
    if let Some(base) = crate::text_utils::parse_subscript(subscript) {
        Ok(base)
    } else {
        Err(KalkError::UnrecognizedBase)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Identifier;
    use crate::lexer::{Token, TokenKind::*};
    use crate::test_helpers::*;
    use wasm_bindgen_test::*;

    fn parse_with_context(context: &mut Context, tokens: Vec<Token>) -> Result<Stmt, KalkError> {
        context.tokens = tokens;
        context.pos = 0;

        let parsed = parse_stmt(context)?;
        let symbol_table = context.symbol_table.get_mut();
        analysis::analyse_stmt(symbol_table, parsed)
    }

    fn parse(tokens: Vec<Token>) -> Result<Stmt, KalkError> {
        let mut context = Context::new();
        context.tokens = tokens;
        context.pos = 0;

        let parsed = match parse_stmt(&mut context) {
            Ok(stmt) => stmt,
            Err(KalkError::WasStmt(stmt)) => stmt,
            Err(err) => return Err(err),
        };
        let symbol_table = context.symbol_table.get_mut();
        analysis::analyse_stmt(symbol_table, parsed)
    }

    #[test]
    #[wasm_bindgen_test]
    fn test_var() {
        // x
        let tokens = vec![token(Identifier, "x"), token(Eof, "")];

        assert_eq!(parse(tokens).unwrap(), Stmt::Expr(var("x")));
    }

    #[test]
    #[wasm_bindgen_test]
    fn test_var_multiplication() {
        let mut context = Context::new();
        context.symbol_table.get_mut().insert(Stmt::VarDecl(
            Identifier::from_full_name("x"),
            f64_to_float_literal(1f64),
        ));
        context.symbol_table.get_mut().insert(Stmt::VarDecl(
            Identifier::from_full_name("y"),
            f64_to_float_literal(2f64),
        ));

        // xy²
        let tokens = vec![
            token(Identifier, "xy"),
            token(Power, ""),
            token(Literal, "2"),
            token(Eof, ""),
        ];

        assert_eq!(
            parse(tokens).unwrap(),
            Stmt::Expr(binary(
                var("x"),
                Star,
                binary(var("y"), Power, f64_to_float_literal(2f64))
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
            token(Eof, ""),
        ];

        assert_eq!(
            parse(tokens).unwrap(),
            Stmt::Expr(binary(
                f64_to_float_literal(1f64),
                Plus,
                binary(
                    f64_to_float_literal(2f64),
                    Star,
                    group(binary(
                        f64_to_float_literal(3f64),
                        Minus,
                        binary(
                            f64_to_float_literal(4f64),
                            Slash,
                            f64_to_float_literal(5f64)
                        )
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
            token(Eof, ""),
        ];

        assert_eq!(
            parse(tokens).unwrap(),
            Stmt::Expr(binary(
                binary(
                    f64_to_float_literal(1f64),
                    Star,
                    binary(
                        f64_to_float_literal(2f64),
                        Power,
                        binary(
                            f64_to_float_literal(3f64),
                            Power,
                            f64_to_float_literal(4f64)
                        ),
                    ),
                ),
                Plus,
                f64_to_float_literal(5f64)
            )),
        );
    }

    #[wasm_bindgen_test]
    fn test_pow_unary() {
        let tokens = vec![
            token(Literal, "10"),
            token(Power, ""),
            token(Minus, ""),
            token(Literal, "1"),
            token(Eof, ""),
        ];

        assert_eq!(
            parse(tokens).unwrap(),
            Stmt::Expr(binary(
                f64_to_float_literal(10f64),
                Power,
                unary(Minus, f64_to_float_literal(1f64))
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
            token(Eof, ""),
        ];

        assert_eq!(
            parse(tokens).unwrap(),
            Stmt::Expr(binary(
                binary(
                    f64_to_float_literal(1f64),
                    Percent,
                    f64_to_float_literal(1f64)
                ),
                Plus,
                unary(Percent, f64_to_float_literal(5f64))
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
            Stmt::Expr(unit("a", f64_to_float_literal(1f64)))
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
            token(Eof, ""),
        ];

        assert_eq!(
            parse(tokens).unwrap(),
            Stmt::VarDecl(
                Identifier::from_full_name("x"),
                binary(f64_to_float_literal(1f64), Plus, f64_to_float_literal(2f64))
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
            token(Eof, ""),
        ];

        assert_eq!(
            parse(tokens).unwrap(),
            Stmt::FnDecl(
                Identifier::from_full_name("f"),
                vec![String::from("f-x")],
                binary(f64_to_float_literal(1f64), Plus, param_var("f", "x"))
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
            token(Eof, ""),
        ];

        let mut context = Context::new();

        // Add the function to the symbol table first, in order to prevent errors.
        context.symbol_table.get_mut().set(Stmt::FnDecl(
            Identifier::from_full_name("f"),
            vec![String::from("x")],
            f64_to_float_literal(1f64),
        ));

        assert_eq!(
            parse_with_context(&mut context, tokens).unwrap(),
            Stmt::Expr(binary(
                Box::new(Expr::FnCall(
                    Identifier::from_full_name("f"),
                    vec![*binary(
                        f64_to_float_literal(1f64),
                        Plus,
                        f64_to_float_literal(2f64)
                    )]
                )),
                Plus,
                f64_to_float_literal(3f64)
            ))
        );
    }
}
