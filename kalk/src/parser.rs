use crate::kalk_num::KalkNum;
use crate::{
    ast::{Expr, Stmt},
    interpreter,
    lexer::{Lexer, Token, TokenKind},
    symbol_table::SymbolTable,
};

pub const DECL_UNIT: &'static str = ".u";
pub const DEFAULT_ANGLE_UNIT: &'static str = "rad";

/// Struct containing the current state of the parser. It stores user-defined functions and variables.
/// # Examples
/// ```
/// use kalk::parser;
/// let mut parser_context = parser::Context::new();
/// let precision = 53;
/// let result = parser::eval(&mut parser_context, "5*3", precision).unwrap().unwrap();
/// assert_eq!(result.to_f64(), 15f64);
/// ```
pub struct Context {
    tokens: Vec<Token>,
    pos: usize,
    symbol_table: SymbolTable,
    angle_unit: String,
    /// This is true whenever the parser is currently parsing a unit declaration.
    /// It is necessary to keep track of this in order to know when to find (figure out) units that haven't been defined yet.
    /// Unit names are instead treated as variables.
    parsing_unit_decl: bool,
    /// When a unit declaration is being parsed, this value will be set
    /// whenever a unit in the expression is found. Eg. unit a = 3b, it will be set to Some("b")
    unit_decl_base_unit: Option<String>,
    parsing_identifier_stmt: bool,
}

impl Context {
    pub fn new() -> Self {
        let mut context = Self {
            tokens: Vec::new(),
            pos: 0,
            symbol_table: SymbolTable::new(),
            angle_unit: DEFAULT_ANGLE_UNIT.into(),
            parsing_unit_decl: false,
            unit_decl_base_unit: None,
            parsing_identifier_stmt: false,
        };

        parse(&mut context, crate::prelude::INIT).unwrap();

        context
    }

    pub fn set_angle_unit(mut self, unit: &str) -> Self {
        self.angle_unit = unit.into();

        self
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
    IncorrectAmountOfArguments(usize, String, usize),
    InvalidNumberLiteral(String),
    InvalidOperator,
    InvalidUnit,
    UnexpectedToken(TokenKind),
    UndefinedFn(String),
    UndefinedVar(String),
    UnableToInvert(String),
    UnableToParseExpression,
    Unknown,
}

/// Evaluate expressions/declarations and return the answer.
///
/// `None` will be returned if the last statement is a declaration.
pub fn eval(
    context: &mut Context,
    input: &str,
    precision: u32,
) -> Result<Option<KalkNum>, CalcError> {
    let statements = parse(context, input)?;

    let mut interpreter =
        interpreter::Context::new(&mut context.symbol_table, &context.angle_unit, precision);
    interpreter.interpret(statements)
}

/// Parse expressions/declarations and return a syntax tree.
///
/// `None` will be returned if the last statement is a declaration.
pub fn parse(context: &mut Context, input: &str) -> Result<Vec<Stmt>, CalcError> {
    context.tokens = Lexer::lex(input);
    context.pos = 0;
    context.parsing_unit_decl = false;
    context.unit_decl_base_unit = None;

    let mut statements: Vec<Stmt> = Vec::new();
    while !is_at_end(context) {
        statements.push(parse_stmt(context)?);

        if match_token(context, TokenKind::Semicolon) {
            advance(context);
        }
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

    // If `primary` is followed by an equal sign, it is a function declaration.
    if let TokenKind::Equals = peek(context).kind {
        advance(context);
        let expr = parse_expr(context)?;

        // Use the "function call" expression that was parsed, and put its values into a function declaration statement instead.
        if let Expr::FnCall(identifier, parameters) = primary {
            let mut parameter_identifiers = Vec::new();

            // All the "arguments" are expected to be parsed as variables,
            // since parameter definitions look the same as variable references.
            // Extract these.
            for parameter in parameters {
                if let Expr::Var(parameter_identifier) = parameter {
                    parameter_identifiers.push(parameter_identifier);
                }
            }

            let fn_decl = Stmt::FnDecl(identifier.clone(), parameter_identifiers, Box::new(expr));

            // Insert the function declaration into the symbol table during parsing
            // so that the parser can find out if particular functions exist.
            context.symbol_table.insert(fn_decl.clone());

            return Ok(fn_decl);
        }

        Err(CalcError::Unknown)
    } else {
        // It is a function call or eg. x(x + 3), not a function declaration.
        // Redo the parsing for this specific part.
        context.pos = began_at;
        Ok(Stmt::Expr(Box::new(parse_expr(context)?)))
    }
}

fn parse_var_decl_stmt(context: &mut Context) -> Result<Stmt, CalcError> {
    let identifier = advance(context).clone();
    advance(context); // Equal sign
    let expr = parse_expr(context)?;

    Ok(Stmt::VarDecl(identifier.value, Box::new(expr)))
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
        Box::new(def.invert(&mut context.symbol_table)?),
    );
    let stmt = Stmt::UnitDecl(identifier.value, base_unit, Box::new(def));

    context.symbol_table.insert(stmt.clone());
    context.symbol_table.insert(stmt_inv);

    Ok(stmt)
}

fn parse_expr(context: &mut Context) -> Result<Expr, CalcError> {
    Ok(parse_to(context)?)
}

fn parse_to(context: &mut Context) -> Result<Expr, CalcError> {
    let left = parse_sum(context)?;

    if match_token(context, TokenKind::ToKeyword) {
        let op = advance(context).kind;
        let right = Expr::Var(advance(context).value.clone()); // Parse this as a variable for now.

        return Ok(Expr::Binary(Box::new(left), op, Box::new(right)));
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
    {
        // If the token is an identifier, literal, or open parenthesis,
        // assume it's multiplication. Eg. 3y or (3x + 2)(2 + 3)
        let op = match peek(context).kind {
            TokenKind::Identifier | TokenKind::Literal | TokenKind::OpenParenthesis => {
                TokenKind::Star
            }
            _ => advance(context).kind,
        };

        let parse_next = parse_unit(context);
        let right = if let Ok(right) = parse_next {
            right
        /*} else if let Err(CalcError::UnableToParseExpression) = parse_next {
        // If it failed to parse further,
        // try to parse it as something else.
        // Eg. percent unary
        break;*/
        } else {
            return parse_next;
        };

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
        TokenKind::Pipe | TokenKind::OpenCeil | TokenKind::OpenFloor => parse_group_fn(context)?,
        TokenKind::Identifier => parse_identifier(context)?,
        TokenKind::Literal => Expr::Literal(string_to_num(&advance(context).value)),
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
        _ => panic!("Unexpected parsing error."),
    };

    let expr = parse_expr(context)?;
    advance(context);

    Ok(Expr::FnCall(name.to_string(), vec![expr]))
}

fn parse_identifier(context: &mut Context) -> Result<Expr, CalcError> {
    let identifier = advance(context).clone();

    // Eg. sqrt64
    if match_token(context, TokenKind::Literal) {
        // If there is a function with this name, parse it as a function, with the next token as the argument.
        if context.symbol_table.contains_fn(&identifier.value) {
            let parameter = Expr::Literal(string_to_num(&advance(context).value));
            return Ok(Expr::FnCall(identifier.value, vec![parameter]));
        }
    }

    let parse_as_var_instead = match_token(context, TokenKind::OpenParenthesis)
        && !context.parsing_identifier_stmt
        && !context.symbol_table.contains_fn(&identifier.value);

    // Eg. sqrt(64)
    // If the function doesn't exist, parse it as a variable and multiplication instead.
    // Although, if the parse_identifier_stmt function called this function,
    // parse it as a function anyway, since it might be creating one.
    if !parse_as_var_instead && match_token(context, TokenKind::OpenParenthesis) {
        advance(context);

        let mut parameters = Vec::new();
        parameters.push(parse_expr(context)?);

        while match_token(context, TokenKind::Comma) {
            advance(context);
            parameters.push(parse_expr(context)?);
        }

        consume(context, TokenKind::ClosedParenthesis)?;

        return Ok(Expr::FnCall(identifier.value, parameters));
    }

    // Eg. x
    if parse_as_var_instead || context.symbol_table.contains_var(&identifier.value) {
        Ok(Expr::Var(identifier.value))
    } else if context.parsing_unit_decl {
        context.unit_decl_base_unit = Some(identifier.value);
        Ok(Expr::Var(DECL_UNIT.into()))
    } else {
        let mut chars = identifier.value.chars();
        let mut left = Expr::Var(chars.next().unwrap().to_string());

        // Turn each individual character into its own variable reference.
        // This parses eg `xy` as `x*y` instead of *one* variable.
        for c in chars {
            left = Expr::Binary(
                Box::new(left),
                TokenKind::Star,
                Box::new(Expr::Var(c.to_string())),
            );
        }

        Ok(left)
    }
}

fn peek(context: &Context) -> &Token {
    &context.tokens[context.pos]
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

    Err(CalcError::UnexpectedToken(kind))
}

fn is_at_end(context: &Context) -> bool {
    context.pos >= context.tokens.len() || peek(context).kind == TokenKind::EOF
}

fn string_to_num(value: &str) -> f64 {
    value.replace(" ", "").parse::<f64>().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Token, TokenKind::*};
    use crate::test_helpers::*;

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
    fn test_var() {
        // x
        let tokens = vec![token(Identifier, "x"), token(EOF, "")];

        assert_eq!(parse(tokens).unwrap(), Stmt::Expr(var("x")));
    }

    #[test]
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
                String::from("x"),
                binary(literal(1f64), Plus, literal(2f64))
            )
        );
    }

    #[test]
    fn test_fn_decl() {
        let tokens = vec![
            token(Identifier, "f"),
            token(OpenParenthesis, ""),
            token(Identifier, "x"),
            token(ClosedParenthesis, ""),
            token(Equals, ""),
            token(Literal, "1"),
            token(Plus, ""),
            token(Literal, "2"),
            token(EOF, ""),
        ];

        assert_eq!(
            parse(tokens).unwrap(),
            Stmt::FnDecl(
                String::from("f"),
                vec![String::from("x")],
                binary(literal(1f64), Plus, literal(2f64))
            )
        );
    }

    #[test]
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
            String::from("f"),
            vec![String::from("x")],
            literal(1f64),
        ));

        assert_eq!(
            parse_with_context(&mut context, tokens).unwrap(),
            Stmt::Expr(binary(
                Box::new(Expr::FnCall(
                    String::from("f"),
                    vec![*binary(literal(1f64), Plus, literal(2f64))]
                )),
                Plus,
                literal(3f64)
            ))
        );
    }
}
