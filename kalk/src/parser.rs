use crate::{
    ast::{Expr, Stmt},
    interpreter,
    lexer::{Lexer, Token, TokenKind},
    symbol_table::SymbolTable,
};
use rug::Float;

pub const DECL_UNIT: &'static str = ".u";

/// Struct containing the current state of the parser. It stores user-defined functions and variables.
/// # Examples
/// ```
/// use kalk::parser;
/// let mut parser_context = parser::Context::new();
/// let precision = 53;
/// assert_eq!(parser::eval(&mut parser_context, "5*3", precision).unwrap().unwrap(), 15);
/// ```
pub struct Context {
    tokens: Vec<Token>,
    pos: usize,
    symbol_table: SymbolTable,
    angle_unit: Unit,
    /// This is true whenever the parser is currently parsing a unit declaration.
    /// It is necessary to keep track of this in order to know when to find (figure out) units that haven't been defined yet.
    /// Unit names are instead treated as variables.
    parsing_unit_decl: bool,
    /// When a unit declaration is being parsed, this value will be set
    /// whenever a unit in the expression is found. Eg. unit a = 3b, it will be set to Some("b")
    unit_decl_base_unit: Option<String>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            tokens: Vec::new(),
            pos: 0,
            symbol_table: SymbolTable::new(),
            angle_unit: Unit::Radians,
            parsing_unit_decl: false,
            unit_decl_base_unit: None,
        }
    }

    pub fn set_angle_unit(mut self, unit: Unit) -> Self {
        self.angle_unit = unit;

        self
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

/// Mathematical unit used in calculations.
#[derive(Debug, Clone, PartialEq)]
pub enum Unit {
    Radians,
    Degrees,
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
    Unknown,
}

/// Evaluate expressions/declarations and return the answer.
///
/// `None` will be returned if the last statement is a declaration.
pub fn eval(
    context: &mut Context,
    input: &str,
    precision: u32,
) -> Result<Option<Float>, CalcError> {
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

    let mut statements: Vec<Stmt> = Vec::new();
    while !is_at_end(context) {
        statements.push(parse_stmt(context)?);
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
    let primary = parse_primary(context)?; // Since function declarations and function calls look the same at first, simply parse a "function call", and re-use the data.

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
        // It is a function call, not a function declaration.
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

    // Parse the definition
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
    Ok(parse_sum(context)?)
}

fn parse_sum(context: &mut Context) -> Result<Expr, CalcError> {
    let mut left = parse_factor(context)?;

    while match_token(context, TokenKind::Plus) || match_token(context, TokenKind::Minus) {
        let op = peek(context).kind.clone();
        advance(context);
        let right = parse_factor(context)?;

        left = Expr::Binary(Box::new(left), op, Box::new(right));
    }

    Ok(left)
}

fn parse_factor(context: &mut Context) -> Result<Expr, CalcError> {
    let mut left = parse_unit(context)?;

    while match_token(context, TokenKind::Star)
        || match_token(context, TokenKind::Slash)
        || match_token(context, TokenKind::Identifier)
        || match_token(context, TokenKind::Literal)
    {
        // If the next token is an identifier, assume it's multiplication. Eg. 3y
        let op = match peek(context).kind {
            TokenKind::Identifier | TokenKind::Literal => TokenKind::Star,
            _ => advance(context).kind.clone(),
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
        let op = advance(context).kind.clone();
        let expr = Box::new(parse_unary(context)?);
        return Ok(Expr::Unary(op, expr));
    }

    Ok(parse_exponent(context)?)
}

fn parse_exponent(context: &mut Context) -> Result<Expr, CalcError> {
    let left = parse_factorial(context)?;

    if match_token(context, TokenKind::Power) {
        let op = advance(context).kind.clone();
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
        _ => Expr::Literal(advance(context).value.clone()),
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
            let parameter = Expr::Literal(advance(context).value.clone());
            return Ok(Expr::FnCall(identifier.value, vec![parameter]));
        }
    }

    // Eg. sqrt(64)
    if match_token(context, TokenKind::OpenParenthesis) {
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
    if context.symbol_table.contains_var(&identifier.value) {
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
    if match_token(context, kind.clone()) {
        return Ok(advance(context));
    }

    Err(CalcError::UnexpectedToken(kind))
}

fn is_at_end(context: &Context) -> bool {
    context.pos >= context.tokens.len() || peek(context).kind == TokenKind::EOF
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Token, TokenKind::*};
    use crate::test_helpers::*;

    fn parse_with_context(context: &mut Context, tokens: Vec<Token>) -> Result<Stmt, CalcError> {
        context.tokens = tokens;

        parse_stmt(context)
    }

    fn parse(tokens: Vec<Token>) -> Result<Stmt, CalcError> {
        let mut context = Context::new();
        context.tokens = tokens;

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
        ];

        assert_eq!(
            parse(tokens).unwrap(),
            Stmt::Expr(binary(
                literal("1"),
                Plus,
                binary(
                    literal("2"),
                    Star,
                    group(binary(
                        literal("3"),
                        Minus,
                        binary(literal("4"), Slash, literal("5"))
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
        ];

        assert_eq!(
            parse(tokens).unwrap(),
            Stmt::Expr(binary(
                binary(
                    literal("1"),
                    Star,
                    binary(
                        literal("2"),
                        Power,
                        binary(literal("3"), Power, literal("4")),
                    ),
                ),
                Plus,
                literal("5")
            )),
        );
    }

    /*#[test_case(Deg)]
    #[test_case(Rad)]
    fn test_unary(angle_unit: TokenKind) {
        let tokens = vec![
            token(Minus, ""),
            token(Literal, "1"),
            token(angle_unit.clone(), ""),
        ];

        assert_eq!(
            parse(tokens).unwrap(),
            Stmt::Expr(unary(Minus, Box::new(Expr::Unit(literal("1"), angle_unit))))
        );
    }*/

    #[test]
    fn test_var_decl() {
        let tokens = vec![
            token(Identifier, "x"),
            token(Equals, ""),
            token(Literal, "1"),
            token(Plus, ""),
            token(Literal, "2"),
        ];

        assert_eq!(
            parse(tokens).unwrap(),
            Stmt::VarDecl(String::from("x"), binary(literal("1"), Plus, literal("2")))
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
        ];

        assert_eq!(
            parse(tokens).unwrap(),
            Stmt::FnDecl(
                String::from("f"),
                vec![String::from("x")],
                binary(literal("1"), Plus, literal("2"))
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
        ];

        let mut context = Context::new();

        // Add the function to the symbol table first, in order to prevent errors.
        context.symbol_table.set(Stmt::FnDecl(
            String::from("f"),
            vec![String::from("x")],
            literal("1"),
        ));

        assert_eq!(
            parse_with_context(&mut context, tokens).unwrap(),
            Stmt::Expr(binary(
                Box::new(Expr::FnCall(
                    String::from("f"),
                    vec![*binary(literal("1"), Plus, literal("2"))]
                )),
                Plus,
                literal("3")
            ))
        );
    }
}
