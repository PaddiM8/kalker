use crate::lexer::TokenKind;

/// Error that occured during parsing or evaluation.
#[derive(Debug, Clone, PartialEq)]
pub enum KalkError {
    CannotIndexByImaginary,
    CanOnlyIndexX,
    Expected(String),
    ExpectedDx,
    ExpectedIf,
    ExpectedReal,
    IncompatibleTypesForOperation(String, String, String),
    IncompatibleVectorsMatrixes,
    IncorrectAmountOfArguments(usize, String, usize),
    IncorrectAmountOfIndexes(usize, usize),
    ItemOfIndexDoesNotExist(Vec<usize>),
    InconsistentColumnWidths,
    InvalidBase,
    InvalidComprehension(String),
    InvalidNumberLiteral(String),
    InvalidOperator,
    InvalidUnit,
    StackOverflow,
    TimedOut,
    VariableReferencesItself,
    PiecewiseConditionsAreFalse,
    EvaluationError(String),
    UnexpectedToken(TokenKind, Option<TokenKind>),
    UnexpectedType(String, Vec<String>),
    UndefinedFn(String),
    UndefinedVar(String),
    UnableToInvert(String),
    UnableToSolveEquation,
    UnableToOverrideConstant(String),
    UnableToParseExpression,
    UnrecognizedBase,
    Unknown,
    WasStmt(crate::ast::Stmt),
}

impl ToString for KalkError {
    fn to_string(&self) -> String {
        match self {
            KalkError::CannotIndexByImaginary => String::from("Cannot index by imaginary numbers."),
            KalkError::CanOnlyIndexX => String::from("Indexing (getting an item with a specific index) is only possible on vectors and matrices."),
            KalkError::Expected(description) => format!("Expected: {}", description),
            KalkError::ExpectedDx => String::from("Expected eg. dx, to specify for which variable the operation is being done to. Example with integration: ∫(0, 1, x dx) or ∫(0, 1, x, dx). You may need to put parenthesis around the expression before dx/dy/du/etc."),
            KalkError::ExpectedIf => String::from("Expected 'if', with a condition after it."),
            KalkError::ExpectedReal => String::from("Expected a real value but got imaginary."),
            KalkError::IncompatibleTypesForOperation(operation, got1, got2) => format!("Incompatible types for operation '{}': {} and {}.", operation, got1, got2),
            KalkError::IncompatibleVectorsMatrixes => String::from("Incompatible vectors/matrixes."),
            KalkError::IncorrectAmountOfArguments(expected, func, got) => format!(
                "Expected {} arguments for function {}, but got {}.",
                expected, func, got
            ),
            KalkError::IncorrectAmountOfIndexes(expected,  got) => format!(
                "Expected {} indexes but got {}.",
                expected, got
            ),
            KalkError::ItemOfIndexDoesNotExist(indices) => format!("Item of index ⟦{}⟧ does not exist.", indices.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", ")),
            KalkError::InconsistentColumnWidths => String::from("Inconsistent column widths. Matrix columns must be the same size."),
            KalkError::InvalidBase => String::from("Invalid base."),
            KalkError::InvalidComprehension(x) => format!("Invalid comprehension: {}", x),
            KalkError::InvalidNumberLiteral(x) => format!("Invalid number literal: '{}'.", x),
            KalkError::InvalidOperator => String::from("Invalid operator."),
            KalkError::InvalidUnit => String::from("Invalid unit."),
            KalkError::StackOverflow => String::from("Operation recursed too deeply."),
            KalkError::TimedOut => String::from("Operation took too long."),
            KalkError::VariableReferencesItself => String::from("Variable references itself."),
            KalkError::PiecewiseConditionsAreFalse => String::from("All the conditions in the piecewise are false."),
            KalkError::EvaluationError(msg) => format!("Evaluation error: {}", msg),
            KalkError::UnexpectedToken(got, expected) => {
                if let Some(expected) = expected {
                    format!("Unexpected token: '{:?}', expected '{:?}'.", got, expected)
                } else {
                    format!("Unexpected token: '{:?}'.", got)
                }
            }
            KalkError::UnexpectedType(got, expected) => {
                format!("Unexpected type. Got {:?} but expected: {:?}.", got, expected.join(", "))
            }
            KalkError::UnableToInvert(msg) => format!("Unable to invert: {}", msg),
            KalkError::UndefinedFn(name) => format!("Undefined function: '{}'.", name),
            KalkError::UndefinedVar(name) => format!("Undefined variable: '{}'.", name),
            KalkError::UnableToParseExpression => String::from("Unable to parse expression."),
            KalkError::UnableToSolveEquation => String::from("Unable to solve equation."),
            KalkError::UnableToOverrideConstant(name) => format!("Unable to override constant: '{}'.", name),
            KalkError::UnrecognizedBase => String::from("Unrecognized base."),
            KalkError::Unknown | KalkError::WasStmt(_) => String::from("Unknown error."),
        }
    }
}
