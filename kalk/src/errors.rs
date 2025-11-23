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

impl std::fmt::Display for KalkError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            KalkError::CannotIndexByImaginary => write!(f, "Cannot index by imaginary numbers."),
            KalkError::CanOnlyIndexX => write!(f, "Indexing (getting an item with a specific index) is only possible on vectors and matrices."),
            KalkError::Expected(description) => write!(f, "Expected: {}", description),
            KalkError::ExpectedDx => write!(f, "Expected eg. dx, to specify for which variable the operation is being done to. Example with integration: ∫(0, 1, x dx) or ∫(0, 1, x, dx). You may need to put parenthesis around the expression before dx/dy/du/etc."),
            KalkError::ExpectedIf => write!(f, "Expected 'if', with a condition after it."),
            KalkError::ExpectedReal => write!(f, "Expected a real value but got imaginary."),
            KalkError::IncompatibleTypesForOperation(operation, got1, got2) => write!(f, "Incompatible types for operation '{}': {} and {}.", operation, got1, got2),
            KalkError::IncompatibleVectorsMatrixes => write!(f, "Incompatible vectors/matrixes."),
            KalkError::IncorrectAmountOfArguments(expected, func, got) => write!(
                f,
                "Expected {} arguments for function {}, but got {}.",
                expected, func, got
            ),
            KalkError::IncorrectAmountOfIndexes(expected,  got) => write!(
                f,
                "Expected {} indexes but got {}.",
                expected, got
            ),
            KalkError::ItemOfIndexDoesNotExist(indices) => write!(f, "Item of index ⟦{}⟧ does not exist.", indices.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", ")),
            KalkError::InconsistentColumnWidths => write!(f, "Inconsistent column widths. Matrix columns must be the same size."),
            KalkError::InvalidBase => write!(f, "Invalid base."),
            KalkError::InvalidComprehension(x) => write!(f, "Invalid comprehension: {}", x),
            KalkError::InvalidNumberLiteral(x) => write!(f, "Invalid number literal: '{}'.", x),
            KalkError::InvalidOperator => write!(f, "Invalid operator."),
            KalkError::InvalidUnit => write!(f, "Invalid unit."),
            KalkError::StackOverflow => write!(f, "Operation recursed too deeply."),
            KalkError::TimedOut => write!(f, "Operation took too long."),
            KalkError::VariableReferencesItself => write!(f, "Variable references itself."),
            KalkError::PiecewiseConditionsAreFalse => write!(f, "All the conditions in the piecewise are false."),
            KalkError::EvaluationError(msg) => write!(f, "Evaluation error: {}", msg),
            KalkError::UnexpectedToken(got, expected) => {
                if let Some(expected) = expected {
                    write!(f, "Unexpected token: '{:?}', expected '{:?}'.", got, expected)
                } else {
                    write!(f, "Unexpected token: '{:?}'.", got)
                }
            }
            KalkError::UnexpectedType(got, expected) => {
                write!(f, "Unexpected type. Got {:?} but expected: {:?}.", got, expected.join(", "))
            }
            KalkError::UnableToInvert(msg) => write!(f, "Unable to invert: {}", msg),
            KalkError::UndefinedFn(name) => write!(f, "Undefined function: '{}'.", name),
            KalkError::UndefinedVar(name) => write!(f, "Undefined variable: '{}'.", name),
            KalkError::UnableToParseExpression => write!(f, "Unable to parse expression."),
            KalkError::UnableToSolveEquation => write!(f, "Unable to solve equation."),
            KalkError::UnableToOverrideConstant(name) => write!(f, "Unable to override constant: '{}'.", name),
            KalkError::UnrecognizedBase => write!(f, "Unrecognized base."),
            KalkError::Unknown | KalkError::WasStmt(_) => write!(f, "Unknown error."),
        }
    }
}
