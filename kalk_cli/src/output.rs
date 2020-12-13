use ansi_term::Colour::Red;
use kalk::parser::{self, CalcError, CalcError::*};

pub fn eval(parser: &mut parser::Context, input: &str) {
    match parser::eval(parser, input, 53) {
        Ok(Some(result)) => {
            let sci_notation = result.to_scientific_notation();
            let result_str = if sci_notation.exponent > 8 || sci_notation.exponent < -6 {
                sci_notation.to_string()
            } else {
                result.to_string()
            };

            println!("{} {}", result_str, result.get_unit());

            if result_str.contains(".") {
                if let Some((numer, denom)) = result.get_rational() {
                    println!("{}/{}", numer, denom);
                }
            }
        }
        Ok(None) => print!(""),
        Err(err) => print_calc_err(err),
    }
}

pub fn print_err(msg: &str) {
    let msg = if cfg!(windows) {
        msg.to_string()
    } else {
        Red.paint(msg).to_string()
    };
    println!("{}", msg);
}

fn print_calc_err(err: CalcError) {
    print_err(&match err {
        IncorrectAmountOfArguments(expected, func, got) => format!(
            "Expected {} arguments for function {}, but got {}.",
            expected, func, got
        ),
        InvalidNumberLiteral(x) => format!("Invalid number literal: '{}'.", x),
        InvalidOperator => format!("Invalid operator."),
        InvalidUnit => format!("Invalid unit."),
        UnexpectedToken(kind) => format!("Unexpected token: '{:?}'.", kind),
        UnableToInvert(msg) => format!("Unable to invert: {}", msg),
        UndefinedFn(name) => format!("Undefined function: '{}'.", name),
        UndefinedVar(name) => format!("Undefined variable: '{}'.", name),
        UnableToParseExpression => format!("Unable to parse expression."),
        Unknown => format!("Unknown error."),
    });
}
