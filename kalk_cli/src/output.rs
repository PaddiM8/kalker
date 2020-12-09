use ansi_term::Colour::Red;
use kalk::parser::{self, CalcError, CalcError::*};

pub fn eval(parser: &mut parser::Context, input: &str) {
    match parser::eval(parser, input, 53) {
        Ok(Some((result, unit))) => {
            let (_, digits, exp_option) = result.to_sign_string_exp(10, None);
            let exp = if let Some(exp) = exp_option { exp } else { 0 };

            if result.is_infinite() {
                print_err("Too big to process.");
            } else {
                let use_sci_notation = exp > 8 || exp < -6;

                if !use_sci_notation {
                    println!("{}", round_value(result));
                    return;
                }

                let comma_pos = if use_sci_notation { 1 } else { exp as usize };
                let sign = if result >= 0 { "" } else { "-" };

                let num = if exp <= 0 {
                    // 0 < x < 1
                    format!("0.{}{}", "0".repeat(exp.abs() as usize), digits)
                        .trim_end_matches('0')
                        .to_string()
                } else {
                    // Insert the comma if there are supposed to be decimals.
                    let mut chars: Vec<char> = digits
                        .trim_end_matches('0')
                        .trim_end_matches('.')
                        .chars()
                        .collect();
                    chars.insert(comma_pos, '.');
                    chars.into_iter().collect::<String>()
                };

                println!("{}{}*10^{} {}", sign, num, exp - 1, unit);
            }
        }
        Ok(None) => print!(""),
        Err(err) => print_calc_err(err),
    }
}

pub fn print_err(msg: &str) {
    println!("{}", Red.paint(msg));
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

fn round_value(value: rug::Float) -> String {
    format!("{:.10}", value.to_f64_round(rug::float::Round::Down))
        .trim_end_matches('0')
        .trim_end_matches('.')
        .to_string()
}
