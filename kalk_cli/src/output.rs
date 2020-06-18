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
                let comma_pos = if use_sci_notation { 1 } else { exp as usize };
                let sign = if result >= 0 { "" } else { "-" };

                let num = if exp <= 0 {
                    // 0 < x < 1
                    round(
                        format!("0.{}{}", "0".repeat(exp.abs() as usize), digits)
                            .trim_end_matches('0'),
                    )
                    .to_string()
                } else if use_sci_notation || result.fract() != 0 {
                    // Insert the comma if there are supposed to be decimals.
                    let mut chars: Vec<char> = digits
                        .trim_end_matches('0')
                        .trim_end_matches('.')
                        .chars()
                        .collect();
                    chars.insert(comma_pos, '.');
                    round(&chars.into_iter().collect::<String>()).to_string()
                } else {
                    // Regular number
                    digits[..(exp as usize)].to_string()
                };

                if use_sci_notation {
                    println!("{}{}*10^{} {}", sign, num, exp - 1, unit);
                } else {
                    println!("{}{} {}", sign, num, unit);
                }
            }
        }
        Ok(None) => print!(""),
        Err(err) => print_calc_err(err),
    }
}

pub fn print_err(msg: &str) {
    println!("{}", Red.paint(msg));
}

fn round(value: &str) -> String {
    let mut value_iter = value.chars().into_iter().rev().skip(1);
    let last_char = value_iter.next().unwrap();
    let mut last_char_count = 1;

    // Find out how many repeating trailing (equal) characters there are
    for c in value_iter {
        if c == last_char {
            last_char_count += 1;
        } else {
            break;
        }
    }

    // Don't round if there aren't that many
    if last_char_count < 5 {
        return value.to_string();
    }

    // Remove zeroes.
    // If it's nines, round it up.
    // Otherwise, only show 3 of them and then three dots:
    match last_char {
        '0' => value[..value.len() - last_char_count - 1].to_string(),
        '9' => value.parse::<f32>().unwrap().ceil().to_string(),
        _ => format!(
            "{}{}...",
            &value[..value.len() - last_char_count - 1],
            last_char.to_string().repeat(3)
        ),
    }
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
        Unknown => format!("Unknown error."),
    });
}
