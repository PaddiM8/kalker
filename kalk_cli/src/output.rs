use crate::DEFAULT_PRECISION;
use ansi_term::Colour::Red;
use kalk::parser;

pub fn eval(parser: &mut parser::Context, input: &str, precision: u32) {
    match parser::eval(parser, input, precision) {
        Ok(Some(result)) => {
            let sci_notation = result.to_scientific_notation();
            let result_str = if sci_notation.exponent > 8 || sci_notation.exponent < -6 {
                sci_notation.to_string()
            } else if precision == DEFAULT_PRECISION {
                result.to_string()
            } else {
                result.to_string_big()
            };

            let unit = result.get_unit();
            if let Some(estimate) = result.estimate() {
                if unit == "" {
                    println!("{} ≈ {}", result_str, estimate);
                } else {
                    println!("{} {} ≈ {}", result_str, unit, estimate);
                }
            } else {
                println!("{} {}", result_str, unit);
            }
        }
        Ok(None) => print!(""),
        Err(err) => print_err(&err.to_string()),
    }
}

pub fn print_err(msg: &str) {
    Red.paint(msg).to_string();
    println!("{}", msg);
}
