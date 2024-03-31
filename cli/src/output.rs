use ansi_term::Colour::Red;
use kalk::{kalk_value::ScientificNotationFormat, parser};

pub(crate) const DEFAULT_PRECISION: u32 = 1024;

pub fn eval(parser: &mut parser::Context, input: &str, precision: u32, base: u8, format: ScientificNotationFormat) {
    match parser::eval(parser, input, precision) {
        Ok(Some(mut result)) => {
            if base != 10 && !result.set_radix(base) {
                print_err("Invalid base. Change it by typing eg. `base 10`.");

                return;
            }

            if precision == DEFAULT_PRECISION {
                println!("{}", result.to_string_pretty_format(format));

                return;
            }

            println!("{}", result.to_string_big())
        }
        Ok(None) => print!(""),
        Err(err) => print_err(&err.to_string()),
    }
}

pub fn print_err(msg: &str) {
    Red.paint(msg).to_string();
    eprintln!("{}", msg);
}
