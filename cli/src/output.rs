use ansi_term::Colour::Red;
use kalk::parser;

pub(crate) const DEFAULT_PRECISION: u32 = 63;

pub fn eval(parser: &mut parser::Context, input: &str, precision: u32, base: u8) {
    match parser::eval(parser, input, precision) {
        Ok(Some(mut result)) => {
            result.set_radix(base);

            if precision == DEFAULT_PRECISION {
                println!("{}", result.to_string_pretty())
            } else {
                println!("{}", result.to_string_big())
            }
        }
        Ok(None) => print!(""),
        Err(err) => print_err(&err.to_string()),
    }
}

pub fn print_err(msg: &str) {
    Red.paint(msg).to_string();
    eprintln!("{}", msg);
}
