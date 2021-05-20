use crate::DEFAULT_PRECISION;
use ansi_term::Colour::Red;
use kalk::parser;

pub fn eval(parser: &mut parser::Context, input: &str, precision: u32) {
    match parser::eval(parser, input, precision) {
        Ok(Some(result)) => println!("{}", result.to_string_pretty()),
        Ok(None) => print!(""),
        Err(err) => print_err(&err.to_string()),
    }
}

pub fn print_err(msg: &str) {
    Red.paint(msg).to_string();
    println!("{}", msg);
}
