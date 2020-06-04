use ansi_term::Colour::Red;
use lek::parser::{self};

pub fn eval(parser: &mut parser::Context, input: &str) {
    match parser::parse(parser, input, 53) {
        Ok(Some(result)) => {
            if result.clone().fract() == 0 {
                println!("{}", result.to_integer().unwrap());
            } else {
                println!("{}", result);
            }
        }
        Ok(None) => print!(""),
        Err(err) => println!("{}", Red.paint(err)),
    }
}
