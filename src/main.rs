use std::env;
use std::io;

mod interpreter;
mod lexer;
mod math_parser;
mod parser;
mod prelude;
mod visitor;
use math_parser::MathParser;
use parser::Unit;

#[allow(unused_assignments)] // The compiler gives a warning that is not valid.
fn main() {
    let angle_unit = if let Ok(angle_unit_var) = env::var("ANGLE_UNIT") {
        match angle_unit_var.as_ref() {
            "radians" => Unit::Radians,
            "degrees" => Unit::Degrees,
            _ => {
                println!("Unexpected angle unit: {}.", angle_unit_var);
                return;
            }
        }
    } else {
        Unit::Radians
    };

    let mut math_parser = MathParser::new();
    math_parser.set_angle_unit(angle_unit);

    if let Some(expr) = env::args().skip(1).next() {
        if let Some(result) = math_parser.parse(&expr) {
            println!("{}", result);
        }
    } else {
        let mut history: Vec<String> = Vec::new();
        let mut input = String::new();

        loop {
            eprint!(">>> ");

            input = String::new();
            io::stdin().read_line(&mut input).unwrap();
            history.push(input.clone());

            if let Some(result) = math_parser.parse(&input) {
                println!("{}", result);
            }
        }
    }
}
