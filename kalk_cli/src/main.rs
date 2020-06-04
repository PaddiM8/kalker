mod output;
mod repl;

use kalk::parser::Unit;
use kalk::parser::{self};
use std::env;

fn main() {
    let mut parser = parser::Context::new().set_angle_unit(get_angle_unit());

    // Command line argument input, execute it and exit.
    if let Some(expr) = env::args().skip(1).next() {
        output::eval(&mut parser, &expr);
        return;
    }

    // REPL
    repl::start(&mut parser);
}

fn get_angle_unit() -> Unit {
    if let Ok(angle_unit_var) = env::var("ANGLE_UNIT") {
        match angle_unit_var.as_ref() {
            "radians" => Unit::Radians,
            "degrees" => Unit::Degrees,
            _ => {
                panic!("Unexpected angle unit: {}.", angle_unit_var);
            }
        }
    } else {
        Unit::Radians
    }
}
