mod output;
mod repl;

use kalk::parser;
use kalk::parser::Unit;
use std::env;
use std::fs::File;
use std::io::Read;

fn main() {
    let mut parser_context = parser::Context::new().set_angle_unit(get_angle_unit());

    // Command line argument input, execute it and exit.
    let mut args = env::args().skip(1);
    let mut expr_input: Option<String> = None;
    loop {
        // Get the next argument if possible, otherwise break the loop.
        let arg = if let Some(arg) = args.next() {
            arg
        } else {
            break;
        };

        if arg == "-i" {
            let file_name = &args.next().expect("Expected input file."); // The next argument will be the file name.
            let mut file_content = String::new();
            File::open(&file_name)
                .expect("Couldn't find file.")
                .read_to_string(&mut file_content)
                .expect("Failed to read input file.");

            // Parse the input file content, resulting in the symbol table being filled out.
            // Output is not needed here.
            parser::eval(&mut parser_context, &file_content, 53)
                .expect("Failed to parse input file.");
        } else {
            // Main argument. This is expected to be a maths expression.
            // After the loop is finished, this will be parsed and outputted.
            expr_input = Some(arg);
        }
    }

    if let Some(input) = expr_input {
        // Direct output
        output::eval(&mut parser_context, &input);
    } else {
        // REPL
        repl::start(&mut parser_context);
    }
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
