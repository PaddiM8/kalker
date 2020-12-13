mod output;
mod repl;

use kalk::parser;
use std::env;
use std::fs::File;
use std::io::Read;

static DEFAULT_PRECISION: u32 = 53;

fn main() {
    let mut parser_context = parser::Context::new().set_angle_unit(&get_angle_unit());

    // Command line argument input, execute it and exit.
    let mut args = env::args().skip(1);
    let mut expr_input: Option<String> = None;
    let mut precision = DEFAULT_PRECISION;
    loop {
        // Get the next argument if possible, otherwise break the loop.
        let arg = if let Some(arg) = args.next() {
            arg
        } else {
            break;
        };

        match arg.as_ref() {
            "-h" | "--help" => {
                // The indentation... Will have to do something more scalable in the future.
                println!(
                    "
[kalk help]

kalk [OPTIONS] [INPUT]
-h, --help  : show this
-i          : load a file with predefined functions/variables
--precision : specify number precision

[Environment variables]
ANGLE_UNIT=(deg/rad) : Sets the default unit used for trigonometric functions.
                "
                );
                return;
            }
            "-i" => {
                let file_name = &args.next().expect("Expected input file."); // The next argument will be the file name.
                let mut file_content = String::new();
                File::open(&file_name)
                    .expect("Couldn't find file.")
                    .read_to_string(&mut file_content)
                    .expect("Failed to read input file.");

                // Parse the input file content, resulting in the symbol table being filled out.
                // Output is not needed here.
                parser::eval(&mut parser_context, &file_content, precision)
                    .expect("Failed to parse input file.");
            }
            "--precision" => {
                precision = args
                    .next()
                    .expect("Expected precision input.")
                    .parse::<u32>()
                    .expect("Precision value could not be parsed.");
            }
            _ => {
                // Main argument. This is expected to be a maths expression.
                // After the loop is finished, this will be parsed and outputted.
                expr_input = Some(arg);
            }
        }
    }

    if let Some(input) = expr_input {
        // Direct output
        output::eval(&mut parser_context, &input, precision);
    } else {
        // REPL
        repl::start(&mut parser_context, precision);
    }
}

fn get_angle_unit() -> String {
    if let Ok(angle_unit_var) = env::var("ANGLE_UNIT") {
        angle_unit_var
    } else {
        String::from("rad")
    }
}
