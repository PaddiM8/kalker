mod output;
mod repl;

use kalk::kalk_value::ScientificNotationFormat;
use kalk::parser;
use seahorse::{App, Context, Flag, FlagType};
use std::env;
use std::fs::File;
use std::io::Read;

fn main() {
    let args: Vec<String> = env::args().collect();
    let app = App::new("kalker")
        .author(env!("CARGO_PKG_AUTHORS"))
        .version(env!("CARGO_PKG_VERSION"))
        .usage("kalker [options] [input]")
        .action(default_action)
        .flag(
            Flag::new("input-file", FlagType::String)
                .description("Load a file with predefined variables and functions. End lines with a semicolon.")
                .alias("i"),
        )
        .flag(
            Flag::new("precision", FlagType::Int)
                .description("Specify number precision")
                .alias("p"),
        )
        .flag(
            Flag::new("eng", FlagType::Bool)
                .description("Engineering mode. Modes can also be switched between by typing `mode [normal|eng]`.")
        )
        .flag(
            Flag::new("angle-unit", FlagType::String)
                .description("Unit used for angles, either rad or deg. This can also be specified using an environment variable with the name 'ANGLE_UNIT'.")
                .alias("a"),
        )
        .flag(
            Flag::new("max-recursion-depth", FlagType::Int)
                .description("The maximum allowed recursion depth. This is used to avoid crashes."),
        )
        .flag(
            Flag::new("no-leading-eq", FlagType::Bool)
                .description("Don't include an equal sign at the start of results")
        );

    app.run(args);
}

fn default_action(context: &Context) {
    #[cfg(windows)]
    ansi_term::enable_ansi_support().unwrap_or_default();

    let angle_unit = if let Ok(angle_unit) = context.string_flag("angle-unit") {
        match angle_unit.as_ref() {
            "rad" | "deg" => angle_unit,
            _ => {
                output::print_err("Invalid angle unit. Expected 'rad' or 'deg'.");
                std::process::exit(1);
            }
        }
    } else {
        get_env_angle_unit()
    };
    let mut parser_context = parser::Context::new()
        .set_angle_unit(&angle_unit)
        .set_timeout(None);
    let precision = context
        .int_flag("precision")
        .unwrap_or(output::DEFAULT_PRECISION as isize) as u32;
    let format = if context.bool_flag("eng") {
        ScientificNotationFormat::Engineering
    } else {
        ScientificNotationFormat::Normal
    };

    if let Ok(max_recursion_depth) = context.int_flag("max-recursion-depth") {
        parser_context = parser_context.set_max_recursion_depth(max_recursion_depth as u32);
    }

    if let Some(input_file_path) = get_input_file_by_name("default") {
        load_input_file(&input_file_path, precision, &mut parser_context);
    }

    if let Ok(input_file_path) = context.string_flag("input-file") {
        load_input_file(&input_file_path, precision, &mut parser_context);
    }

    if context.args.is_empty() {
        // REPL
        repl::start(
            &mut parser_context,
            precision,
            format,
            context.bool_flag("no-leading-eq")
        );
    } else {
        // Direct output
        output::eval(
            &mut parser_context,
            &context.args.join(" "),
            precision,
            10u8,
            format,
            context.bool_flag("no-leading-eq")
        );
    }
}

pub(crate) fn get_input_file_by_name(name: &str) -> Option<String> {
    let mut path = dirs::config_dir()?;
    path.push("kalker");
    path.push(name);
    path.set_extension("kalker");

    if path.exists() {
        Some(path.to_str()?.to_string())
    } else {
        None
    }
}

pub fn load_input_file(file_name: &str, precision: u32, parser_context: &mut parser::Context) {
    let mut file_content = String::new();
    File::open(file_name)
        .expect("Couldn't find file.")
        .read_to_string(&mut file_content)
        .expect("Failed to read input file.");

    // Parse the input file content, resulting in the symbol table being filled out.
    // Output is not needed here.
    if let Err(error) = parser::eval(parser_context, &file_content, precision) {
        eprintln!("{}", error.to_string());
    }
}

fn get_env_angle_unit() -> String {
    if let Ok(angle_unit_var) = env::var("ANGLE_UNIT") {
        angle_unit_var
    } else {
        String::from("rad")
    }
}
