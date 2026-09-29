use ansi_term::Colour::Red;
use kalk::{kalk_value::ScientificNotationFormat, parser};
use std::io::Write;

pub(crate) const DEFAULT_PRECISION: u32 = 1024;

pub fn eval(
    parser: &mut parser::Context,
    input: &str,
    precision: u32,
    base: u8,
    format: ScientificNotationFormat,
    no_leading_equal: bool,
    raw: bool,
) {
    match parser::eval(parser, input, precision) {
        Ok(Some(mut result)) => {
            if base != 10 && !result.set_radix(base) {
                print_err("Invalid base. Change it by typing eg. `base 10`.");

                return;
            }

            if precision == DEFAULT_PRECISION && !raw {
                let mut result_str = result.to_string_pretty_format(format);
                if no_leading_equal {
                    result_str = result_str
                        .trim_start_matches('=')
                        .trim_start_matches('≈')
                        .trim_start()
                        .to_string();
                }

                println_stdout(&result_str);

                return;
            }

            println_stdout(&result.to_string_big())
        }
        Ok(None) => {}
        Err(err) => print_err(&err.to_string()),
    }
}

/// Write a string to stdout. If the output pipe has been closed (eg.
/// `kalker 1+1 | head -n1`), exit quietly instead of panicking like
/// `print!`/`println!` would.
pub(crate) fn print_stdout(msg: &str) {
    if let Err(err) = write!(std::io::stdout(), "{}", msg) {
        handle_stdout_error(err);
    }
}

/// Write a string to stdout, followed by a newline. If the output pipe
/// has been closed, exit quietly instead of panicking.
pub(crate) fn println_stdout(msg: &str) {
    if let Err(err) = writeln!(std::io::stdout(), "{}", msg) {
        handle_stdout_error(err);
    }
}

fn handle_stdout_error(err: std::io::Error) {
    if err.kind() == std::io::ErrorKind::BrokenPipe {
        // The reader closed the pipe; there is nothing more to do.
        std::process::exit(0);
    }
}

pub fn print_err(msg: &str) {
    Red.paint(msg).to_string();
    // If stderr has been closed there is nowhere to report the error anyway.
    let _ = writeln!(std::io::stderr(), "{}", msg);
}
