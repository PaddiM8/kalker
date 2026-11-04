use ansi_term::Colour::Red;
use kalk::{kalk_value::ScientificNotationFormat, parser};

pub(crate) const CALCULATION_PRECISION: u32 = 1024;

pub fn eval(
    parser: &mut parser::Context,
    input: &str,
    significant_figures: u32,
    base: u8,
    format: ScientificNotationFormat,
    no_leading_equal: bool,
    raw: bool
) {
    match parser::eval(parser, input, CALCULATION_PRECISION) {
        Ok(Some(mut result)) => {
            if base != 10 && !result.set_radix(base) {
                print_err("Invalid base. Change it by typing eg. `base 10`.");

                return;
            }

            if significant_figures == 0 && !raw {
                let mut result_str = result.to_string_pretty_format(format);
                if no_leading_equal {
                    result_str = result_str
                        .trim_start_matches('=')
                        .trim_start_matches('≈')
                        .trim_start()
                        .to_string();
                }

                println!("{}", result_str);

                return;
            }

            if significant_figures > 0 {
                println!("{}", result.to_string_sigfigs(significant_figures as usize));
            } else {
                println!("{}", result.to_string_big());
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
