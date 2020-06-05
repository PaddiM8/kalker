use ansi_term::Colour::Red;
use kalk::parser::{self};

pub fn eval(parser: &mut parser::Context, input: &str) {
    match parser::parse(parser, input, 53) {
        Ok(Some(result)) => {
            if result.is_infinite() {
                err("Too big to process.");
            } else if result > 100_000_000 || result < -100_000_00 {
                let sign = if result >= 0 { "" } else { "-" };
                let (_, digits, exp) = result.to_sign_string_exp(10, None);
                let value = format!(
                    "{}.{}",
                    digits[0..1].to_string(),
                    digits[1..].to_string().trim_end_matches('0')
                );

                println!("{}{}*10^{}", sign, value, exp.unwrap() - 1);
            } else if result.clone().fract() == 0 {
                println!("{}", result.to_integer().unwrap());
            } else {
                println!(
                    "{}",
                    result
                        .to_string()
                        .trim_end_matches('0')
                        .trim_end_matches('.')
                );
            }
        }
        Ok(None) => print!(""),
        Err(msg) => err(&msg),
    }
}

fn err(msg: &str) {
    println!("{}", Red.paint(msg));
}
