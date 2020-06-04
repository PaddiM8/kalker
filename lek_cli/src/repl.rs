use crate::output;
use ansi_term::Colour::Cyan;
use lek::parser;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::process;

pub fn start(mut parser: &mut parser::Context) {
    let mut rl = Editor::<()>::new();

    loop {
        let readline = rl.readline(&Cyan.paint(">> ").to_string());

        match readline {
            Ok(input) => {
                rl.add_history_entry(input.as_str());
                eval_repl(&mut parser, &input);
            }
            Err(ReadlineError::Interrupted) => break,
            _ => break,
        }
    }
}

fn eval_repl(parser: &mut parser::Context, input: &str) {
    match input {
        "" => eprint!(""),
        "clear" => print!("\x1B[2J"),
        "exit" => process::exit(0),
        _ => output::eval(parser, input),
    }
}
