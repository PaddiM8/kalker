use crate::output;
use ansi_term::Colour::{self, Cyan};
use kalk::parser;
use lazy_static::lazy_static;
use regex::Captures;
use regex::Regex;
use rustyline::completion::Completer;
use rustyline::config::Configurer;
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::MatchingBracketValidator;
use rustyline::validate::ValidationContext;
use rustyline::validate::ValidationResult;
use rustyline::validate::Validator;
use rustyline::{Editor, Helper};
use std::borrow::Cow;
use std::borrow::Cow::Owned;
use std::collections::HashMap;
use std::fs;
use std::process;

pub fn start(mut parser: &mut parser::Context, precision: u32) {
    let mut editor = Editor::<RLHelper>::new();
    editor.set_helper(Some(RLHelper {
        highlighter: LineHighlighter {},
        validator: MatchingBracketValidator::new(),
    }));
    editor.set_max_history_size(30);

    // Load history
    let mut history_path = None;
    if let Some(config_path) = dirs::config_dir() {
        let mut config_path = config_path.clone();
        config_path.push("kalker");
        if let Ok(_) = fs::create_dir_all(config_path.as_path()) {
            config_path.push("history.txt");
            let history = config_path.into_os_string().into_string().unwrap();
            editor.load_history(&history).ok();
            history_path = Some(history)
        }
    }

    // If in tty, print the welcome message
    if atty::is(atty::Stream::Stdin) && atty::is(atty::Stream::Stdout) {
        println!("kalker");
        println!(
            "{}",
            ansi_term::Color::Fixed(246).paint("Type 'help' for instructions.")
        );
    }

    loop {
        let prompt = if cfg!(windows) {
            String::from(">> ")
        } else {
            Cyan.paint(">> ").to_string()
        };
        let readline = editor.readline(&prompt);

        match readline {
            Ok(input) => {
                editor.add_history_entry(input.as_str());
                eval_repl(&mut parser, &input, precision);
            }
            Err(ReadlineError::Interrupted) => break,
            _ => break,
        }
    }

    if let Some(history_path) = history_path {
        editor.save_history(&history_path).ok();
    }
}

fn eval_repl(parser: &mut parser::Context, input: &str, precision: u32) {
    match input {
        "" => eprint!(""),
        "clear" => print!("\x1B[2J"),
        "exit" => process::exit(0),
        "help" => print_cli_help(),
        _ => output::eval(parser, input, precision),
    }
}

fn print_cli_help() {
    let help_text = include_str!("../help.txt");
    println!("{}", help_text);
}

struct LineHighlighter {}

impl Highlighter for LineHighlighter {
    fn highlight<'l>(&self, line: &'l str, _: usize) -> Cow<'l, str> {
        let mut coloured = line.to_string();

        let reg = Regex::new(
            r"(?x)
            (?P<op>([+\-/*%^!×÷]|if|otherwise)) |
            (?P<radix>0[box][a-zA-Z0-9]+) |
            (?P<identifier>[^!-@\s_|^⌊⌋⌈⌉\[\]\{\}≠≥≤⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎]+(_\d+)?)",
        )
        .unwrap();

        coloured = reg
            .replace_all(&coloured, |caps: &Captures| {
                if let Some(cap) = caps.name("identifier") {
                    match cap.as_str() {
                        "rad" | "deg" | "°" => Colour::Yellow.paint(cap.as_str()).to_string(),
                        _ => Colour::Fixed(32).paint(cap.as_str()).to_string(),
                    }
                } else if let Some(cap) = caps.name("op") {
                    Colour::Fixed(172).paint(cap.as_str()).to_string()
                } else {
                    caps[0].to_string()
                }
            })
            .to_string();

        Owned(coloured)
    }
}

struct RLHelper {
    highlighter: LineHighlighter,
    validator: MatchingBracketValidator,
}

impl Helper for RLHelper {}

lazy_static! {
    pub static ref COMPLETION_FUNCS: HashMap<&'static str, &'static str> = {
        let mut m = HashMap::new();
        m.insert("ceil", "⌈⌉");
        m.insert("deg", "°");
        m.insert("floor", "⌊⌋");
        m.insert("gamma", "Γ");
        m.insert("sum", "Σ()");
        m.insert("prod", "∏()");
        m.insert("integrate", "∫()");
        m.insert("integral", "∫()");
        m.insert("phi", "ϕ");
        m.insert("pi", "π");
        m.insert("sqrt", "√");
        m.insert("tau", "τ");
        m.insert("(", "()");
        m.insert("!=", "≠");
        m.insert(">=", "≥");
        m.insert("<=", "≤");
        m.insert("*", "×");
        m.insert("/", "÷");
        m.insert("asin", "sin⁻¹()");
        m.insert("acos", "cos⁻¹()");
        m.insert("atan", "tan⁻¹()");
        m.insert("acot", "cot⁻¹()");
        m.insert("acosec", "cosec⁻¹()");
        m.insert("asec", "sec⁻¹()");
        m.insert("asinh", "sinh⁻¹()");
        m.insert("acosh", "cosh⁻¹()");
        m.insert("atanh", "tanh⁻¹()");
        m.insert("acoth", "coth⁻¹()");
        m.insert("acosech", "cosech⁻¹()");
        m.insert("asech", "sech⁻¹()");
        m.insert("cbrt", "∛");
        m
    };
}

impl Completer for RLHelper {
    type Candidate = String;
    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &rustyline::Context<'_>,
    ) -> Result<(usize, Vec<Self::Candidate>), ReadlineError> {
        for key in COMPLETION_FUNCS.keys() {
            let slice = &line[..pos];
            if slice.ends_with(key) {
                let value = *COMPLETION_FUNCS.get(key).unwrap();
                return Ok((pos - key.len(), vec![value.to_string()]));
            }

            let mut subscript_digits = String::new();
            for c in slice.chars().rev() {
                if c.is_digit(10) {
                    subscript_digits.insert(0, c);
                } else {
                    break;
                }
            }

            if subscript_digits.len() > 0 {
                let value = kalk::text_utils::digits_to_subscript(subscript_digits.chars());
                return Ok((pos - subscript_digits.chars().count() - 1, vec![value]));
            }
        }

        Ok((0, vec![line.to_string()]))
    }

    fn update(&self, line: &mut rustyline::line_buffer::LineBuffer, start: usize, elected: &str) {
        line.backspace(line.pos() - start);
        line.insert_str(line.pos(), elected);
        line.move_forward(if elected.ends_with(")") {
            elected.chars().count() - 1
        } else {
            elected.chars().count()
        });
    }
}

impl Highlighter for RLHelper {
    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Owned(Colour::Fixed(244).paint(hint).to_string())
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_candidate<'c>(
        &self,
        candidate: &'c str,
        _completion: rustyline::CompletionType,
    ) -> Cow<'c, str> {
        self.highlighter.highlight(candidate, 0)
    }

    fn highlight_char(&self, line: &str, _: usize) -> bool {
        line.len() > 0
    }
}

impl Hinter for RLHelper {
    type Hint = String;

    fn hint(&self, _: &str, _: usize, _: &rustyline::Context) -> Option<String> {
        None
    }
}

impl Validator for RLHelper {
    fn validate(&self, ctx: &mut ValidationContext) -> Result<ValidationResult, ReadlineError> {
        let mut group_symbol_count = vec![0i32, 0i32, 0i32];

        for c in ctx.input().chars() {
            match c {
                '⌈' | '⌉' => group_symbol_count[0] += 1,
                '⌊' | '⌋' => group_symbol_count[1] += 1,
                '|' => group_symbol_count[2] += 1,
                _ => (),
            }
        }

        if !group_symbol_count.into_iter().all(|x| x % 2 == 0) {
            Ok(ValidationResult::Incomplete)
        } else {
            self.validator.validate(ctx)
        }
    }

    fn validate_while_typing(&self) -> bool {
        self.validator.validate_while_typing()
    }
}
