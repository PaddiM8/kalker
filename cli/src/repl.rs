use crate::output;
use crate::output::print_err;
use ansi_term::Colour::{self, Cyan};
use kalk::kalk_value::ScientificNotationFormat;
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

struct Context {
    base: u8,
    mode: ScientificNotationFormat,
}

pub fn start(
    parser: &mut parser::Context,
    precision: u32,
    format: ScientificNotationFormat,
    no_leading_equal: bool
) {
    let mut editor = Editor::<RLHelper>::new();
    editor.set_helper(Some(RLHelper {
        highlighter: LineHighlighter {},
        validator: MatchingBracketValidator::new(),
    }));
    editor.set_max_history_size(30);

    // Load history
    let mut history_path = None;
    if let Some(cache_path) = dirs::cache_dir() {
        let mut cache_path = cache_path;
        cache_path.push("kalker");
        if fs::create_dir_all(cache_path.as_path()).is_ok() {
            cache_path.push("history.txt");
            let history = cache_path.into_os_string().into_string().unwrap();
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

    let mut repl = Context {
        base: 10u8,
        mode: format,
    };

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
                eval_repl(&mut repl, parser, &input, precision, no_leading_equal);
            }
            Err(ReadlineError::Interrupted) => break,
            _ => break,
        }
    }

    if let Some(history_path) = history_path {
        editor.save_history(&history_path).ok();
    }
}

fn eval_repl(
    repl: &mut self::Context,
    parser: &mut parser::Context,
    input: &str,
    precision: u32,
    no_leading_equal: bool
) {
    if let Some(file_name) = input.strip_prefix("load ") {
        if let Some(file_path) = crate::get_input_file_by_name(file_name) {
            crate::load_input_file(&file_path, precision, parser);
        } else {
            eprintln!("Unable to find '{}'", file_name);
        }

        return;
    }

    if let Some(base_str) = input.strip_prefix("base ") {
        if !base_str.is_empty() && base_str.chars().next().unwrap().is_ascii_digit() {
            if let Ok(base) = base_str.parse::<u8>() {
                if base > 1 && base < 50 {
                    repl.base = base;
                    return;
                }
            }

            eprintln!("Invalid number base");

            return;
        }
    }

    if input.starts_with("mode") {
        let mut parts = input.split(' ');
        let mode = match parts.nth(1) {
            Some("normal") => ScientificNotationFormat::Normal,
            Some("eng") => ScientificNotationFormat::Engineering,
            _ => {
                print_err("Invalid mode name. Available modes: normal, eng");

                return;
            },
        };

        repl.mode = mode;

        return;
    }

    match input {
        "" => eprint!(""),
        "clear" => print!("\x1B[2J"),
        "exit" => process::exit(0),
        "help" => print_cli_help(),
        _ => output::eval(parser, input, precision, repl.base, repl.mode, no_leading_equal),
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
            (?P<op>([+\-/*%^!×÷⋅∧∨¬ᵀ]|if|otherwise|\b(and|or|mod|true|false|not)\b|load|exit|clear|help)) |
            (?P<radix>0[box][a-zA-Z0-9]+) |
            (?P<identifier>[^!-@\s_|^⌊⌋⌈⌉\[\]\{\}⟦⟧≠≥≤⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎ᵀ]+(_\d+)?)",
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
        m.insert("[[", "⟦⟧");
        m.insert("!=", "≠");
        m.insert(">=", "≥");
        m.insert("<=", "≤");
        m.insert(" and", " ∧");
        m.insert(" or", " ∨");
        m.insert(" not", " ¬");
        m.insert("*", "×");
        m.insert("/", "÷");
        m.insert("^T", "ᵀ");
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

            // If the key starts with a space, it should also be expanded
            // if it is at the start of the line. To do this, the strings
            // are compared with the space removed in these situations.
            if key.starts_with(' ') && slice.len() == key.len() - 1 && slice == &key[1..] {
                let value = &(*COMPLETION_FUNCS.get(key).unwrap())[1..];
                return Ok((pos - (key.len() - 1), vec![value.to_string()]));
            }

            let mut subscript_digits = String::new();
            for c in slice.chars().rev() {
                if c.is_ascii_digit() {
                    subscript_digits.insert(0, c);
                } else {
                    break;
                }
            }

            let subscript_char_count = subscript_digits.chars().count();
            if subscript_char_count > 0 && pos - subscript_char_count > 0 {
                let value = kalk::text_utils::normal_to_subscript(subscript_digits.chars());
                return Ok((pos - subscript_char_count - 1, vec![value]));
            }
        }

        Ok((0, vec![line.to_string()]))
    }

    fn update(&self, line: &mut rustyline::line_buffer::LineBuffer, start: usize, elected: &str) {
        line.backspace(line.pos() - start);
        line.insert_str(line.pos(), elected);
        line.move_forward(if elected.ends_with(')') || elected.ends_with('⟧') {
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
        !line.is_empty()
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
