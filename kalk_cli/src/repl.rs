use crate::output;
use ansi_term::Colour::{self, Cyan};
use kalk::parser;
use regex::Captures;
use regex::Regex;
use rustyline::completion::Completer;
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
use std::process;

pub fn start(mut parser: &mut parser::Context) {
    let mut editor = Editor::<RLHelper>::new();
    editor.set_helper(Some(RLHelper {
        highlighter: LineHighlighter {},
        validator: MatchingBracketValidator::new(),
    }));

    loop {
        let readline = editor.readline(&Cyan.paint(">> ").to_string());

        match readline {
            Ok(input) => {
                editor.add_history_entry(input.as_str());
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

struct LineHighlighter {}

impl Highlighter for LineHighlighter {
    fn highlight<'l>(&self, line: &'l str, _: usize) -> Cow<'l, str> {
        let mut coloured = line.to_string();

        let reg = Regex::new(
            r"(?x)
            (?P<identifier>[^!-@\s_|^⌊⌋⌈⌉]+(_\d+)?) |
            (?P<op>[+\-/*^!])",
        )
        .unwrap();

        coloured = reg
            .replace_all(&coloured, |caps: &Captures| {
                if let Some(cap) = caps.name("identifier") {
                    match cap.as_str() {
                        "rad" | "deg" | "°" => Colour::Yellow.paint(cap.as_str()).to_string(),
                        _ => Colour::Blue.paint(cap.as_str()).to_string(),
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

const COMPLETION_FUNCS: phf::Map<&'static str, &'static str> = phf::phf_map! {
    "ceil" => "⌈⌉",
    "deg" => "°",
    "floor" => "⌊⌋",
    "gamma" => "Γ",
    "sum" => "Σ()",
    "sqrt" => "√",
    "(" => "()",
};

impl Completer for RLHelper {
    type Candidate = String;
    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &rustyline::Context<'_>,
    ) -> Result<(usize, Vec<Self::Candidate>), ReadlineError> {
        for key in COMPLETION_FUNCS.keys() {
            if line[..pos].ends_with(key) {
                let value = *COMPLETION_FUNCS.get(key).unwrap();
                return Ok((pos - key.len(), vec![value.to_string()]));
            }
        }

        Ok((0, vec![line.to_string()]))
    }

    fn update(&self, line: &mut rustyline::line_buffer::LineBuffer, start: usize, elected: &str) {
        line.backspace(line.pos() - start);
        line.insert_str(line.pos(), elected);
        line.move_forward(match elected {
            "Σ()" => 2,
            _ => 1,
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
    fn hint(&self, _: &str, _: usize, _: &rustyline::Context) -> Option<String> {
        None
    }
}

impl Validator for RLHelper {
    fn validate(&self, ctx: &mut ValidationContext) -> Result<ValidationResult, ReadlineError> {
        self.validator.validate(ctx)
    }

    fn validate_while_typing(&self) -> bool {
        self.validator.validate_while_typing()
    }
}
