use crate::output;
use crate::output::print_err;
use crossterm::event::{Event as CrosstermEvent, KeyCode as CrosstermKeyCode};
use kalk::kalk_value::ScientificNotationFormat;
use kalk::parser;
use lazy_static::lazy_static;
use nu_ansi_term::{Color as NuColor, Style as NuStyle};
use reedline::{
    default_emacs_keybindings, default_vi_insert_keybindings, default_vi_normal_keybindings,
    ColumnarMenu, Completer as ReedlineCompleter, DefaultCompleter, EditCommand, EditMode, Emacs,
    FileBackedHistory, KeyCode, KeyModifiers, Keybindings, MenuBuilder, Prompt, PromptEditMode,
    PromptHistorySearch, PromptHistorySearchStatus, PromptViMode, Reedline, ReedlineEvent,
    ReedlineMenu, Signal, Span, StyledText, Suggestion,
    ValidationResult as ReedlineValidationResult, Validator as ReedlineValidator, Vi,
};
use regex::Regex;
use std::collections::HashMap;
use std::convert::TryFrom;

struct Context {
    base: u8,
    mode: ScientificNotationFormat,
}

pub fn start(
    parser: &mut parser::Context,
    precision: u32,
    format: ScientificNotationFormat,
    no_leading_equal: bool,
    raw: bool,
    vim_mode: bool,
) {
    start_reedline(parser, precision, format, no_leading_equal, raw, vim_mode);
}

struct VimPrompt {
    vim_mode: bool,
}

impl Prompt for VimPrompt {
    fn render_prompt_left(&self) -> std::borrow::Cow<'_, str> {
        "".into()
    }

    fn render_prompt_right(&self) -> std::borrow::Cow<'_, str> {
        "".into()
    }

    fn render_prompt_indicator(&self, edit_mode: PromptEditMode) -> std::borrow::Cow<'_, str> {
        if !self.vim_mode {
            return ">> ".into();
        }
        match edit_mode {
            PromptEditMode::Vi(PromptViMode::Normal) => "[N] >> ".into(),
            PromptEditMode::Vi(PromptViMode::Insert) => "[I] >> ".into(),
            _ => "[I] >> ".into(),
        }
    }

    fn render_prompt_multiline_indicator(&self) -> std::borrow::Cow<'_, str> {
        "... ".into()
    }

    fn render_prompt_history_search_indicator(
        &self,
        search: PromptHistorySearch,
    ) -> std::borrow::Cow<'_, str> {
        let status = match search.status {
            PromptHistorySearchStatus::Passing => "",
            PromptHistorySearchStatus::Failing => "failing ",
        };
        format!("({}reverse-search: {}) ", status, search.term).into()
    }
}

struct VimHighlighter;

impl reedline::Highlighter for VimHighlighter {
    fn highlight(&self, line: &str, _cursor: usize) -> StyledText {
        let regex = Regex::new(
            r"(?x)
            (?P<op>([+\-/*%^!×÷⋅∧∨¬ᵀ]|if|otherwise|\b(and|or|mod|true|false|not)\b|load|exit|clear|help)) |
            (?P<radix>0[box][a-zA-Z0-9]+) |
            (?P<identifier>[^!-@\s_|^⌊⌋⌈⌉\[\]\{\}⟦⟧≠≥≤⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎ᵀ]+(_\d+)?)",
        )
        .expect("valid syntax highlighting regex");
        let mut output = StyledText::new();
        let mut previous_end = 0;

        for captures in regex.captures_iter(line) {
            let matched = captures.get(0).expect("match has a full capture");
            if matched.start() > previous_end {
                output.push((NuStyle::new(), line[previous_end..matched.start()].into()));
            }
            let style = if captures.name("op").is_some() {
                NuStyle::new().fg(NuColor::Fixed(172))
            } else if captures.name("identifier").is_some() {
                match matched.as_str() {
                    "rad" | "deg" | "°" => NuStyle::new().fg(NuColor::Yellow),
                    _ => NuStyle::new().fg(NuColor::Fixed(32)),
                }
            } else {
                NuStyle::new()
            };
            output.push((style, matched.as_str().into()));
            previous_end = matched.end();
        }

        if previous_end < line.len() {
            output.push((NuStyle::new(), line[previous_end..].into()));
        }
        output
    }
}

struct VimCompleter {
    completions: DefaultCompleter,
}

impl ReedlineCompleter for VimCompleter {
    fn complete(&mut self, line: &str, pos: usize) -> Vec<Suggestion> {
        for key in COMPLETION_FUNCS.keys() {
            let slice = &line[..pos];
            if slice.ends_with(key) {
                let value = COMPLETION_FUNCS.get(key).expect("completion key exists");
                return vec![Suggestion {
                    value: (*value).to_string(),
                    span: Span::new(pos - key.len(), pos),
                    ..Suggestion::default()
                }];
            }
            if key.starts_with(' ') && slice.len() == key.len() - 1 && slice == &key[1..] {
                let value = &COMPLETION_FUNCS.get(key).expect("completion key exists")[1..];
                return vec![Suggestion {
                    value: value.to_string(),
                    span: Span::new(pos - key.len() + 1, pos),
                    ..Suggestion::default()
                }];
            }

            let digits: String = slice
                .chars()
                .rev()
                .take_while(char::is_ascii_digit)
                .collect();
            if !digits.is_empty() && pos > digits.len() {
                let subscript = kalk::text_utils::normal_to_subscript(digits.chars().rev());
                return vec![Suggestion {
                    value: subscript,
                    span: Span::new(pos - digits.len(), pos),
                    ..Suggestion::default()
                }];
            }
        }

        self.completions.complete(line, pos)
    }
}

struct KalkerValidator;

impl ReedlineValidator for KalkerValidator {
    fn validate(&self, line: &str) -> ReedlineValidationResult {
        let mut expected = Vec::new();
        let mut in_string = false;
        let mut escaped = false;

        for character in line.chars() {
            if escaped {
                escaped = false;
                continue;
            }
            if character == '\\' && in_string {
                escaped = true;
                continue;
            }
            if character == '"' {
                in_string = !in_string;
                continue;
            }
            if in_string {
                continue;
            }

            match character {
                '(' => expected.push(')'),
                '[' => expected.push(']'),
                '{' => expected.push('}'),
                '⌈' => expected.push('⌉'),
                '⌊' => expected.push('⌋'),
                ')' | ']' | '}' | '⌋' | '⌉' => {
                    if expected.last() == Some(&character) {
                        expected.pop();
                    }
                }
                '|' => {
                    if expected.last() == Some(&'|') {
                        expected.pop();
                    } else {
                        expected.push('|');
                    }
                }
                _ => {}
            }
        }

        if in_string || !expected.is_empty() {
            ReedlineValidationResult::Incomplete
        } else {
            ReedlineValidationResult::Complete
        }
    }
}

fn start_reedline(
    parser: &mut parser::Context,
    precision: u32,
    format: ScientificNotationFormat,
    no_leading_equal: bool,
    raw: bool,
    vim_mode: bool,
) {
    let mut history_path = dirs::cache_dir().map(|mut path| {
        path.push("kalker");
        path.push("history.txt");
        path
    });
    let history = history_path
        .take()
        .and_then(|path| FileBackedHistory::with_file(30, path).ok())
        .or_else(|| FileBackedHistory::new(30).ok())
        .expect("unable to initialize REPL history");

    let mut completer = VimCompleter {
        completions: DefaultCompleter::default(),
    };
    completer.completions.insert(
        COMPLETION_FUNCS
            .keys()
            .map(|key| (*key).to_string())
            .collect(),
    );
    let menu = Box::new(ColumnarMenu::default().with_name("completion_menu"));
    let edit_mode: Box<dyn EditMode> = if vim_mode {
        let mut insert_bindings = default_vi_insert_keybindings();
        insert_bindings.add_binding(
            KeyModifiers::NONE,
            KeyCode::Tab,
            ReedlineEvent::Menu("completion_menu".into()),
        );
        Box::new(ScriptInputMode::vi(insert_bindings))
    } else {
        let mut emacs_bindings = default_emacs_keybindings();
        emacs_bindings.add_binding(
            KeyModifiers::NONE,
            KeyCode::Tab,
            ReedlineEvent::Menu("completion_menu".into()),
        );
        Box::new(ScriptInputMode::emacs(emacs_bindings))
    };

    let mut line_editor = Reedline::create()
        .with_history(Box::new(history))
        .with_completer(Box::new(completer))
        .with_menu(ReedlineMenu::EngineCompleter(menu))
        .with_highlighter(Box::new(VimHighlighter))
        .with_validator(Box::new(KalkerValidator))
        .with_edit_mode(edit_mode);
    let prompt = VimPrompt { vim_mode };

    if atty::is(atty::Stream::Stdin) && atty::is(atty::Stream::Stdout) {
        println!("kalker");
        println!(
            "{}",
            ansi_term::Color::Fixed(246).paint("Type 'help' for instructions.")
        );
    }

    let mut repl = Context {
        base: 10,
        mode: format,
    };

    loop {
        match line_editor.read_line(&prompt) {
            Ok(Signal::Success(input)) => {
                if input == "exit" {
                    break;
                }
                eval_repl(&mut repl, parser, &input, precision, no_leading_equal, raw);
            }
            Ok(Signal::CtrlC) => println!(),
            Ok(Signal::CtrlD) => break,
            Err(error) => {
                print_err(&format!("REPL error: {}", error));
                break;
            }
        }
    }
}

enum WrappedEditMode {
    Emacs(Emacs),
    Vi(Vi),
}

struct ScriptInputMode {
    editor: WrappedEditMode,
    previous_marker: Option<char>,
    active_script: Option<char>,
}

impl ScriptInputMode {
    fn emacs(keybindings: Keybindings) -> Self {
        Self {
            editor: WrappedEditMode::Emacs(Emacs::new(keybindings)),
            previous_marker: None,
            active_script: None,
        }
    }

    fn vi(insert_keybindings: Keybindings) -> Self {
        Self {
            editor: WrappedEditMode::Vi(Vi::new(
                insert_keybindings,
                default_vi_normal_keybindings(),
            )),
            previous_marker: None,
            active_script: None,
        }
    }

    fn is_insert_mode(&self) -> bool {
        match &self.editor {
            WrappedEditMode::Emacs(_) => true,
            WrappedEditMode::Vi(editor) => {
                matches!(editor.edit_mode(), PromptEditMode::Vi(PromptViMode::Insert))
            }
        }
    }
}

impl EditMode for ScriptInputMode {
    fn parse_event(&mut self, event: reedline::ReedlineRawEvent) -> ReedlineEvent {
        let raw_event: CrosstermEvent = event.into();
        let typed_char = match &raw_event {
            CrosstermEvent::Key(key) if self.is_insert_mode() => match key.code {
                CrosstermKeyCode::Char(c)
                    if key.modifiers == KeyModifiers::NONE
                        || key.modifiers == KeyModifiers::SHIFT =>
                {
                    Some(if key.modifiers == KeyModifiers::SHIFT {
                        c.to_ascii_uppercase()
                    } else {
                        c
                    })
                }
                _ => None,
            },
            _ => None,
        };

        let event = reedline::ReedlineRawEvent::try_from(raw_event)
            .expect("raw terminal key events should already be validated");
        let parsed = match &mut self.editor {
            WrappedEditMode::Emacs(editor) => editor.parse_event(event),
            WrappedEditMode::Vi(editor) => editor.parse_event(event),
        };

        let Some(character) = typed_char else {
            self.active_script = None;
            if let Some(marker) = self.previous_marker.take() {
                return match parsed {
                    ReedlineEvent::Edit(mut commands) => {
                        commands.insert(0, EditCommand::InsertChar(marker));
                        ReedlineEvent::Edit(commands)
                    }
                    other => ReedlineEvent::Multiple(vec![
                        ReedlineEvent::Edit(vec![EditCommand::InsertChar(marker)]),
                        other,
                    ]),
                };
            }
            return parsed;
        };

        if character == '^' || character == '_' {
            self.previous_marker = Some(character);
            self.active_script = None;
            return ReedlineEvent::None;
        }

        if let Some(script_type) = self.previous_marker.or(self.active_script) {
            if self.previous_marker.is_some() && matches!(character, ' ' | '\t' | '\n' | '\r') {
                self.previous_marker = None;
                self.active_script = None;
                return parsed;
            }
            let starts_sign =
                self.previous_marker.is_some() && (character == '-' || character == '+');
            if character.is_ascii_digit() || starts_sign {
                let converted = if script_type == '^' {
                    superscript_character(character)
                } else {
                    subscript_character(character)
                };
                self.previous_marker = None;
                self.active_script = Some(script_type);
                return ReedlineEvent::Edit(vec![EditCommand::InsertChar(converted)]);
            }
            if self.previous_marker == Some('^') && character == 'T' {
                self.previous_marker = None;
                self.active_script = None;
                return ReedlineEvent::Edit(vec![EditCommand::InsertChar('ᵀ')]);
            }
            if let Some(marker) = self.previous_marker {
                self.previous_marker = None;
                self.active_script = None;
                return match parsed {
                    ReedlineEvent::Edit(mut commands) => {
                        commands.insert(0, EditCommand::InsertChar(marker));
                        ReedlineEvent::Edit(commands)
                    }
                    other => ReedlineEvent::Multiple(vec![
                        ReedlineEvent::Edit(vec![EditCommand::InsertChar(marker)]),
                        other,
                    ]),
                };
            }
            self.previous_marker = None;
            self.active_script = None;
        }

        self.previous_marker = None;
        if !matches!(character, '0'..='9' | '+' | '-') {
            self.active_script = None;
        }
        parsed
    }

    fn edit_mode(&self) -> PromptEditMode {
        match &self.editor {
            WrappedEditMode::Emacs(editor) => editor.edit_mode(),
            WrappedEditMode::Vi(editor) => editor.edit_mode(),
        }
    }
}

fn superscript_character(digit: char) -> char {
    match digit {
        '0' => '⁰',
        '1' => '¹',
        '2' => '²',
        '3' => '³',
        '4' => '⁴',
        '5' => '⁵',
        '6' => '⁶',
        '7' => '⁷',
        '8' => '⁸',
        '9' => '⁹',
        '-' => '⁻',
        '+' => '⁺',
        _ => digit,
    }
}

fn subscript_character(digit: char) -> char {
    match digit {
        '0' => '₀',
        '1' => '₁',
        '2' => '₂',
        '3' => '₃',
        '4' => '₄',
        '5' => '₅',
        '6' => '₆',
        '7' => '₇',
        '8' => '₈',
        '9' => '₉',
        '-' => '₋',
        '+' => '₊',
        _ => digit,
    }
}

fn eval_repl(
    repl: &mut self::Context,
    parser: &mut parser::Context,
    input: &str,
    precision: u32,
    no_leading_equal: bool,
    raw: bool,
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
            }
        };

        repl.mode = mode;

        return;
    }

    match input {
        "" => eprint!(""),
        "clear" => print!("\x1B[2J"),
        "help" => print_cli_help(),
        _ => output::eval(
            parser,
            input,
            precision,
            repl.base,
            repl.mode,
            no_leading_equal,
            raw,
        ),
    }
}

fn print_cli_help() {
    let help_text = include_str!("../help.txt");
    println!("{}", help_text);
}

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

#[cfg(test)]
mod vim_tests {
    use super::{
        KalkerValidator, Prompt, PromptEditMode, PromptViMode, ReedlineValidationResult,
        ScriptInputMode, VimPrompt,
    };
    use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
    use reedline::{EditCommand, EditMode, ReedlineEvent, ReedlineRawEvent, Validator};
    use std::convert::TryFrom;

    #[test]
    fn vim_prompt_indicator_tracks_editor_mode() {
        let prompt = VimPrompt { vim_mode: true };

        assert_eq!(
            "[N] >> ",
            prompt.render_prompt_indicator(PromptEditMode::Vi(PromptViMode::Normal))
        );
        assert_eq!(
            "[I] >> ",
            prompt.render_prompt_indicator(PromptEditMode::Vi(PromptViMode::Insert))
        );
    }

    #[test]
    fn vim_validator_requests_continuation_for_open_groups_and_strings() {
        let validator = KalkerValidator;

        assert!(matches!(
            validator.validate("(1 + 2"),
            ReedlineValidationResult::Incomplete
        ));
        assert!(matches!(
            validator.validate("\"unfinished"),
            ReedlineValidationResult::Incomplete
        ));
        assert!(matches!(
            validator.validate("(1 + 2)"),
            ReedlineValidationResult::Complete
        ));
    }

    #[test]
    fn scripts_convert_ascii_markers_to_display_glyphs() {
        assert_eq!('²', super::superscript_character('2'));
        assert_eq!('⁻', super::superscript_character('-'));
        assert_eq!('₁', super::subscript_character('1'));
        assert_eq!('₊', super::subscript_character('+'));
    }

    #[test]
    fn typed_power_marker_converts_to_superscript_but_evaluates_as_power() {
        let mut input = ScriptInputMode::emacs(reedline::default_emacs_keybindings());
        assert!(matches!(type_char(&mut input, '1'), ReedlineEvent::Edit(_)));
        assert!(matches!(type_char(&mut input, '0'), ReedlineEvent::Edit(_)));
        assert!(matches!(type_char(&mut input, '^'), ReedlineEvent::None));
        assert_eq!(
            ReedlineEvent::Edit(vec![EditCommand::InsertChar('²')]),
            type_char(&mut input, '2')
        );

        let mut context = kalk::parser::Context::new();
        let value = kalk::parser::eval(&mut context, "10²", 128)
            .unwrap()
            .unwrap();
        assert_eq!(100.0, value.to_f64());
    }

    #[test]
    fn typed_base_marker_converts_to_subscript() {
        let mut input = ScriptInputMode::emacs(reedline::default_emacs_keybindings());
        type_char(&mut input, '1');
        type_char(&mut input, '0');
        assert!(matches!(type_char(&mut input, '_'), ReedlineEvent::None));
        assert_eq!(
            ReedlineEvent::Edit(vec![EditCommand::InsertChar('₂')]),
            type_char(&mut input, '2')
        );
    }

    fn type_char(input: &mut ScriptInputMode, character: char) -> ReedlineEvent {
        let key = if character == '^' || character == '_' {
            KeyEvent::new(KeyCode::Char(character), KeyModifiers::SHIFT)
        } else {
            KeyEvent::new(KeyCode::Char(character), KeyModifiers::NONE)
        };
        let event = ReedlineRawEvent::try_from(Event::Key(key)).unwrap();
        input.parse_event(event)
    }
}
