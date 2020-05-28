use crate::interpreter::Interpreter;
use crate::lexer::Lexer;
use crate::parser::{Parser, Unit};

pub const DEFAULT_ANGLE_UNIT: Unit = Unit::Radians;

pub struct MathParser {
    parser: Parser,
    interpreter: Interpreter,
}

impl MathParser {
    pub fn new() -> Self {
        MathParser {
            parser: Parser::new(),
            interpreter: Interpreter::new(DEFAULT_ANGLE_UNIT),
        }
    }

    pub fn parse(&mut self, source: &str) -> Option<f64> {
        let tokens = Lexer::lex(source);
        let statements = self.parser.parse(tokens);

        self.interpreter.interpret(statements)
    }

    pub fn set_angle_unit(&mut self, unit: Unit) {
        self.interpreter.set_angle_unit(unit);
    }
}
