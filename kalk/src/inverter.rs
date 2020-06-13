use crate::ast::Expr;
use crate::lexer::TokenKind;

impl Expr {
    pub fn invert(&self) -> Self {
        match self {
            Expr::Binary(left, op, right) => invert_binary(&left, op, &right),
            Expr::Unary(op, expr) => invert_unary(op, &expr),
            Expr::Unit(identifier, expr) => invert_unit(&identifier, &expr),
            Expr::Var(identifier) => invert_value(self),
            Expr::Group(expr) => invert_group(&expr),
            Expr::FnCall(identifier, expressions) => invert_fn_call(&identifier, expressions),
            Expr::Literal(value) => invert_value(self),
        }
    }
}

fn invert_binary(left: &Expr, op: &TokenKind, right: &Expr) -> Expr {
    let op_inv = match op {
        TokenKind::Plus => TokenKind::Minus,
        TokenKind::Minus => TokenKind::Plus,
        TokenKind::Star => TokenKind::Slash,
        TokenKind::Slash => TokenKind::Star,
        _ => unreachable!(),
    };

    Expr::Binary(Box::new(right.invert()), op_inv, Box::new(left.invert()))
}

fn invert_unary(op: &TokenKind, expr: &Expr) -> Expr {
    match op {
        TokenKind::Minus => expr.clone(),
        TokenKind::Exclamation => unimplemented!(),
        _ => unreachable!(),
    }
}

fn invert_unit(identifier: &str, expr: &Expr) -> Expr {
    unimplemented!()
}

fn invert_group(expr: &Expr) -> Expr {
    invert_value(expr)
}

fn invert_fn_call(identifier: &str, expressions: &Vec<Expr>) -> Expr {
    unimplemented!()
}

fn invert_value(expr: &Expr) -> Expr {
    Expr::Unary(TokenKind::Minus, Box::new(expr.clone()))
}
