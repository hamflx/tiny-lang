use crate::parser::BinaryOperator;

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Id(String),
    CstI(isize),
    CstF(f64),
    CstB(bool),
    Instant(usize),
    TimeSpan(usize),
    Let(Box<LetExpression>),
    BinaryOperation(Box<BinaryExpression>),
}

#[derive(Debug, Clone, PartialEq)]
struct BinaryExpression {
    op: BinaryOperator,
    left: Expr,
    right: Expr,
}

impl BinaryExpression {
    fn new(op: BinaryOperator, left: Expr, right: Expr) -> Self {
        Self { op, left, right }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct LetExpression {
    name: String,
    value: Expr,
    context: Expr,
}

fn compile() {}
