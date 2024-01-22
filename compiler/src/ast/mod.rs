use crate::semantic::Typ;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct LetExpression {
    pub(crate) name: String,
    pub(crate) value: Expression,
    pub(crate) scope: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FnExpression {
    pub(crate) params: Vec<String>,
    pub(crate) body: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expression {
    Var(String),
    CstI(isize),
    CstF(f64),
    CstB(bool),
    Instant(usize),
    TimeSpan(usize),
    Fn(Box<FnExpression>),
    Let(Box<LetExpression>),
    App(String, Vec<Expression>),
    If(Box<IfExpression>),
    BinaryOperation(Box<BinaryExpression>),
    Comparison(Box<ComparisonExpression>),
    Logical(Box<LogicalExpression>),
    Not(Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct AstProgram {
    pub(crate) items: Vec<AstDeclaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum AstDeclaration {
    Fn(AstFnDeclaration),
    Let(AstLetDeclaration),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct AstFnDeclaration {
    pub(crate) name: String,
    pub(crate) params: Vec<(String, Typ)>,
    pub(crate) typ: Typ,
    pub(crate) body: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct AstLetDeclaration {
    pub(crate) name: String,
    pub(crate) value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BinaryExpression {
    pub(crate) op: BinaryOperator,
    pub(crate) left: Expression,
    pub(crate) right: Expression,
}

impl BinaryExpression {
    pub(crate) fn new(op: BinaryOperator, left: Expression, right: Expression) -> Self {
        Self { op, left, right }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ComparisonOperator {
    Lt,
    Gt,
    Ge,
    Le,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum LogicalOperator {
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ComparisonExpression {
    pub(crate) op: ComparisonOperator,
    pub(crate) left: Expression,
    pub(crate) right: Expression,
}

impl ComparisonExpression {
    pub(crate) fn new(op: ComparisonOperator, left: Expression, right: Expression) -> Self {
        Self { op, left, right }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct LogicalExpression {
    pub(crate) op: LogicalOperator,
    pub(crate) left: Expression,
    pub(crate) right: Expression,
}

impl LogicalExpression {
    pub(crate) fn new(op: LogicalOperator, left: Expression, right: Expression) -> Self {
        Self { op, left, right }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct IfExpression {
    pub(crate) condition: Expression,
    pub(crate) then: Expression,
    pub(crate) other: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct CallExpression {
    pub(crate) callee: String,
    pub(crate) args: Vec<Expression>,
}
