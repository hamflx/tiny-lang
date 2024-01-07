use crate::parser::{
    BinaryExpression, BinaryOperator, Expression, FnExpression, IfExpression, LetExpression,
    LogicalExpression, LogicalOperator,
};

pub(crate) fn id(name: String) -> Expression {
    Expression::Var(name)
}

pub(crate) fn integer(num: isize) -> Expression {
    Expression::CstI(num)
}

pub(crate) fn timestamp(ts: usize) -> Expression {
    Expression::Instant(ts)
}

pub(crate) fn time_span(ts: usize) -> Expression {
    Expression::TimeSpan(ts)
}

pub(crate) fn var(name: &str) -> Expression {
    Expression::Var(name.to_string())
}

pub(crate) fn let_expr(name: &str, value: Expression, scope: Expression) -> Expression {
    Expression::Let(
        LetExpression {
            name: name.to_string(),
            scope,
            value,
        }
        .into(),
    )
}

pub(crate) fn if_expr(cond: Expression, then: Expression, other: Expression) -> Expression {
    Expression::If(
        IfExpression {
            condition: cond,
            then,
            other,
        }
        .into(),
    )
}

pub(crate) fn let_fn(params: &[&str], body: Expression) -> Expression {
    Expression::Fn(
        FnExpression {
            params: params.iter().map(|s| s.to_string()).collect(),
            body,
        }
        .into(),
    )
}

pub(crate) fn app_fn(name: &str, args: &[Expression]) -> Expression {
    Expression::App(name.to_string(), args.iter().cloned().collect())
}

pub(crate) fn op_gt(left: Expression, right: Expression) -> Expression {
    Expression::Logical(LogicalExpression::new(LogicalOperator::Gt, left, right).into())
}

pub(crate) fn op_lt(left: Expression, right: Expression) -> Expression {
    Expression::Logical(LogicalExpression::new(LogicalOperator::Lt, left, right).into())
}

pub(crate) fn op_add(left: Expression, right: Expression) -> Expression {
    Expression::BinaryOperation(BinaryExpression::new(BinaryOperator::Add, left, right).into())
}

pub(crate) fn op_sub(left: Expression, right: Expression) -> Expression {
    Expression::BinaryOperation(BinaryExpression::new(BinaryOperator::Sub, left, right).into())
}

pub(crate) fn op_mul(left: Expression, right: Expression) -> Expression {
    Expression::BinaryOperation(BinaryExpression::new(BinaryOperator::Mul, left, right).into())
}

pub(crate) fn op_div(left: Expression, right: Expression) -> Expression {
    Expression::BinaryOperation(BinaryExpression::new(BinaryOperator::Div, left, right).into())
}
