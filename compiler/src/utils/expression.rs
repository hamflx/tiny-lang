use crate::{
    ast::{
        AstDeclaration, AstFnDeclaration, AstLetDeclaration, AstProgram, AstStatement,
        BinaryExpression, BinaryOperator, ComparisonExpression, ComparisonOperator, Expression,
        FnExpression, IfExpression, LetExpression, LogicalExpression, LogicalOperator,
    },
    semantic::Typ,
};

pub(crate) fn ast_prog(items: &[AstDeclaration]) -> AstProgram {
    AstProgram {
        items: items.into_iter().cloned().collect(),
    }
}

pub(crate) fn ast_fn(
    name: &str,
    params: &[(&str, Typ)],
    ret_type: Typ,
    body: Expression,
) -> AstDeclaration {
    AstDeclaration::Fn(AstFnDeclaration {
        name: name.to_string(),
        params: params
            .into_iter()
            .map(|(n, t)| (n.to_string(), t.clone()))
            .collect(),
        body: vec![AstStatement::Expr(body)],
        typ: ret_type,
    })
}

pub(crate) fn ast_fn_stmt(
    name: &str,
    params: &[(&str, Typ)],
    ret_type: Typ,
    body: &[AstStatement],
) -> AstDeclaration {
    AstDeclaration::Fn(AstFnDeclaration {
        name: name.to_string(),
        params: params
            .into_iter()
            .map(|(n, t)| (n.to_string(), t.clone()))
            .collect(),
        body: body.iter().cloned().collect(),
        typ: ret_type,
    })
}

pub(crate) fn ast_let(name: &str, value: Expression) -> AstDeclaration {
    AstDeclaration::Let(AstLetDeclaration {
        name: name.to_string(),
        value,
    })
}

pub(crate) fn id(name: String) -> Expression {
    Expression::Var(name)
}

pub(crate) fn lit_string(text: &str) -> Expression {
    Expression::StrLiteral(text.to_string())
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

pub(crate) fn stmt_let(name: &str, value: Expression) -> AstStatement {
    AstStatement::Let(AstLetDeclaration {
        name: name.to_string(),
        value,
    })
}

pub(crate) fn stmt_expr(value: Expression) -> AstStatement {
    AstStatement::Expr(value)
}

pub(crate) fn fn_expr(params: &[String], body: Expression) -> Expression {
    Expression::Fn(
        FnExpression {
            params: params.into_iter().cloned().collect(),
            body,
        }
        .into(),
    )
}

pub(crate) fn call_expr(callee: String, args: Vec<Expression>) -> Expression {
    Expression::App(callee, args)
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
    Expression::Comparison(ComparisonExpression::new(ComparisonOperator::Gt, left, right).into())
}

pub(crate) fn op_lt(left: Expression, right: Expression) -> Expression {
    Expression::Comparison(ComparisonExpression::new(ComparisonOperator::Lt, left, right).into())
}

pub(crate) fn op_ge(left: Expression, right: Expression) -> Expression {
    Expression::Comparison(ComparisonExpression::new(ComparisonOperator::Ge, left, right).into())
}

pub(crate) fn op_le(left: Expression, right: Expression) -> Expression {
    Expression::Comparison(ComparisonExpression::new(ComparisonOperator::Le, left, right).into())
}

pub(crate) fn op_and(left: Expression, right: Expression) -> Expression {
    Expression::Logical(LogicalExpression::new(LogicalOperator::And, left, right).into())
}

pub(crate) fn op_or(left: Expression, right: Expression) -> Expression {
    Expression::Logical(LogicalExpression::new(LogicalOperator::Or, left, right).into())
}

pub(crate) fn op_not(expr: Expression) -> Expression {
    Expression::Not(expr.into())
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
