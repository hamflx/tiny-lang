use crate::{
    resolution::{self, Identifier},
    SysVariableTable,
};

fn replace_var_with_call(
    expr: resolution::Expr,
    get_ident: Identifier,
    table: &SysVariableTable,
) -> resolution::Expr {
    match expr {
        resolution::Expr::Var(ident) => {
            let record = table.rows.iter().find(|r| r.id == ident).unwrap();
            let no = resolution::Expr::CstI(record.no as _);
            let typ = resolution::Expr::CstI(record.typ as _);
            let args = vec![no, typ];
            resolution::Expr::App(get_ident.clone(), args)
        }
        resolution::Expr::Fn(expr) => resolution::Expr::Fn(
            resolution::FnExpression {
                params: expr.params,
                body: replace_var_with_call(expr.body, get_ident, table),
            }
            .into(),
        ),
        resolution::Expr::Let(expr) => resolution::Expr::Let(
            resolution::LetExpression {
                name: expr.name,
                value: replace_var_with_call(expr.value, get_ident.clone(), table),
                scope: replace_var_with_call(expr.scope, get_ident.clone(), table),
            }
            .into(),
        ),
        resolution::Expr::App(ident, args) => resolution::Expr::App(
            ident,
            args.into_iter()
                .map(|p| replace_var_with_call(p, get_ident.clone(), table))
                .collect(),
        ),
        resolution::Expr::If(expr) => resolution::Expr::If(
            resolution::IfExpression {
                condition: replace_var_with_call(expr.condition, get_ident.clone(), table),
                consequence: replace_var_with_call(expr.consequence, get_ident.clone(), table),
                alternative: replace_var_with_call(expr.alternative, get_ident.clone(), table),
            }
            .into(),
        ),
        resolution::Expr::BinaryOperation(expr) => resolution::Expr::BinaryOperation(
            resolution::BinaryExpression {
                op: expr.op,
                left: replace_var_with_call(expr.left, get_ident.clone(), table),
                right: replace_var_with_call(expr.right, get_ident.clone(), table),
            }
            .into(),
        ),
        resolution::Expr::Comparison(expr) => resolution::Expr::Comparison(
            resolution::ComparisonExpression {
                op: expr.op,
                left: replace_var_with_call(expr.left, get_ident.clone(), table),
                right: replace_var_with_call(expr.right, get_ident.clone(), table),
            }
            .into(),
        ),
        resolution::Expr::Logical(expr) => resolution::Expr::Logical(
            resolution::LogicalExpression {
                op: expr.op,
                left: replace_var_with_call(expr.left, get_ident.clone(), table),
                right: replace_var_with_call(expr.right, get_ident.clone(), table),
            }
            .into(),
        ),
        resolution::Expr::Not(expr) => {
            resolution::Expr::Not(replace_var_with_call(*expr, get_ident.clone(), table).into())
        }
        expr => expr,
    }
}
