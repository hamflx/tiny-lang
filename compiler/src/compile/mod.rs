use crate::{parser::BinaryOperator, resolution::Expr};

struct Fun {}

fn compile_impl(expr: Expr) -> Vec<Fun> {
    todo!()
}

fn compile(expr: Expr) -> Vec<Fun> {
    compile_impl(expr)
}
