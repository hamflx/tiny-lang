use std::{path::Path, process::Command};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    IntPredicate,
};
use tempfile::tempdir;

use crate::{
    ast,
    parser::parse_code,
    resolution,
    semantic::{check_program, solve, Substituation, Typ},
    KAITIAN_RUNTIME_LIB,
};

use super::Fun;

#[test]
fn test_compile_llvm_ir() {
    assert_eq!(compile_and_run("fn main () -> usize { 3 + 2 }"), 5);
}

pub(crate) fn compile_and_run(code: &str) -> i64 {
    let base_path = std::env::current_dir().unwrap().join("tiny-lang-program");
    compile_exe(code, &base_path, &base_path);
    Command::new(base_path)
        .spawn()
        .unwrap()
        .wait()
        .unwrap()
        .code()
        .unwrap() as _
}

pub(crate) fn compile_exe(code: &str, base_path: &Path, output_path: &Path) {
    let rt_name = "kaitian_rt";
    let rt_dir = tempdir().unwrap();
    let rt_path = rt_dir.path().join(format!("lib{}.a", rt_name));
    std::fs::write(rt_path, KAITIAN_RUNTIME_LIB).unwrap();

    let ir_path = base_path.with_extension("ll");
    let prog = parse_code(code);
    let prog = resolution::compile_program(&prog, Vec::new());
    let constraints = check_program(Vec::new(), &prog);
    let subst = solve(constraints);
    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("tmp");
    for i in prog.items {
        match i {
            resolution::AstDeclaration::Fn(func) => {
                let mut compiler = Compiler::new(&context, &builder, &module, &func, &subst);
                compiler.compile(&func);
                module.print_to_file(&ir_path).unwrap();
            }
            resolution::AstDeclaration::Let(_) => todo!(),
        }
    }
    let out_dir = ir_path.parent().unwrap();
    let obj_file = base_path.with_extension("o");
    Command::new("llc")
        .args([
            "-filetype=obj",
            ir_path.to_str().unwrap(),
            "-o",
            obj_file.to_str().unwrap(),
        ])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    Command::new("clang")
        .args([
            "-L",
            rt_dir.path().to_str().unwrap(),
            "-l",
            rt_name,
            obj_file.to_str().unwrap(),
            "-o",
            output_path.to_str().unwrap(),
        ])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
}

struct Compiler<'a, 'ctx> {
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    function: &'a resolution::AstFnDeclaration,
    fn_value_opt: Option<FunctionValue<'ctx>>,
    subst: &'a Substituation,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
        function: &'a resolution::AstFnDeclaration,
        subst: &'a Substituation,
    ) -> Self {
        Self {
            context,
            builder,
            module,
            function,
            fn_value_opt: None,
            subst,
        }
    }

    fn compile(&mut self, function: &resolution::AstFnDeclaration) {
        self.compile_fn(function);
    }

    fn compile_let_statement(
        &self,
        env: &mut Vec<(resolution::Identifier, PointerValue<'ctx>)>,
        stmt: &resolution::AstLetDeclaration,
    ) {
        let value = self.compile_expr(env, &stmt.value).unwrap();
        // todo typ
        let alloc = self.create_entry_block_alloca(&stmt.name.to_string(), Typ::Int);
        self.builder.build_store(alloc, value).unwrap();
        env.insert(0, (stmt.name.clone(), alloc));
    }

    fn compile_expr(
        &self,
        env: &mut Vec<(resolution::Identifier, PointerValue<'ctx>)>,
        expr: &resolution::Expr,
    ) -> Option<BasicValueEnum<'ctx>> {
        match expr {
            resolution::Expr::Var(v) => {
                let (ident, ptr) = env
                    .iter()
                    .find(|(ident, _)| ident == v)
                    .ok_or_else(|| format!("No variable named: {}", v))
                    .unwrap();
                Some(
                    self.builder
                        .build_load(*ptr, &format!("{}", ident))
                        .unwrap(),
                )
            }
            resolution::Expr::CstI(v) => {
                Some(self.context.i64_type().const_int(*v as _, *v < 0).into())
            }
            resolution::Expr::CstF(_) => todo!(),
            resolution::Expr::CstB(_) => todo!(),
            resolution::Expr::StrLiteral(_) => todo!(),
            resolution::Expr::Instant(_) => todo!(),
            resolution::Expr::TimeSpan(_) => todo!(),
            resolution::Expr::Fn(_) => todo!(),
            resolution::Expr::Let(_) => todo!(),
            resolution::Expr::App(ident, args) => {
                let compiled_args = args
                    .iter()
                    .map(|arg| self.compile_expr(env, arg).unwrap().into())
                    .collect::<Vec<_>>();
                let fun = self.module.get_function(&format!("{}", ident)).unwrap();
                self.builder
                    .build_call(fun, &compiled_args, "tmp")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
            }
            resolution::Expr::If(expr) => {
                let cond = self
                    .compile_expr(env, &expr.condition)
                    .unwrap()
                    .into_int_value();
                let fun = self.fn_value();
                let then_block = self.context.append_basic_block(fun, "then");
                let else_block = self.context.append_basic_block(fun, "else");
                let cont_block = self.context.append_basic_block(fun, "ifcont");
                self.builder
                    .build_conditional_branch(cond, then_block, else_block)
                    .unwrap();

                self.builder.position_at_end(then_block);
                let then_val = self.compile_expr(env, &expr.consequence).unwrap();
                self.builder.build_unconditional_branch(cont_block).unwrap();

                self.builder.position_at_end(else_block);
                let else_val = self.compile_expr(env, &expr.alternative).unwrap();
                self.builder.build_unconditional_branch(cont_block).unwrap();

                self.builder.position_at_end(cont_block);
                // todo type
                let phi = self
                    .builder
                    .build_phi(self.context.i64_type(), "iftmp")
                    .unwrap();
                phi.add_incoming(&[(&then_val, then_block), (&else_val, else_block)]);

                Some(phi.as_basic_value())
            }
            resolution::Expr::BinaryOperation(expr) => {
                let lhs = self.compile_expr(env, &expr.left).unwrap();
                let rhs = self.compile_expr(env, &expr.right).unwrap();
                let val = match expr.op {
                    crate::ast::BinaryOperator::Add => self
                        .builder
                        .build_int_add(lhs.into_int_value(), rhs.into_int_value(), "tmpadd")
                        .unwrap(),
                    crate::ast::BinaryOperator::Sub => self
                        .builder
                        .build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "tmpsub")
                        .unwrap(),
                    crate::ast::BinaryOperator::Mul => self
                        .builder
                        .build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "tmpmul")
                        .unwrap(),
                    crate::ast::BinaryOperator::Div => self
                        .builder
                        .build_int_signed_div(lhs.into_int_value(), rhs.into_int_value(), "tmpdiv")
                        .unwrap(),
                };
                Some(val.into())
            }
            resolution::Expr::Comparison(expr) => {
                let cmp_val = self
                    .builder
                    .build_int_compare(
                        match expr.op {
                            ast::ComparisonOperator::Lt => IntPredicate::SLT,
                            ast::ComparisonOperator::Gt => IntPredicate::SGT,
                            ast::ComparisonOperator::Ge => IntPredicate::SGE,
                            ast::ComparisonOperator::Le => IntPredicate::SLE,
                        },
                        self.compile_expr(env, &expr.left).unwrap().into_int_value(),
                        self.compile_expr(env, &expr.right)
                            .unwrap()
                            .into_int_value(),
                        "tmpcmp",
                    )
                    .unwrap();
                Some(cmp_val.as_basic_value_enum())
            }
            resolution::Expr::Logical(expr) => {
                let lhs = self.compile_expr(env, &expr.left).unwrap().into_int_value();
                let rhs = self
                    .compile_expr(env, &expr.right)
                    .unwrap()
                    .into_int_value();
                let value = match expr.op {
                    ast::LogicalOperator::And => self.builder.build_and(lhs, rhs, "tmpand"),
                    ast::LogicalOperator::Or => self.builder.build_or(lhs, rhs, "tmpor"),
                }
                .unwrap();
                Some(value.as_basic_value_enum())
            }
            resolution::Expr::Not(_) => todo!(),
        }
    }

    fn compile_statement(
        &self,
        env: &mut Vec<(resolution::Identifier, PointerValue<'ctx>)>,
        stmt: &resolution::AstStatement,
    ) -> Option<BasicValueEnum<'ctx>> {
        match stmt {
            resolution::AstStatement::Let(let_stmt) => {
                self.compile_let_statement(env, let_stmt);
                None
            }
            resolution::AstStatement::Expr(expr_stmt) => self.compile_expr(env, expr_stmt),
        }
    }

    fn compile_fn(&mut self, function: &resolution::AstFnDeclaration) -> FunctionValue<'ctx> {
        let fn_value = self.compile_fn_proto(function);
        self.fn_value_opt = Some(fn_value);

        if function.body.is_empty() {
            return fn_value;
        }

        let entry = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry);

        let mut variables = fn_value
            .get_param_iter()
            .enumerate()
            .map(|(i, arg)| {
                let (ident, typ) = function.params[i].clone();
                let alloca = self.create_entry_block_alloca(ident.to_string().as_str(), typ);
                self.builder.build_store(alloca, arg).unwrap();
                (ident, alloca)
            })
            .collect::<Vec<_>>();

        for stmt in function.body.iter().take(function.body.len() - 1) {
            self.compile_statement(&mut variables, stmt);
        }

        let last = self.compile_statement(&mut variables, function.body.last().unwrap());
        self.builder
            .build_return(last.as_ref().map(|v| v as &dyn BasicValue))
            .unwrap();

        if fn_value.verify(true) {
            fn_value
        } else {
            unsafe { fn_value.delete() };
            panic!("Invalid generated function.");
        }
    }

    fn compile_fn_proto(&self, function: &resolution::AstFnDeclaration) -> FunctionValue<'ctx> {
        let param_types = function
            .params
            .iter()
            .map(|(name, typ)| match typ {
                crate::semantic::Typ::Unit => todo!(),
                crate::semantic::Typ::Int => self.context.i64_type().into(),
                crate::semantic::Typ::Bool => self.context.bool_type().into(),
                crate::semantic::Typ::Instant => todo!(),
                crate::semantic::Typ::Duration => todo!(),
                crate::semantic::Typ::String => todo!(),
                crate::semantic::Typ::Var(_) => todo!(),
                crate::semantic::Typ::Record(_) => todo!(),
                crate::semantic::Typ::Arrow(_) => todo!(),
            })
            .collect::<Vec<BasicMetadataTypeEnum>>();
        let fn_type = match function.typ {
            crate::semantic::Typ::Unit => self.context.void_type().fn_type(&param_types, false),
            crate::semantic::Typ::Int => self.context.i64_type().fn_type(&param_types, false),
            crate::semantic::Typ::Bool => self.context.bool_type().fn_type(&param_types, false),
            crate::semantic::Typ::Instant => todo!(),
            crate::semantic::Typ::Duration => todo!(),
            crate::semantic::Typ::String => todo!(),
            crate::semantic::Typ::Var(_) => todo!(),
            crate::semantic::Typ::Record(_) => todo!(),
            crate::semantic::Typ::Arrow(_) => todo!(),
        };
        let fn_val = self
            .module
            .add_function(function.name.to_string().as_str(), fn_type, None);
        for (i, param) in fn_val.get_param_iter().enumerate() {
            param.set_name(&function.params[i].0.name);
        }
        fn_val
    }

    /// Creates a new stack allocation instruction in the entry block of the function.
    fn create_entry_block_alloca(&self, name: &str, typ: Typ) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = self.fn_value().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder
            .build_alloca(self.build_basic_type(typ), name)
            .unwrap()
    }

    fn build_basic_type(&self, typ: Typ) -> BasicTypeEnum<'ctx> {
        match typ {
            Typ::Unit => todo!(),
            Typ::Int => self.context.i64_type().as_basic_type_enum(),
            Typ::Bool => self.context.bool_type().as_basic_type_enum(),
            Typ::Instant => todo!(),
            Typ::Duration => todo!(),
            Typ::String => todo!(),
            Typ::Var(_) => todo!(),
            Typ::Record(_) => todo!(),
            Typ::Arrow(_) => todo!(),
        }
    }

    /// Returns the `FunctionValue` representing the function being compiled.
    #[inline]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }
}
