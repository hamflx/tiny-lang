use std::{path::Path, process::Command};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicMetadataTypeEnum,
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
};

use crate::{parser::parse_code, resolution};

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
    let ir_path = base_path.with_extension("ll");
    let prog = parse_code(code);
    let prog = resolution::compile_program(&prog, Vec::new());
    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("tmp");
    for i in prog.items {
        match i {
            resolution::AstDeclaration::Fn(func) => {
                let mut compiler = Compiler::new(&context, &builder, &module, &func);
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
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
        function: &'a resolution::AstFnDeclaration,
    ) -> Self {
        Self {
            context,
            builder,
            module,
            function,
            fn_value_opt: None,
        }
    }

    fn compile(&mut self, function: &resolution::AstFnDeclaration) {
        self.compile_fn(function);
    }

    fn compile_let_statement(
        &self,
        env: &mut Vec<(resolution::Identifier, BasicValueEnum<'ctx>)>,
        stmt: &resolution::AstLetDeclaration,
    ) {
        todo!()
    }

    fn compile_expr(
        &self,
        env: &mut Vec<(resolution::Identifier, BasicValueEnum<'ctx>)>,
        expr: &resolution::Expr,
    ) -> Option<BasicValueEnum<'ctx>> {
        match expr {
            resolution::Expr::Var(_) => todo!(),
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
            resolution::Expr::App(_, _) => todo!(),
            resolution::Expr::If(_) => todo!(),
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
            resolution::Expr::Comparison(_) => todo!(),
            resolution::Expr::Logical(_) => todo!(),
            resolution::Expr::Not(_) => todo!(),
        }
    }

    fn compile_statement(
        &self,
        env: &mut Vec<(resolution::Identifier, BasicValueEnum<'ctx>)>,
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
                let alloca = self.create_entry_block_alloca(ident.to_string().as_str());
                self.builder.build_store(alloca, arg);
                (ident, arg)
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
    fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = self.fn_value().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(self.context.f64_type(), name).unwrap()
    }

    /// Returns the `FunctionValue` representing the function being compiled.
    #[inline]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }
}
