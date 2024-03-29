mod ast;
mod compile;
mod formula;
mod lexer;
mod parser;
mod resolution;
mod semantic;
mod utils;
mod vm;

use std::{
    ffi::CStr,
    path::Path,
    time::{SystemTime, UNIX_EPOCH},
};

use clap::Parser;
use compile::{build_syscall_stub, llvm};
use parser::parse_code;
use resolution::{make_identifier, Identifier};
use semantic::{check_expr, solve};
use vm::{CallContext, SysCall, Vm};

use crate::{
    compile::native::{run_code_native, run_code_native_string},
    semantic::{apply_subst, check_program, t_arrow, Typ},
};

const KAITIAN_RUNTIME_LIB: &[u8] =
    include_bytes!(env!("CARGO_STATICLIB_FILE_KAITIAN_RT_kaitian-rt"));

#[derive(Debug, Clone, PartialEq, Eq)]
struct SysVariableTable {
    rows: Vec<SysVariableTableRecord>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SysVariableTableRecord {
    id: Identifier,
    no: usize,
    typ: usize,
}

struct SysCallTable {
    rows: Vec<SysCallTableRecord>,
}

struct SysCallTableRecord {
    ptr: *const (),
    id: Identifier,
    typ: Typ,
    ctx: Option<Box<dyn CallContext>>,
}

fn compile_to_byte_code(
    code: &str,
    sys_calls: &SysCallTable,
    sys_vars: &SysVariableTable,
    get_index: usize,
) -> Vec<u8> {
    let expr = parse_code(code);
    let get_var_record = &sys_calls.rows[get_index];
    let expr = resolution::compile_program(
        &expr,
        sys_calls
            .rows
            .iter()
            .map(|r| r.id.clone())
            .chain(sys_vars.rows.iter().map(|r| r.id.clone()))
            .collect(),
    );
    // let expr = replace_var_with_call(expr, get_var_record.id.clone(), &sys_vars);
    let cs = check_program(
        sys_calls
            .rows
            .iter()
            .map(|r| (r.id.clone(), r.typ.clone()))
            .collect(),
        &expr,
    );
    let subst = solve(cs);
    // let typ = apply_subst(&typ, &subst);
    // println!("{}: {:?}", code, typ);
    let stub: Vec<_> = sys_calls
        .rows
        .iter()
        .enumerate()
        .map(|(i, r)| build_syscall_stub(r.id.clone(), i, r.typ.arg_len().unwrap()))
        .flatten()
        .collect();
    let instrs = compile::compile_program(expr);
    let instrs = instrs.into_iter().chain(stub).collect();
    compile::bytecode::compile(instrs)
}

struct SysCallGetContext {
    get: Box<dyn Fn(u32, u32) -> u32>,
}

impl CallContext for SysCallGetContext {}

impl std::fmt::Debug for SysCallGetContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SysCallGetContext")
            .field("get", &"Fn(u32, u32) -> u32")
            .finish()
    }
}

fn build_default_sys_calls(get_var: impl Fn(u32, u32) -> u32 + 'static) -> SysCallTable {
    fn now(_: &SysCall) -> u32 {
        return SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs() as _;
    }

    fn get_sys_var(sys_call: &SysCall, no: u32, typ: u32) -> u32 {
        println!("get sys var: no={no}, typ={typ}");
        let ctx = sys_call.ctx.as_ref().unwrap().as_ref();
        let ctx: &dyn std::any::Any = ctx.as_any();
        let ctx = ctx.downcast_ref::<SysCallGetContext>().unwrap();
        return (ctx.get)(no, typ);
    }

    fn print(_: &SysCall, fmt: u64) -> u32 {
        let ptr = unsafe { CStr::from_ptr(fmt as _) }.to_str().unwrap();
        println!("==> print: {}", ptr);
        0
    }

    SysCallTable {
        rows: vec![
            SysCallTableRecord {
                ptr: get_sys_var as *const (),
                id: make_identifier("get".to_string()),
                typ: t_arrow(Typ::Int, &[Typ::Int, Typ::Int]),
                ctx: Some(Box::new(SysCallGetContext {
                    get: Box::new(get_var),
                })),
            },
            SysCallTableRecord {
                ptr: now as *const (),
                id: make_identifier("now".to_string()),
                typ: t_arrow(Typ::Int, &[]),
                ctx: None,
            },
            SysCallTableRecord {
                ptr: print as *const (),
                id: make_identifier("print".to_string()),
                typ: t_arrow(Typ::Int, &[Typ::String]),
                ctx: None,
            },
        ],
    }
}

fn compile_and_run(code: &str) -> isize {
    compile_and_run_with_vars(code, &[])
}

fn compile_and_run_expr(code: &str) -> isize {
    compile_and_run_with_vars(&format!("fn main () -> usize {{ {code} }}"), &[])
}

fn compile_and_run_with_vars(code: &str, vars: &[(&str, u32)]) -> isize {
    let get_var = {
        let vars: Vec<_> = vars.iter().map(|(s, v)| (s.to_string(), *v)).collect();
        move |no: u32, typ: u32| vars[no as usize].1
    };
    let sys_calls = build_default_sys_calls(get_var);
    let rows = vars
        .iter()
        .enumerate()
        .map(|(i, (s, v))| SysVariableTableRecord {
            id: make_identifier(s.to_string()),
            no: i,
            typ: 1,
        })
        .collect();
    let bytecode = compile_to_byte_code(code, &sys_calls, &SysVariableTable { rows }, 0);
    let mut vm = Vm::create(bytecode);
    for SysCallTableRecord { ptr, typ, ctx, .. } in sys_calls.rows {
        vm.add_sys_call(SysCall::new(ptr, typ.arg_len().unwrap(), ctx));
    }
    vm.start() as isize
}

#[test]
fn test_compile_and_run_now() {
    assert!(
        (compile_and_run("fn main () -> usize { now() + 1 }")
            - SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_secs() as isize)
            .abs()
            < 2
    );
}

#[test]
fn test_compile_and_run_fn() {
    assert_eq!(compile_and_run("fn main() -> usize { 1 + 1 }"), 2);
}

#[test]
fn test_compile_and_run_print_str() {
    assert_eq!(
        compile_and_run("fn main() -> usize { print(\"hello\"); 0 }"),
        0
    );
    assert_eq!(run_code_native_string("\"hello\""), "hello");
}

#[test]
fn test_compile_and_run() {
    macro_rules! run {
        ($($t:tt)*) => {
            assert_eq!(compile_and_run_expr(stringify!($($t)*)), $($t)*);
        };
    }
    run!(1 + 2 * 3);
    run!(if 5 > 2 { 1 } else { 0 });
    run!(if 2 > 2 { 1 } else { 0 });
    run!(if 2 >= 2 { 1 } else { 0 });
    run!(if 2 >= 4 { 1 } else { 0 });

    run!(if 5 < 2 { 1 } else { 0 });
    run!(if 2 < 2 { 1 } else { 0 });
    run!(if 2 <= 2 { 1 } else { 0 });
    run!(if 2 <= 4 { 1 } else { 0 });
    run!(if 2 < 4 { 1 } else { 0 });

    run!(if 2 < 4 && 3 > 2 { 1 } else { 0 });
    run!(if 2 < 4 && 3 > 5 { 1 } else { 0 });

    run!(if 2 < 4 && 3 > 5 || 5 > 1 { 1 } else { 0 });

    run!(if 2 < 4 && 3 > 5 || !(5 > 1) { 1 } else { 0 });

    // todo 虚拟机的负数支持。
    // run!(1 + 2 * -3);
    // run!(1 + 2 - -3);
    run!(1 + 2 * 3 - (5 - 2));
    // run!(1 + 2 * 3 - -(5 - 2));
    // run!(1 + 2 * 3 / -(5 - 2));
}

#[test]
fn test_run_native() {
    macro_rules! run {
        ($($t:tt)*) => {
            assert_eq!(run_code_native(stringify!($($t)*)), $($t)*);
        };
    }
    run!(1 + 2 * 3);
    run!(if 5 > 2 { 1 } else { 0 });
    run!(if 2 > 2 { 1 } else { 0 });
    run!(if 2 >= 2 { 1 } else { 0 });
    run!(if 2 >= 4 { 1 } else { 0 });

    run!(if 5 < 2 { 1 } else { 0 });
    run!(if 2 < 2 { 1 } else { 0 });
    run!(if 2 <= 2 { 1 } else { 0 });
    run!(if 2 <= 4 { 1 } else { 0 });
    run!(if 2 < 4 { 1 } else { 0 });

    run!(if 2 < 4 && 3 > 2 { 1 } else { 0 });
    run!(if 2 < 4 && 3 > 5 { 1 } else { 0 });

    run!(if 2 < 4 && 3 > 5 || 5 > 1 { 1 } else { 0 });

    run!(if 2 < 4 && 3 > 5 || !(5 > 1) { 1 } else { 0 });

    run!(1 + 2 * 3 - (5 - 2));
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    filename: Option<String>,
}

fn main() {
    let args = Args::parse();
    match args.filename {
        Some(filename) => {
            let path = Path::new(&filename).canonicalize().unwrap();
            let base_path = path.parent().unwrap().join(path.file_stem().unwrap());
            let content = std::fs::read_to_string(filename).unwrap();
            llvm::compile_exe(&content, &base_path, &base_path);
        }
        None => {
            for line in std::io::stdin().lines() {
                let line = line.unwrap();
                let result = compile_and_run(&line);
                println!("= {result}");
            }
        }
    }
}
