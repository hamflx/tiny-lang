use std::collections::HashMap;

use crate::{
    resolution,
    utils::expression::{app_fn, if_expr, integer, let_expr, let_fn, op_add, op_gt, op_lt, var},
    vm,
};

pub(crate) fn compile(instrs: Vec<super::Instruction>) -> Vec<u8> {
    let (label_map, _) = instrs
        .iter()
        .fold((HashMap::new(), 0), |(mut map, pos), instr| {
            match instr {
                super::Instruction::Label(label) => {
                    map.insert(label.clone(), pos);
                }
                _ => {}
            }
            (map, pos + instr.size())
        });
    let instrs = instrs.into_iter().fold(Vec::new(), |mut instrs, instr| {
        match instr {
            super::Instruction::Label(_) => {}
            super::Instruction::Add => instrs.push(vm::Instruction::Add.code()),
            super::Instruction::Sub => instrs.push(vm::Instruction::Sub.code()),
            super::Instruction::Mul => instrs.push(vm::Instruction::Mul.code()),
            super::Instruction::Le => instrs.push(vm::Instruction::Le.code()),
            super::Instruction::Pop => instrs.push(vm::Instruction::Pop.code()),
            super::Instruction::Swap => instrs.push(vm::Instruction::Swap.code()),
            super::Instruction::Exit => instrs.push(vm::Instruction::Exit.code()),
            super::Instruction::Const(i) => {
                instrs.extend([vm::Instruction::Const.code(), i as u32])
            }
            super::Instruction::Var(i) => instrs.extend([vm::Instruction::Var.code(), i as u32]),
            super::Instruction::Ret(i) => instrs.extend([vm::Instruction::Ret.code(), i as u32]),
            super::Instruction::Goto(ident) => {
                let addr = label_map[&ident];
                instrs.extend([vm::Instruction::Goto.code(), addr as u32])
            }
            super::Instruction::IfZero(ident) => {
                let addr = label_map[&ident];
                instrs.extend([vm::Instruction::IfZero.code(), addr as u32])
            }
            super::Instruction::Call(ident, len) => {
                let addr = label_map[&ident];
                instrs.extend([vm::Instruction::Call.code(), addr as u32, len as u32])
            }
        };
        instrs
    });
    instrs.into_iter().fold(Vec::new(), |mut bytes, int| {
        bytes.extend(int.to_le_bytes());
        bytes
    })
}

#[test]
fn test_compile() {
    let expr = let_expr(
        "add",
        let_fn(
            &["a", "b"],
            let_expr(
                "log",
                let_fn(&["n"], var("n")),
                app_fn("log", &[op_add(var("a"), var("b"))]),
            ),
        ),
        app_fn("add", &[integer(3), integer(25)]),
    );
    let expr = resolution::compile(&expr);
    let instrs = super::compile(expr);
    let bytecode = compile(instrs);
    let mut vm = vm::Vm::create(bytecode);
    let result = vm.start();
    assert_eq!(result, 28);

    println!("result: {}", result);
}

#[test]
fn test_compile_if_else() {
    let expr = if_expr(op_lt(integer(3), integer(2)), integer(6), integer(7));
    let expr = resolution::compile(&expr);
    let instrs = super::compile(expr);
    let bytecode = compile(instrs);
    let mut vm = vm::Vm::create(bytecode);
    let result = vm.start();
    assert_eq!(result, 7);

    println!("result: {}", result);
}

#[test]
fn test_compile_if_then() {
    let expr = if_expr(op_lt(integer(3), integer(5)), integer(6), integer(7));
    let expr = resolution::compile(&expr);
    let instrs = super::compile(expr);
    let bytecode = compile(instrs);
    let mut vm = vm::Vm::create(bytecode);
    let result = vm.start();
    assert_eq!(result, 6);

    println!("result: {}", result);
}
