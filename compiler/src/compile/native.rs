use std::collections::HashMap;

use iced_x86::code_asm::*;
use memmap2::MmapOptions;

use crate::{compile, parser_gen::parse_code, resolution};

type ExeCodeFn = extern "system" fn() -> usize;

pub(crate) fn run_code_native(code: &str) -> u32 {
    let expr = parse_code(code);
    let expr = resolution::compile_with_env(&expr, vec![]);
    let instrs = compile::compile(expr);
    let bytes = compile(instrs);
    let mut mem = MmapOptions::new().len(bytes.len()).map_anon().unwrap();
    mem.copy_from_slice(&bytes);
    let mem = mem.make_exec().unwrap();
    let exe_fn: ExeCodeFn = unsafe { std::mem::transmute(mem.as_ptr()) };
    let value = exe_fn();
    value as _
}

#[test]
fn test_generate_shellcode() {
    assert_eq!(run_code_native("3+4"), 7);
}

pub(crate) fn compile(instrs: Vec<super::Instruction>) -> Vec<u8> {
    let mut asm = CodeAssembler::new(64).unwrap();
    let mut label_map = instrs.iter().fold(HashMap::new(), |mut map, instr| {
        match instr {
            super::Instruction::Label(ident) => {
                let label = asm.create_label();
                map.insert(ident.clone(), label);
            }
            _ => {}
        }
        map
    });
    for instr in instrs {
        match instr {
            super::Instruction::Label(ident) => {
                let label = label_map.get_mut(&ident).unwrap();
                asm.set_label(label).unwrap();
            }
            super::Instruction::Add => {
                asm.pop(rax).unwrap();
                asm.pop(rcx).unwrap();
                asm.add(rax, rcx).unwrap();
                asm.push(rax).unwrap();
            }
            super::Instruction::Sub => {
                asm.pop(rcx).unwrap();
                asm.pop(rax).unwrap();
                asm.sub(rax, rcx).unwrap();
                asm.push(rax).unwrap();
            }
            super::Instruction::Mul => {
                asm.pop(rax).unwrap();
                asm.pop(rcx).unwrap();
                asm.mul(rcx).unwrap();
                asm.push(rax).unwrap();
            }
            // 由类型检查保证 And 和 Or 的操作数总是 bool 类型。
            super::Instruction::And => {
                asm.pop(rax).unwrap();
                asm.pop(rcx).unwrap();
                asm.and(rax, rcx).unwrap();
                asm.and(rax, 1).unwrap();
                asm.push(rax).unwrap();
            }
            super::Instruction::Or => {
                asm.pop(rax).unwrap();
                asm.pop(rcx).unwrap();
                asm.or(rax, rcx).unwrap();
                asm.and(rax, 1).unwrap();
                asm.push(rax).unwrap();
            }
            super::Instruction::Not => {
                asm.pop(rax).unwrap();
                asm.not(rax).unwrap();
                asm.and(rax, 1).unwrap();
                asm.push(rax).unwrap();
            }
            super::Instruction::Le => {
                asm.pop(rcx).unwrap();
                asm.pop(rax).unwrap();
                asm.cmp(rax, rcx).unwrap();
                asm.setna(al).unwrap();
                asm.movzx(rcx, al).unwrap();
                asm.push(rcx).unwrap();
            }
            super::Instruction::Ge => {
                asm.pop(rcx).unwrap();
                asm.pop(rax).unwrap();
                asm.cmp(rax, rcx).unwrap();
                asm.setae(al).unwrap();
                asm.movzx(rcx, al).unwrap();
                asm.push(rcx).unwrap();
            }
            super::Instruction::Lt => {
                asm.pop(rcx).unwrap();
                asm.pop(rax).unwrap();
                asm.cmp(rax, rcx).unwrap();
                asm.setnae(al).unwrap();
                asm.movzx(rcx, al).unwrap();
                asm.push(rcx).unwrap();
            }
            super::Instruction::Gt => {
                asm.pop(rcx).unwrap();
                asm.pop(rax).unwrap();
                asm.cmp(rax, rcx).unwrap();
                asm.seta(al).unwrap();
                asm.movzx(rcx, al).unwrap();
                asm.push(rcx).unwrap();
            }
            super::Instruction::Pop => {
                asm.pop(rax).unwrap();
            }
            super::Instruction::Swap => {
                asm.pop(rax).unwrap();
                asm.pop(rcx).unwrap();
                asm.push(rax).unwrap();
                asm.push(rcx).unwrap();
            }
            super::Instruction::Exit => {
                asm.pop(rax).unwrap();
                asm.ret().unwrap();
            }
            super::Instruction::Const(i) => {
                asm.mov(rax, i as i64).unwrap();
                asm.push(rax).unwrap();
            }
            super::Instruction::Var(i) => {
                asm.mov(rcx, i as i64).unwrap();
                asm.mov(rax, rax + rcx * 8 + 0).unwrap();
                asm.push(rax).unwrap();
            }
            super::Instruction::Ret(0) => {
                asm.pop(rax).unwrap();
                asm.ret().unwrap();
            }
            super::Instruction::Ret(i) => {
                asm.pop(rax).unwrap();
                asm.ret_1(i as i32).unwrap();
            }
            super::Instruction::Goto(ident) => {
                let addr = label_map[&ident];
                asm.jmp(addr).unwrap();
            }
            super::Instruction::IfZero(ident) => {
                let addr = label_map[&ident];
                asm.pop(rax).unwrap();
                asm.test(rax, rax).unwrap();
                asm.je(addr).unwrap();
            }
            super::Instruction::Call(ident, _) => {
                let addr = label_map[&ident];
                asm.call(addr).unwrap();
                asm.push(rax).unwrap();
            }
            super::Instruction::SysCall(ident, len) => {
                todo!()
            }
        }
    }
    asm.assemble(0)
        .unwrap()
        .into_iter()
        .fold(Vec::new(), |mut bytes, int| {
            bytes.extend(int.to_le_bytes());
            bytes
        })
}
