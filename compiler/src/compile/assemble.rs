use std::process::Command;

use crate::{
    compile,
    parser::parse_code,
    resolution::{self, Identifier},
};

#[derive(Clone, Debug)]
pub(crate) enum Reg {
    Al,
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsp,
    Rdi,
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reg::Al => write!(f, "al"),
            Reg::Rax => write!(f, "rax"),
            Reg::Rbx => write!(f, "rbx"),
            Reg::Rcx => write!(f, "rcx"),
            Reg::Rdx => write!(f, "rdx"),
            Reg::Rsp => write!(f, "rsp"),
            Reg::Rdi => write!(f, "rdi"),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum MoveArg {
    Constant(i64),
    Reg(Reg),
    Addr(i64),
    RegOffset {
        base: Reg,
        index: Reg,
        scale: usize,
        disp: usize,
    },
}

#[derive(Clone, Debug)]
pub(crate) enum AndArg {
    Constant(i64),
    Reg(Reg),
}

impl std::fmt::Display for AndArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AndArg::Constant(i) => write!(f, "{i}"),
            AndArg::Reg(reg) => write!(f, "{reg}"),
        }
    }
}

impl std::fmt::Display for MoveArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MoveArg::Constant(i) => write!(f, "{i}"),
            MoveArg::Reg(reg) => write!(f, "{reg}"),
            MoveArg::Addr(addr) => write!(f, "[{addr}]"),
            MoveArg::RegOffset {
                base,
                index,
                scale,
                disp,
            } => write!(f, "[{}+{}*{}+{}]", base, index, scale, disp),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Instruction {
    Mov(Reg, MoveArg),
    Movzx(Reg, Reg),
    Push(Reg),
    Pop(Reg),
    Add(Reg, Reg),
    Sub(Reg, Reg),
    Or(Reg, Reg),
    And(Reg, AndArg),
    Not(Reg),
    Mul(Reg),
    Test(Reg, Reg),
    Cmp(Reg, Reg),
    Setnae(Reg),
    Setna(Reg),
    Setae(Reg),
    Seta(Reg),
    Retn(usize),
    Ret,
    Int(usize),
    SysCall,
    Label(Identifier),
    Call(Identifier),
    Goto(Identifier),
    Je(Identifier),
}

#[test]
fn test_emit_assembly() {
    assert_eq!(run("fn main () -> usize {3 + 1}"), 4);
}

pub(crate) fn run(code: &str) -> u64 {
    let prog = parse_code(code);
    let prog = resolution::compile_program(&prog, vec![]);
    let instrs = compile::compile_program(prog);
    let instrs = translate(instrs);
    let bytes = compile(instrs);
    let dir = std::env::current_dir().unwrap();
    let asm_path = dir.join("machine_code.s");
    let obj_path = dir.join("machine_code.o");
    let exe_path = dir.join("machine_code");
    std::fs::write(&asm_path, bytes.as_bytes()).unwrap();
    let output = String::from_utf8(
        Command::new("clang")
            .args([
                "-nostdlib",
                "-c",
                format!("{}", asm_path.display()).as_str(),
                "-o",
                format!("{}", obj_path.display()).as_str(),
            ])
            .output()
            .unwrap()
            .stdout,
    )
    .unwrap();
    println!("{}", output);
    let output = String::from_utf8(
        Command::new("clang")
            .args([
                "-nostdlib",
                format!("{}", obj_path.display()).as_str(),
                "-o",
                format!("{}", exe_path.display()).as_str(),
            ])
            .output()
            .unwrap()
            .stdout,
    )
    .unwrap();
    println!("{}", output);
    Command::new(exe_path)
        .spawn()
        .unwrap()
        .wait()
        .unwrap()
        .code()
        .unwrap() as _
}

pub(crate) fn translate(instrs: Vec<super::Instruction>) -> Vec<Instruction> {
    instrs.into_iter().fold(Vec::new(), |mut list, instr| {
        let extend = match instr {
            super::Instruction::Label(id) => [Instruction::Label(id)].to_vec(),
            super::Instruction::Const(i) => [
                Instruction::Mov(Reg::Rax, MoveArg::Constant(i as _)),
                Instruction::Push(Reg::Rax),
            ]
            .to_vec(),
            super::Instruction::Add => [
                Instruction::Pop(Reg::Rax),
                Instruction::Pop(Reg::Rbx),
                Instruction::Add(Reg::Rax, Reg::Rbx),
                Instruction::Push(Reg::Rax),
            ]
            .to_vec(),
            super::Instruction::Sub => [
                Instruction::Pop(Reg::Rbx),
                Instruction::Pop(Reg::Rax),
                Instruction::Sub(Reg::Rax, Reg::Rbx),
                Instruction::Push(Reg::Rax),
            ]
            .to_vec(),
            super::Instruction::Mul => [
                Instruction::Pop(Reg::Rax),
                Instruction::Pop(Reg::Rbx),
                Instruction::Mul(Reg::Rbx),
                Instruction::Push(Reg::Rax),
            ]
            .to_vec(),
            super::Instruction::Gt => [
                Instruction::Pop(Reg::Rcx),
                Instruction::Pop(Reg::Rax),
                Instruction::Cmp(Reg::Rax, Reg::Rcx),
                Instruction::Seta(Reg::Al),
                Instruction::Movzx(Reg::Rcx, Reg::Al),
                Instruction::Push(Reg::Rcx),
            ]
            .to_vec(),
            super::Instruction::Lt => [
                Instruction::Pop(Reg::Rcx),
                Instruction::Pop(Reg::Rax),
                Instruction::Cmp(Reg::Rax, Reg::Rcx),
                Instruction::Setnae(Reg::Al),
                Instruction::Movzx(Reg::Rcx, Reg::Al),
                Instruction::Push(Reg::Rcx),
            ]
            .to_vec(),
            super::Instruction::Ge => [
                Instruction::Pop(Reg::Rcx),
                Instruction::Pop(Reg::Rax),
                Instruction::Cmp(Reg::Rax, Reg::Rcx),
                Instruction::Setae(Reg::Al),
                Instruction::Movzx(Reg::Rcx, Reg::Al),
                Instruction::Push(Reg::Rcx),
            ]
            .to_vec(),
            super::Instruction::Le => [
                Instruction::Pop(Reg::Rcx),
                Instruction::Pop(Reg::Rax),
                Instruction::Cmp(Reg::Rax, Reg::Rcx),
                Instruction::Setna(Reg::Al),
                Instruction::Movzx(Reg::Rcx, Reg::Al),
                Instruction::Push(Reg::Rcx),
            ]
            .to_vec(),
            super::Instruction::And => [
                Instruction::Pop(Reg::Rax),
                Instruction::Pop(Reg::Rcx),
                Instruction::And(Reg::Rax, AndArg::Reg(Reg::Rcx)),
                Instruction::And(Reg::Rax, AndArg::Constant(1)),
                Instruction::Push(Reg::Rax),
            ]
            .to_vec(),
            super::Instruction::Or => [
                Instruction::Pop(Reg::Rax),
                Instruction::Pop(Reg::Rcx),
                Instruction::Or(Reg::Rax, Reg::Rcx),
                Instruction::And(Reg::Rax, AndArg::Constant(1)),
                Instruction::Push(Reg::Rax),
            ]
            .to_vec(),
            super::Instruction::Not => [
                Instruction::Pop(Reg::Rax),
                Instruction::Not(Reg::Rax),
                Instruction::And(Reg::Rax, AndArg::Constant(1)),
                Instruction::Push(Reg::Rax),
            ]
            .to_vec(),
            super::Instruction::Var(i) => [
                Instruction::Mov(Reg::Rcx, MoveArg::Constant(i as _)),
                Instruction::Mov(
                    Reg::Rax,
                    MoveArg::RegOffset {
                        base: Reg::Rax,
                        index: Reg::Rcx,
                        scale: 8,
                        disp: 0,
                    },
                ),
                Instruction::Push(Reg::Rax),
            ]
            .to_vec(),
            super::Instruction::Pop => [Instruction::Pop(Reg::Rax)].to_vec(),
            super::Instruction::Swap => [
                Instruction::Pop(Reg::Rax),
                Instruction::Pop(Reg::Rcx),
                Instruction::Push(Reg::Rax),
                Instruction::Push(Reg::Rcx),
            ]
            .to_vec(),
            super::Instruction::Ldstr(_) => todo!(),
            super::Instruction::Bytes(_) => todo!(),
            super::Instruction::Call(id, _) => {
                [Instruction::Call(id), Instruction::Push(Reg::Rax)].to_vec()
            }
            super::Instruction::SysCall(_, _) => todo!(),
            super::Instruction::Ret(0) => [Instruction::Pop(Reg::Rax), Instruction::Ret].to_vec(),
            super::Instruction::Ret(i) => {
                [Instruction::Pop(Reg::Rax), Instruction::Retn(i as _)].to_vec()
            }
            super::Instruction::Goto(addr) => [Instruction::Goto(addr)].to_vec(),
            super::Instruction::IfZero(addr) => [
                Instruction::Pop(Reg::Rax),
                Instruction::Test(Reg::Rax, Reg::Rax),
                Instruction::Je(addr),
            ]
            .to_vec(),
            super::Instruction::Exit => [
                Instruction::Pop(Reg::Rdi),
                Instruction::Mov(Reg::Rax, MoveArg::Constant(60)),
                Instruction::SysCall,
            ]
            .to_vec(),
        };
        list.extend(extend);
        list
    })
}

pub(crate) fn compile(instrs: Vec<Instruction>) -> String {
    let mut asm_result = ".intel_syntax noprefix\n".to_string();
    for instr in instrs {
        let asm_text = match instr {
            Instruction::Label(ident) => {
                // #[cfg(not(target_os = "linux"))]
                #[cfg(target_os = "linux")]
                let modifiers = format!(".type {},@function\n.global {}\n", ident, ident);
                format!("{}{}:", modifiers, ident)
            }
            Instruction::Mov(reg, arg) => format!("mov {}, {}", reg, arg),
            Instruction::Movzx(reg, arg) => format!("mov {}, {}", reg, arg),
            Instruction::Push(reg) => format!("push {reg}"),
            Instruction::Pop(reg) => format!("pop {reg}"),
            Instruction::Add(op1, op2) => format!("add {op1}, {op2}"),
            Instruction::Sub(op1, op2) => format!("sub {op1}, {op2}"),
            Instruction::Mul(op) => format!("mul {op}"),
            Instruction::Test(op1, op2) => format!("test {op1}, {op2}"),
            Instruction::Cmp(op1, op2) => format!("cmp {op1}, {op2}"),
            Instruction::Setna(reg) => format!("setna {reg}"),
            Instruction::Retn(n) => format!("ret {n}"),
            Instruction::Ret => format!("ret"),
            Instruction::Call(addr) => format!("call {}", addr),
            Instruction::Goto(addr) => format!("goto {}", addr),
            Instruction::Je(addr) => format!("je {}", addr),
            Instruction::Or(op1, op2) => format!("or {op1}, {op2}"),
            Instruction::And(op1, op2) => format!("and {op1}, {op2}"),
            Instruction::Not(op) => format!("not {op}"),
            Instruction::Setnae(op) => format!("setnae {op}"),
            Instruction::Setae(op) => format!("setae {op}"),
            Instruction::Seta(op) => format!("seta {op}"),
            Instruction::Int(no) => format!("int 0x{:x}", no),
            Instruction::SysCall => format!("syscall"),
        };
        asm_result.push_str(&asm_text);
        asm_result.push('\n');
    }
    asm_result
}
