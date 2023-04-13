#[derive(Debug)]
enum Instruction {
    Const,
    Add,
    Mul,
    Var,
    Pop,
    Swap,
    Call,
    Ret,
    Goto,
    IfZero,
    Exit,
}

impl TryFrom<u32> for Instruction {
    type Error = &'static str;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Instruction::Const),
            1 => Ok(Instruction::Add),
            2 => Ok(Instruction::Mul),
            3 => Ok(Instruction::Var),
            4 => Ok(Instruction::Pop),
            5 => Ok(Instruction::Swap),
            6 => Ok(Instruction::Call),
            7 => Ok(Instruction::Ret),
            8 => Ok(Instruction::Goto),
            9 => Ok(Instruction::IfZero),
            10 => Ok(Instruction::Exit),
            _ => Err("invalid instruction"),
        }
    }
}

struct Vm {
    code: Vec<u32>,
    stack: Vec<u32>,
    pc: u32,
    sp: u32,
}

impl Vm {
    fn create(code: Vec<u8>) -> Self {
        let code = code
            .chunks(4)
            .map(|chunk| u32::from_le_bytes(chunk.try_into().unwrap()))
            .collect();
        Self {
            code,
            stack: vec![0; 4096],
            pc: 0,
            sp: 0,
        }
    }

    fn start(&mut self) -> u32 {
        loop {
            let instr = self.code[self.pc as usize];
            let instr: Instruction = instr.try_into().unwrap();
            match instr {
                Instruction::Const => {
                    let operand = self.code[self.pc as usize + 1];
                    self.stack[self.sp as usize + 1] = operand;
                    self.sp += 1;
                    self.pc += 2;
                }
                Instruction::Add => {
                    let op1 = self.stack[self.sp as usize];
                    let op2 = self.stack[self.sp as usize - 1];
                    self.stack[self.sp as usize - 1] = op1 + op2;
                    self.sp -= 1;
                    self.pc += 1;
                }
                Instruction::Mul => {
                    let op1 = self.stack[self.sp as usize];
                    let op2 = self.stack[self.sp as usize - 1];
                    self.stack[self.sp as usize - 1] = op1 * op2;
                    self.sp -= 1;
                    self.pc += 1;
                }
                Instruction::Var => todo!(),
                Instruction::Pop => todo!(),
                Instruction::Swap => todo!(),
                Instruction::Call => todo!(),
                Instruction::Ret => todo!(),
                Instruction::Goto => todo!(),
                Instruction::IfZero => todo!(),
                Instruction::Exit => {
                    assert_eq!(self.sp, 1);
                    return self.stack[self.sp as usize];
                }
            }
        }
    }
}

fn main() {
    let bytecode = std::fs::read("bytecode.bin").unwrap();
    let mut vm = Vm::create(bytecode);
    let result = vm.start();

    println!("result: {}", result);
}
