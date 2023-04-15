#[derive(Debug)]
enum Instruction {
    Const,
    Add,
    Sub,
    Mul,
    Le,
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
            11 => Ok(Instruction::Sub),
            12 => Ok(Instruction::Le),
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
                Instruction::Var => {
                    let offset = self.code[self.pc as usize + 1];
                    let var = self.stack[(self.sp - offset) as usize];
                    self.stack[(self.sp + 1) as usize] = var;
                    self.sp += 1;
                    self.pc += 2;
                }
                Instruction::Pop => {
                    self.sp -= 1;
                    self.pc += 1;
                }
                Instruction::Swap => {
                    let top = self.stack[self.sp as usize];
                    self.stack[self.sp as usize] = self.stack[self.sp as usize - 1];
                    self.stack[self.sp as usize - 1] = top;
                    self.pc += 1;
                }
                Instruction::Call => {
                    let ret_addr = self.pc + 3;
                    self.push(ret_addr);

                    let target = self.code[self.pc as usize + 1];
                    let _ = self.code[self.pc as usize + 2];
                    self.pc = target;
                }
                Instruction::Ret => {
                    let n = self.code[self.pc as usize + 1];

                    let ret_value = self.pop();
                    let ret_addr = self.pop();

                    self.sp -= n;

                    self.push(ret_value);
                    self.pc = ret_addr;
                }
                Instruction::Goto => {
                    let target_addr = self.operand();
                    self.pc = target_addr;
                }
                Instruction::IfZero => {
                    let else_addr = self.operand();
                    let value = self.pop();
                    if value == 0 {
                        self.pc = else_addr;
                    } else {
                        self.pc += 1;
                    }
                }
                Instruction::Exit => {
                    assert_eq!(self.sp, 1);
                    return self.stack[self.sp as usize];
                }
                Instruction::Sub => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a - b);
                    self.pc += 1;
                }
                Instruction::Le => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a <= b) as u32);
                    self.pc += 1;
                }
            }
        }
    }

    fn push(&mut self, value: u32) {
        self.sp += 1;
        self.stack[self.sp as usize] = value;
    }

    fn pop(&mut self) -> u32 {
        let value = self.stack[self.sp as usize];
        self.sp -= 1;
        value
    }

    fn operand(&mut self) -> u32 {
        self.pc += 1;
        self.code[self.pc as usize]
    }
}

fn main() {
    let bytecode = std::fs::read("bytecode.bin").unwrap();
    let mut vm = Vm::create(bytecode);
    let result = vm.start();

    println!("result: {}", result);
}
