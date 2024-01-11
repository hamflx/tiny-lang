#[derive(Debug)]
pub(crate) enum Instruction {
    Const,
    Add,
    Sub,
    Mul,
    Le,
    Ge,
    Lt,
    Gt,
    And,
    Or,
    Not,
    Var,
    Pop,
    Swap,
    Call,
    SysCall,
    Ret,
    Goto,
    IfZero,
    Exit,
}

impl Instruction {
    pub(crate) fn code(&self) -> u32 {
        match self {
            Instruction::Const => 0,
            Instruction::Add => 1,
            Instruction::Mul => 2,
            Instruction::Var => 3,
            Instruction::Pop => 4,
            Instruction::Swap => 5,
            Instruction::Call => 6,
            Instruction::Ret => 7,
            Instruction::Goto => 8,
            Instruction::IfZero => 9,
            Instruction::Exit => 10,
            Instruction::Sub => 11,
            Instruction::Le => 12,
            Instruction::SysCall => 13,
            Instruction::Ge => 14,
            Instruction::Lt => 15,
            Instruction::Gt => 16,
            Instruction::And => 17,
            Instruction::Or => 18,
            Instruction::Not => 19,
        }
    }
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
            13 => Ok(Instruction::SysCall),
            14 => Ok(Instruction::Ge),
            15 => Ok(Instruction::Lt),
            16 => Ok(Instruction::Gt),
            17 => Ok(Instruction::And),
            18 => Ok(Instruction::Or),
            19 => Ok(Instruction::Not),
            _ => Err("invalid instruction"),
        }
    }
}

pub(crate) struct SysCall {
    ptr: *const (),
    param_count: usize,
}

impl SysCall {
    pub(crate) fn new(ptr: *const (), param_count: usize) -> Self {
        Self { ptr, param_count }
    }
}

pub(crate) struct Vm {
    code: Vec<u32>,
    stack: Vec<u32>,
    pc: u32,
    sp: u32,
    syscalls: Vec<SysCall>,
}

impl Vm {
    pub(crate) fn create(code: Vec<u8>) -> Self {
        let code = code
            .chunks(4)
            .map(|chunk| u32::from_le_bytes(chunk.try_into().unwrap()))
            .collect();
        Self {
            code,
            stack: vec![0; 4096],
            pc: 0,
            sp: 0,
            syscalls: vec![],
        }
    }

    pub(crate) fn add_sys_call(&mut self, syscall: SysCall) {
        self.syscalls.push(syscall);
    }

    pub(crate) fn start(&mut self) -> u32 {
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
                Instruction::SysCall => {
                    let call_no = self.code[self.pc as usize + 1];
                    let len = self.code[self.pc as usize + 2];
                    let syscall = &self.syscalls[call_no as usize];
                    let result = match len {
                        0 => (unsafe { std::mem::transmute::<_, fn() -> u32>(syscall.ptr) })(),
                        1 => (unsafe { std::mem::transmute::<_, fn(u32) -> u32>(syscall.ptr) })(
                            self.pop(),
                        ),
                        2 => {
                            let ptr = syscall.ptr;
                            let p2 = self.pop();
                            let p1 = self.pop();
                            (unsafe { std::mem::transmute::<_, fn(u32, u32) -> u32>(ptr) })(p1, p2)
                        }
                        _ => todo!(),
                    };
                    self.push(result);
                    self.pc += 3;
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
                Instruction::Ge => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a >= b) as u32);
                    self.pc += 1;
                }
                Instruction::Lt => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a < b) as u32);
                    self.pc += 1;
                }
                Instruction::Gt => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a > b) as u32);
                    self.pc += 1;
                }
                Instruction::And => {
                    let b = self.pop() != 0;
                    let a = self.pop() != 0;
                    self.push((a && b) as u32);
                    self.pc += 1;
                }
                Instruction::Or => {
                    let b = self.pop() != 0;
                    let a = self.pop() != 0;
                    self.push((a || b) as u32);
                    self.pc += 1;
                }
                Instruction::Not => {
                    let a = self.pop() != 0;
                    self.push(!a as u32);
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
