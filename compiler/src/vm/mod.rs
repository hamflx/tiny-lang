#[derive(Debug)]
pub(crate) enum Instruction {
    Const,
    Ldstr,
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
            Instruction::Ldstr => 20,
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
            20 => Ok(Instruction::Ldstr),
            _ => Err("invalid instruction"),
        }
    }
}

pub trait ToAny: 'static {
    fn as_any(&self) -> &dyn std::any::Any;
}

impl<T: 'static> ToAny for T {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

pub(crate) trait CallContext: ToAny {}

pub(crate) struct SysCall {
    ptr: *const (),
    param_count: usize,
    pub(crate) ctx: Option<Box<dyn CallContext>>,
}

impl SysCall {
    pub(crate) fn new(
        ptr: *const (),
        param_count: usize,
        ctx: Option<Box<dyn CallContext>>,
    ) -> Self {
        Self {
            ptr,
            param_count,
            ctx,
        }
    }
}

pub(crate) struct VmStack {
    sp: u32,
    stack: Vec<u64>,
}

impl VmStack {
    pub(crate) fn new() -> Self {
        Self {
            sp: 0,
            stack: vec![0; 4096],
        }
    }

    pub(crate) fn push(&mut self, i: u64) {
        self.sp += 1;
        self.stack[self.sp as usize] = i;
    }

    pub(crate) fn pop(&mut self) -> u64 {
        let i = self.stack[self.sp as usize];
        self.sp -= 1;
        i
    }
}

pub(crate) struct Vm {
    code: Vec<u32>,
    stack: VmStack,
    pc: u32,
    sys_calls: Vec<SysCall>,
}

impl Vm {
    pub(crate) fn create(code: Vec<u8>) -> Self {
        let code = code
            .chunks(4)
            .map(|chunk| u32::from_le_bytes(chunk.try_into().unwrap()))
            .collect();
        Self {
            code,
            stack: VmStack::new(),
            pc: 0,
            sys_calls: vec![],
        }
    }

    pub(crate) fn add_sys_call(&mut self, sys_call: SysCall) {
        self.sys_calls.push(sys_call);
    }

    pub(crate) fn start(&mut self) -> u64 {
        loop {
            let instr = self.code[self.pc as usize];
            let instr: Instruction = instr.try_into().unwrap();
            match instr {
                Instruction::Const => {
                    let operand = self.code[self.pc as usize + 1];
                    self.stack.push(operand as _);
                    self.pc += 2;
                }
                Instruction::Ldstr => {
                    let operand = self.operand();
                    self.stack.push(unsafe {
                        &self.code[operand as usize] as *const u32 as *const u8
                    } as _);
                    self.pc += 1;
                }
                Instruction::Add => {
                    let op1 = self.stack.pop();
                    let op2 = self.stack.pop();
                    self.stack.push(op1 + op2);
                    self.pc += 1;
                }
                Instruction::Mul => {
                    let op1 = self.stack.pop();
                    let op2 = self.stack.pop();
                    self.stack.push(op1 * op2);
                    self.pc += 1;
                }
                Instruction::Var => {
                    let offset = self.code[self.pc as usize + 1];
                    let var = self.stack.stack[(self.stack.sp - offset) as usize];
                    self.stack.stack[(self.stack.sp + 1) as usize] = var;
                    self.stack.sp += 1;
                    self.pc += 2;
                }
                Instruction::Pop => {
                    self.stack.sp -= 1;
                    self.pc += 1;
                }
                Instruction::Swap => {
                    let top = self.stack.stack[self.stack.sp as usize];
                    self.stack.stack[self.stack.sp as usize] =
                        self.stack.stack[self.stack.sp as usize - 1];
                    self.stack.stack[self.stack.sp as usize - 1] = top;
                    self.pc += 1;
                }
                Instruction::Call => {
                    let ret_addr = self.pc + 3;
                    self.push(ret_addr as _);

                    let target = self.code[self.pc as usize + 1];
                    let _ = self.code[self.pc as usize + 2];
                    self.pc = target;
                }
                Instruction::SysCall => {
                    let call_no = self.code[self.pc as usize + 1];
                    let len = self.code[self.pc as usize + 2];
                    let syscall = &self.sys_calls[call_no as usize];
                    let result = match len {
                        0 => {
                            (unsafe { std::mem::transmute::<_, fn(&SysCall) -> u64>(syscall.ptr) })(
                                syscall,
                            )
                        }
                        1 => (unsafe {
                            std::mem::transmute::<_, fn(&SysCall, u64) -> u64>(syscall.ptr)
                        })(syscall, self.stack.pop()),
                        2 => {
                            let p2 = self.stack.pop();
                            let p1 = self.stack.pop();
                            (unsafe {
                                std::mem::transmute::<_, fn(&SysCall, u64, u64) -> u64>(syscall.ptr)
                            })(syscall, p1, p2)
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

                    self.stack.sp -= n;

                    self.push(ret_value);
                    self.pc = ret_addr as u32;
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
                    assert_eq!(self.stack.sp, 1);
                    return self.stack.stack[self.stack.sp as usize];
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
                    self.push((a <= b) as _);
                    self.pc += 1;
                }
                Instruction::Ge => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a >= b) as _);
                    self.pc += 1;
                }
                Instruction::Lt => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a < b) as _);
                    self.pc += 1;
                }
                Instruction::Gt => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a > b) as _);
                    self.pc += 1;
                }
                Instruction::And => {
                    let b = self.pop() != 0;
                    let a = self.pop() != 0;
                    self.push((a && b) as _);
                    self.pc += 1;
                }
                Instruction::Or => {
                    let b = self.pop() != 0;
                    let a = self.pop() != 0;
                    self.push((a || b) as _);
                    self.pc += 1;
                }
                Instruction::Not => {
                    let a = self.pop() != 0;
                    self.push(!a as _);
                    self.pc += 1;
                }
            }
        }
    }

    fn push(&mut self, value: u64) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> u64 {
        self.stack.pop()
    }

    fn operand(&mut self) -> u32 {
        self.pc += 1;
        self.code[self.pc as usize]
    }
}
