let findIndex = (list: list<'a>, item: 'a) => {
  let rec findWithIndex = (list: list<'a>, index: int) => {
    switch list {
    | list{} => raise(Not_found)
    | list{head, ...tail} => head === item ? index : findWithIndex(tail, index + 1)
    }
  }
  findWithIndex(list, 0)
}

module Ast = {
  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(string)
    | Let(string, expr, expr)
    | Fn(list<string>, expr)
    | App(expr, list<expr>)

  type senv = list<(string, int)>

  let eval = (expr: expr) => {
    let rec eval_inner = (expr: expr, env: senv) => {
      switch expr {
      | Cst(i) => i
      | Var(name) => List.assoc(name, env)
      | Add(a, b) => eval_inner(a, env) + eval_inner(b, env)
      | Mul(a, b) => eval_inner(a, env) * eval_inner(b, env)
      | Let(name, a, b) => eval_inner(b, list{(name, eval_inner(a, env)), ...env})
      | _ => assert false
      }
    }
    eval_inner(expr, list{})
  }
}

module Nameless = {
  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(int)
    | Let(expr, expr)

  let compile = (expr: Ast.expr): expr => {
    let rec compile_inner = (expr: Ast.expr, cenv: list<string>): expr => {
      switch expr {
      | Ast.Cst(i) => Cst(i)
      | Ast.Add(a, b) => Add(compile_inner(a, cenv), compile_inner(b, cenv))
      | Ast.Mul(a, b) => Mul(compile_inner(a, cenv), compile_inner(b, cenv))
      | Ast.Var(name) => Var(findIndex(cenv, name))
      | Ast.Let(name, e1, e2) =>
        Let(compile_inner(e1, cenv), compile_inner(e2, list{name, ...cenv}))
      | _ => assert false
      }
    }
    compile_inner(expr, list{})
  }
}

module Indexed = {
  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(int)
    | Let(expr, expr)

  type sv = Slocal | Stmp
  type senv = list<sv>

  let find_local_index = (env: senv, local_index: int) => {
    let rec findWithIndex = (env: senv, local_index: int, stack_index: int) => {
      switch (env, local_index) {
      | (list{head, ..._}, 0) if head === Slocal => stack_index
      | (list{head, ...tail}, 0) =>
        findWithIndex(tail, head === Slocal ? local_index - 1 : local_index, stack_index + 1)
      | _ => raise(Not_found)
      }
    }
    findWithIndex(env, local_index, 0)
  }

  let compile = (expr: Nameless.expr): expr => {
    let rec go = (expr: Nameless.expr, env: senv): expr => {
      switch expr {
      | Nameless.Cst(i) => Cst(i)
      | Nameless.Add(a, b) => Add(go(a, env), go(b, list{Stmp, ...env}))
      | Nameless.Mul(a, b) => Mul(go(a, env), go(b, list{Stmp, ...env}))
      | Nameless.Var(i) => Var(find_local_index(env, i))
      | Nameless.Let(e1, e2) => Let(go(e1, env), go(e2, list{Slocal, ...env}))
      }
    }
    go(expr, list{})
  }
}

module Vm = {
  type instr = Cst(int) | Add | Mul | Var(int) | Pop | Swap
  type instrs = list<instr>
  type operand = int
  type stack = list<operand>

  let rec eval = (instrs: instrs, stk: stack) => {
    switch (instrs, stk) {
    | (list{}, list{operand}) => operand
    | (list{Cst(i), ...rest}, _) => eval(rest, list{i, ...stk})
    | (list{Var(i), ...rest}, _) => eval(rest, list{List.nth(stk, i), ...stk})
    | (list{Add, ...rest}, list{a, b, ...stk}) => eval(rest, list{a + b, ...stk})
    | (list{Mul, ...rest}, list{a, b, ...stk}) => eval(rest, list{a * b, ...stk})
    | (list{Pop, ...rest}, list{_, ...stk}) => eval(rest, stk)
    | (list{Swap, ...rest}, list{a, b, ...stk}) => eval(rest, list{b, a, ...stk})
    | _ => assert false
    }
  }

  type sv = Slocal(string) | Stmp
  type senv = list<sv>

  let find_local_index = (env: senv, name: string) => {
    let rec find_recursive = (env: senv, stack_index: int) => {
      switch env {
      | list{Slocal(head), ..._} if head === name => stack_index
      | list{_, ...tail} => find_recursive(tail, stack_index + 1)
      | _ => raise(Not_found)
      }
    }
    find_recursive(env, 0)
  }

  let compile_ast = (expr: Ast.expr): instrs => {
    let rec compile_inner = (expr: Ast.expr, env: senv): instrs => {
      switch expr {
      | Ast.Cst(i) => list{Cst(i)}
      | Ast.Add(e1, e2) =>
        list{...compile_inner(e1, env), ...compile_inner(e2, list{Stmp, ...env}), Add}
      | Ast.Mul(e1, e2) =>
        list{...compile_inner(e1, env), ...compile_inner(e2, list{Stmp, ...env}), Mul}
      | Ast.Var(name) => list{Var(find_local_index(env, name))}
      | Ast.Let(name, e1, e2) =>
        list{...compile_inner(e1, env), ...compile_inner(e2, list{Slocal(name), ...env}), Swap, Pop}
      | _ => assert false
      }
    }
    compile_inner(expr, list{})
  }

  let rec compile_nameless = (expr: Nameless.expr): instrs => {
    switch expr {
    | Nameless.Cst(i) => list{Cst(i)}
    | Nameless.Add(e1, e2) => list{...compile_nameless(e1), ...compile_nameless(e2), Add}
    | Nameless.Mul(e1, e2) => list{...compile_nameless(e1), ...compile_nameless(e2), Mul}
    | Nameless.Var(i) => list{Var(i)}
    | Nameless.Let(e1, e2) => list{...compile_nameless(e1), ...compile_nameless(e2), Swap, Pop}
    }
  }

  let rec compile_indexed = (expr: Indexed.expr): instrs => {
    switch expr {
    | Indexed.Cst(i) => list{Cst(i)}
    | Indexed.Add(e1, e2) => list{...compile_indexed(e1), ...compile_indexed(e2), Add}
    | Indexed.Mul(e1, e2) => list{...compile_indexed(e1), ...compile_indexed(e2), Mul}
    | Indexed.Var(i) => list{Var(i)}
    | Indexed.Let(e1, e2) => list{...compile_indexed(e1), ...compile_indexed(e2), Swap, Pop}
    }
  }

  let print = (instrs: instrs) => {
    for i in 1 to List.length(instrs) {
      let instr_text = switch List.nth(instrs, i - 1) {
      | Cst(i) => "const " ++ Belt.Int.toString(i)
      | Add => "add"
      | Mul => "mul"
      | Var(i) => "var " ++ Belt.Int.toString(i)
      | Pop => "pop"
      | Swap => "swap"
      }
      Js.log(instr_text)
    }
  }
}

module Native = {
  type reg = Rax | Rbx | Rcx | Rdx | Rsp
  type mov_arg =
    Constant(int) | Reg(reg) | Addr(int) | RegOffset({base: reg, index: reg, scale: int, disp: int})
  type instr = Mov(reg, mov_arg) | Push(reg) | Pop(reg) | Add(reg, reg) | Mul(reg, reg) | Ret
  type instrs = list<instr>

  type sv = Slocal(string) | Stmp
  type senv = list<sv>

  let find_local_index = (env: senv, name: string) => {
    let rec find_recursive = (env: senv, stack_index: int) => {
      switch env {
      | list{Slocal(head), ..._} if head === name => stack_index
      | list{_, ...tail} => find_recursive(tail, stack_index + 1)
      | _ => raise(Not_found)
      }
    }
    find_recursive(env, 0)
  }

  let compile_ast = (expr: Ast.expr): instrs => {
    let rec compile_inner = (expr: Ast.expr, env: senv): instrs => {
      switch expr {
      | Ast.Cst(i) => list{Mov(Rax, Constant(i)), Push(Rax)}
      | Ast.Add(e1, e2) =>
        list{
          ...compile_inner(e1, env),
          ...compile_inner(e2, list{Stmp, ...env}),
          Pop(Rax),
          Pop(Rbx),
          Add(Rax, Rbx),
          Push(Rax),
        }
      | Ast.Mul(e1, e2) =>
        list{
          ...compile_inner(e1, env),
          ...compile_inner(e2, list{Stmp, ...env}),
          Pop(Rax),
          Pop(Rbx),
          Mul(Rax, Rbx),
          Push(Rax),
        }
      | Ast.Var(name) =>
        list{
          Mov(Rbx, Constant(find_local_index(env, name))),
          Mov(Rax, RegOffset({base: Rsp, index: Rbx, scale: 8, disp: 0})),
          Push(Rax),
        }
      | Ast.Let(name, e1, e2) =>
        list{
          ...compile_inner(e1, env),
          ...compile_inner(e2, list{Slocal(name), ...env}),
          Pop(Rax),
          Pop(Rbx),
          Push(Rax),
        }
      | _ => assert false
      }
    }
    list{...compile_inner(expr, list{}), Pop(Rax), Ret}
  }

  let generate = (instrs: instrs) => {
    let generate_instr = (instr: instr) => {
      switch instr {
      | Mov(reg, Constant(i)) if i < 0xff =>
        list{
          0x48,
          0xc7,
          switch reg {
          | Rax => 0xc0
          | Rbx => 0xc3
          | Rcx => 0xc1
          | Rdx => 0xc2
          | _ => assert false
          },
          i,
          0x00,
          0x00,
          0x00,
        }
      | Mov(target, RegOffset({base, index, scale: 8, disp})) if disp < 0x80 =>
        switch (base, index) {
        | (Rsp, index) =>
          list{
            0x48,
            0x8b,
            switch target {
            | Rax => 0x44
            | Rbx => 0x5c
            | Rcx => 0x4c
            | Rdx => 0x54
            | _ => assert false
            },
            switch index {
            | Rax => 0xc4
            | Rbx => 0xdc
            | Rcx => 0xcc
            | Rdx => 0xd4
            | _ => assert false
            },
            disp,
          }
        | _ => assert false
        }
      | Push(Rax) => list{0x50}
      | Push(Rbx) => list{0x53}
      | Push(Rcx) => list{0x51}
      | Push(Rdx) => list{0x52}
      | Pop(Rax) => list{0x58}
      | Pop(Rbx) => list{0x5b}
      | Pop(Rcx) => list{0x59}
      | Pop(Rdx) => list{0x5a}
      | Add(Rax, Rbx) => list{0x48, 0x01, 0xd8}
      | Ret => list{0xc3}
      | _ => assert false
      }
    }
    let rec generate_inner = (instrs: instrs) => {
      switch instrs {
      | list{} => list{}
      | list{head, ...tail} => list{...generate_instr(head), ...generate_inner(tail)}
      }
    }
    generate_inner(instrs)
  }

  let to_hex = (code: list<int>) => {
    code->Belt.List.reduce("", (sum, item) =>
      sum ++ ("00" ++ Js.Int.toStringWithRadix(item, ~radix=16))->Js.String2.sliceToEnd(~from=-2)
    )
  }

  let to_reg_str = (reg: reg) => {
    switch reg {
    | Rax => "rax"
    | Rbx => "rbx"
    | Rcx => "rcx"
    | Rdx => "rdx"
    | Rsp => "rsp"
    }
  }

  let to_mov_arg_str = (reg: mov_arg) => {
    switch reg {
    | Constant(i) => Belt.Int.toString(i)
    | Reg(reg) => to_reg_str(reg)
    | Addr(i) => "[" ++ Belt.Int.toString(i) ++ "]"
    | RegOffset({base, index, scale, disp}) =>
      "[" ++
      to_reg_str(base) ++
      "+" ++
      to_reg_str(index) ++
      "*" ++
      Belt.Int.toString(scale) ++
      "+" ++
      Belt.Int.toString(disp) ++ "]"
    }
  }

  let print = (instrs: instrs) => {
    for i in 1 to List.length(instrs) {
      let instr_text = switch List.nth(instrs, i - 1) {
      | Mov(reg, arg) => "mov " ++ to_reg_str(reg) ++ ", " ++ to_mov_arg_str(arg)
      | Push(reg) => "push " ++ to_reg_str(reg)
      | Pop(reg) => "pop " ++ to_reg_str(reg)
      | Add(r1, r2) => "add " ++ to_reg_str(r1) ++ ", " ++ to_reg_str(r2)
      | Mul(r1, r2) => "mul " ++ to_reg_str(r1) ++ ", " ++ to_reg_str(r2)
      | Ret => "ret"
      }
      Js.log(instr_text)
    }
  }
}

let my_expr = Ast.Add(
  Ast.Add(Ast.Cst(1), Ast.Let("x", Ast.Cst(2), Ast.Add(Ast.Var("x"), Ast.Var("x")))),
  Ast.Cst(3),
)

let my_nameless = Nameless.compile(my_expr)
let my_indexed = Indexed.compile(my_nameless)
let instrs = Vm.compile_indexed(my_indexed)

let instrs2 = Vm.compile_ast(my_expr)

let assembly = Native.compile_ast(my_expr)

Js.log("==> multi-level ir:")
Vm.print(instrs)

Js.log("==> single pass:")
Vm.print(instrs2)

Js.log(Ast.eval(my_expr))
Js.log(Vm.eval(instrs, list{}))
Js.log(Vm.eval(instrs2, list{}))

Js.log("==> native ir:")
Native.print(assembly)

Js.log("==> machine code:")
let machine_code = Native.to_hex(Native.generate(assembly))
Js.log(machine_code)

Node.Fs.writeFileSync("machine_code.bin", machine_code, #hex)
