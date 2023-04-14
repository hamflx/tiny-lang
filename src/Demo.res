let findIndex = (list: list<'a>, item: 'a) => {
  let rec findWithIndex = (list: list<'a>, index: int) => {
    switch list {
    | list{} => raise(Not_found)
    | list{head, ...tail} => head === item ? index : findWithIndex(tail, index + 1)
    }
  }
  findWithIndex(list, 0)
}

let to_little_endian_32 = (i: int): list<int> => {
  let b1 = i->land(0xff)
  let b2 = i->lsr(8)->land(0xff)
  let b3 = i->lsr(16)->land(0xff)
  let b4 = i->lsr(24)->land(0xff)
  list{b1, b2, b3, b4}
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

  type rec value =
    | Vint(int)
    | Vclosure(env, list<string>, expr)
  and env = list<(string, value)>

  let vadd = (a: value, b: value) => {
    switch (a, b) {
    | (Vint(a), Vint(b)) => Vint(a + b)
    | _ => assert false
    }
  }

  let vmul = (a: value, b: value) => {
    switch (a, b) {
    | (Vint(a), Vint(b)) => Vint(a * b)
    | _ => assert false
    }
  }

  let eval = (expr: expr) => {
    let rec eval_inner = (expr: expr, env: env) => {
      switch expr {
      | Cst(i) => Vint(i)
      | Var(name) => List.assoc(name, env)
      | Add(a, b) => vadd(eval_inner(a, env), eval_inner(b, env))
      | Mul(a, b) => vmul(eval_inner(a, env), eval_inner(b, env))
      | Let(name, a, b) => eval_inner(b, list{(name, eval_inner(a, env)), ...env})
      | Fn(params, body) => Vclosure(env, params, body)
      | App(fn, args) => {
          let (env_closure, params, body) = switch eval_inner(fn, env) {
          | Vint(_) => assert false
          | Vclosure(env_closure, params, body) => (env_closure, params, body)
          }
          let arg_env = Belt.List.zip(params, args->Belt.List.map(arg => eval_inner(arg, env)))
          let fn_env = list{...arg_env, ...env_closure}
          eval_inner(body, fn_env)
        }
      }
    }
    switch eval_inner(expr, list{}) {
    | Vint(i) => i
    | _ => assert false
    }
  }
}

module Nameless = {
  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(int)
    | Let(expr, expr)
    | Fn(int, expr)
    | App(expr, list<expr>)

  let compile = (expr: Ast.expr): expr => {
    let rec compile_inner = (expr: Ast.expr, env: list<string>): expr => {
      switch expr {
      | Ast.Cst(i) => Cst(i)
      | Ast.Add(a, b) => Add(compile_inner(a, env), compile_inner(b, env))
      | Ast.Mul(a, b) => Mul(compile_inner(a, env), compile_inner(b, env))
      | Ast.Var(name) => Var(findIndex(env, name))
      | Ast.Let(name, e1, e2) => Let(compile_inner(e1, env), compile_inner(e2, list{name, ...env}))
      | Ast.Fn(params, body) =>
        Fn(Belt.List.length(params), compile_inner(body, list{...params, ...env}))
      | Ast.App(fn, args) =>
        App(compile_inner(fn, env), args->Belt.List.map(item => compile_inner(item, env)))
      }
    }
    compile_inner(expr, list{})
  }

  let print = (expr: expr) => {
    let rec go = (expr: expr) => {
      switch expr {
      | Cst(i) => Belt.Int.toString(i)
      | Add(a, b) => "(" ++ go(a) ++ ") + (" ++ go(b) ++ ")"
      | Mul(a, b) => "(" ++ go(a) ++ ") * (" ++ go(b) ++ ")"
      | Var(i) => "var" ++ Belt.Int.toString(i)
      | Let(e1, e2) => "let v0 = (" ++ go(e1) ++ ") in (" ++ go(e2) ++ ")"
      | Fn(_, e) => "fn(" ++ go(e) ++ ")"
      | App(e, args) =>
        "app( " ++
        go(e) ++
        args
        ->Belt.List.map(item => go(item))
        ->Belt.List.reduce("", (list, item) => list ++ ", (" ++ item ++ "), ") ++ " )"
      }
    }
    go(expr)
  }
}

module Indexed = {
  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(int)
    | Let(expr, expr)
    | Fn(expr)
    | App(expr, list<expr>)

  type sv = Slocal | Stmp
  type senv = list<sv>

  let find_local_index = (env: senv, local_index: int) => {
    let rec findWithIndex = (env: senv, local_index: int, stack_index: int) => {
      switch (env, local_index) {
      | (list{Slocal, ..._}, 0) => stack_index
      | (list{Slocal, ...tail}, _) => findWithIndex(tail, local_index - 1, stack_index + 1)
      | (list{_, ...tail}, _) => findWithIndex(tail, local_index, stack_index + 1)
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
      | Nameless.Fn(size, e) => Fn(go(e, list{...Belt.List.makeBy(size, _ => Slocal), ...env}))
      | Nameless.App(e, args) => App(go(e, env), args->Belt.List.map(item => go(item, env)))
      }
    }
    go(expr, list{})
  }

  let print = (expr: expr) => {
    let rec go = (expr: expr) => {
      switch expr {
      | Cst(i) => Belt.Int.toString(i)
      | Add(a, b) => "(" ++ go(a) ++ ") + (" ++ go(b) ++ ")"
      | Mul(a, b) => "(" ++ go(a) ++ ") * (" ++ go(b) ++ ")"
      | Var(i) => "var" ++ Belt.Int.toString(i)
      | Let(e1, e2) => "let v0 = (" ++ go(e1) ++ ") in (" ++ go(e2) ++ ")"
      | Fn(e) => "fn(" ++ go(e) ++ ")"
      | App(e, args) =>
        "app( " ++
        go(e) ++
        ", " ++
        args
        ->Belt.List.map(item => go(item))
        ->Belt.List.reduce("", (list, item) => list ++ "(" ++ item ++ "), ") ++ " )"
      }
    }
    go(expr)
  }
}

module Vm = {
  type instr =
    | Cst(int)
    | Add
    | Mul
    | Var(int)
    | Pop
    | Swap
    | Call(string, int)
    | Ret(int)
    | IfZero
    | Goto(string)
    | Exit
    | Label(string)
  type instrs = list<instr>
  type operand = int
  type stack = list<operand>

  let instr_const = 0
  let instr_add = 1
  let instr_mul = 2
  let instr_var = 3
  let instr_pop = 4
  let instr_swap = 5
  let instr_call = 6
  let instr_ret = 7
  let instr_goto = 8
  let instr_if_zero = 9
  let instr_exit = 10

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

  let get_instr_size = (instr: instr) => {
    switch instr {
    | Label(_) => 0
    | Cst(_) | Var(_) | Ret(_) | Goto(_) => 2
    | Add | Mul | Pop | Swap | IfZero | Exit => 1
    | Call(_, _) => 3
    }
  }

  let to_hex = (code: list<int>) => {
    code->Belt.List.reduce("", (sum, item) =>
      sum ++
      item
      ->to_little_endian_32
      ->Belt.List.reduce("", (sum, item) =>
        sum ++ ("00" ++ Js.Int.toStringWithRadix(item, ~radix=16))->Js.String2.sliceToEnd(~from=-2)
      )
    )
  }

  let to_bytecode = (instrs: instrs) => {
    let (label_map, _) = instrs->Belt.List.reduce((list{}, 0), ((label_map, pos), item) => {
      switch item {
      | Label(label) => (list{(label, pos), ...label_map}, pos)
      | _ => (label_map, pos + item->get_instr_size)
      }
    })
    let compile_instr = (instr: instr, label_map: list<(string, int)>) => {
      switch instr {
      | Label(_) => list{}
      | Cst(i) => list{instr_const, i}
      | Add => list{instr_add}
      | Mul => list{instr_mul}
      | Var(i) => list{instr_var, i}
      | Pop => list{instr_pop}
      | Swap => list{instr_swap}
      | Call(target, args) => list{instr_call, target->List.assoc(label_map), args}
      | Ret(args) => list{instr_ret, args}
      | IfZero => list{instr_if_zero}
      | Goto(target) => list{instr_goto, target->List.assoc(label_map)}
      | Exit => list{instr_exit}
      }
    }
    let code = instrs->Belt.List.reduce(list{}, (code, instr) =>
      list{
        ...code,
        ...switch instr {
        | Label(_) => list{}
        | instr => instr->compile_instr(label_map)
        },
      }
    )
    code
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
  type instr = Mov(reg, mov_arg) | Push(reg) | Pop(reg) | Add(reg, reg) | Mul(reg) | Ret
  type instrs = list<instr>

  let compile_vm = (instrs: Vm.instrs): instrs => {
    let compile_instr = (instr: Vm.instr) => {
      switch instr {
      | Vm.Cst(i) => list{Mov(Rax, Constant(i)), Push(Rax)}
      | Vm.Add => list{Pop(Rax), Pop(Rbx), Add(Rax, Rbx), Push(Rax)}
      | Vm.Mul => list{Pop(Rax), Pop(Rbx), Mul(Rbx), Push(Rax)}
      | Vm.Var(i) =>
        list{
          Mov(Rbx, Constant(i)),
          Mov(Rax, RegOffset({base: Rsp, index: Rbx, scale: 8, disp: 0})),
          Push(Rax),
        }
      | Vm.Pop => list{Pop(Rax)}
      | Vm.Swap => list{Pop(Rax), Pop(Rbx), Push(Rax), Push(Rbx)}
      }
    }
    let rec compile_inner = (instrs: Vm.instrs): instrs => {
      switch instrs {
      | list{} => list{}
      | list{Vm.Swap, Vm.Pop, ...tail} =>
        list{Pop(Rax), Pop(Rbx), Push(Rax), ...compile_inner(tail)}
      | list{head, ...tail} => list{...compile_instr(head), ...compile_inner(tail)}
      }
    }
    list{...compile_inner(instrs), Pop(Rax), Ret}
  }

  let optimize = (instrs: instrs) => {
    let rec optimize_inner = (instrs: instrs) => {
      switch instrs {
      | list{Push(Rax), Pop(Rax), ...tail} => optimize_inner(tail)
      | list{head, ...tail} => list{head, ...optimize_inner(tail)}
      | list{} => list{}
      }
    }
    let rec optimize_rec = (instrs: instrs) => {
      let o = optimize_inner(instrs)
      if Belt.List.length(o) === Belt.List.length(instrs) {
        o
      } else {
        optimize_rec(o)
      }
    }
    optimize_rec(instrs)
  }

  let generate = (instrs: instrs) => {
    let generate_instr = (instr: instr) => {
      switch instr {
      | Mov(reg, Constant(i)) if i < 0x7fffffff =>
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
          ...to_little_endian_32(i),
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
      | Mul(Rbx) => list{0x48, 0xf7, 0xe3}
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
      | Mul(reg) => "mul " ++ to_reg_str(reg)
      | Ret => "ret"
      }
      Js.log(instr_text)
    }
  }
}

// let my_expr = Ast.Mul(
//   Ast.App(
//     Ast.Fn(list{"x"}, Ast.Let("y", Ast.Cst(1), Ast.Add(Ast.Var("x"), Ast.Var("y")))),
//     list{Ast.Cst(3)},
//   ),
//   Ast.Add(
//     Ast.Add(Ast.Cst(10086), Ast.Let("x", Ast.Cst(2), Ast.Add(Ast.Var("x"), Ast.Var("x")))),
//     Ast.Cst(3),
//   ),
// )

// 20
let my_expr = Ast.Mul(
  Ast.Add(Ast.Cst(1), Ast.Cst(3)), // 4
  Ast.Let(
    "x", // x=3
    Ast.Add(Ast.Cst(1), Ast.Cst(2)), // 3
    Ast.Add(
      Ast.Var("x"), // 3
      Ast.Let("y", Ast.Add(Ast.Cst(1), Ast.Cst(1)), Ast.Add(Ast.Var("x"), Ast.Var("y"))), // y=2 5
    ),
  ),
)

let my_nameless = Nameless.compile(my_expr)
Js.log("Nameless:")
Js.log(Nameless.print(my_nameless))

let my_indexed = Indexed.compile(my_nameless)
Js.log("==> Indexed:")
Js.log(Indexed.print(my_indexed))

let instrs = Vm.compile_indexed(my_indexed)
let instrs2 = Vm.compile_ast(my_expr)

Js.log("==> multi-level ir:")
Vm.print(instrs)

Js.log("==> single pass:")
Vm.print(instrs2)

Js.log(Ast.eval(my_expr))
Js.log(Vm.eval(instrs, list{}))
Js.log(Vm.eval(instrs2, list{}))

// bytecode
let bytecode = Vm.to_hex(Vm.to_bytecode(list{...instrs2, Vm.Exit}))
Node.Fs.writeFileSync("bytecode.bin", bytecode, #hex)

// machine code
let assembly = Native.optimize(Native.compile_vm(instrs2))
Js.log("==> native ir:")
Native.print(assembly)

Js.log("==> machine code:")
let machine_code = Native.to_hex(Native.generate(assembly))
Js.log(machine_code)

Node.Fs.writeFileSync("machine_code.bin", machine_code, #hex)
