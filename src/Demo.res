let is_linux = "linux" == Node.Process.process["platform"]

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
    | Sub(expr, expr)
    | Mul(expr, expr)
    | Var(string)
    | Let(string, expr, expr)
    | Fn(list<string>, expr)
    | App(string, list<expr>)
    | Le(expr, expr)
    | If(expr, expr, expr)

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

  // let eval = (expr: expr) => {
  //   let rec eval_inner = (expr: expr, env: env) => {
  //     switch expr {
  //     | Cst(i) => Vint(i)
  //     | Var(name) => List.assoc(name, env)
  //     | Add(a, b) => vadd(eval_inner(a, env), eval_inner(b, env))
  //     | Mul(a, b) => vmul(eval_inner(a, env), eval_inner(b, env))
  //     | Let(name, a, b) => eval_inner(b, list{(name, eval_inner(a, env)), ...env})
  //     | Fn(params, body) => Vclosure(env, params, body)
  //     | App(fn, args) => {
  //         let (env_closure, params, body) = switch eval_inner(fn, env) {
  //         | Vint(_) => assert false
  //         | Vclosure(env_closure, params, body) => (env_closure, params, body)
  //         }
  //         let arg_env = Belt.List.zip(params, args->Belt.List.map(arg => eval_inner(arg, env)))
  //         let fn_env = list{...arg_env, ...env_closure}
  //         eval_inner(body, fn_env)
  //       }
  //     }
  //   }
  //   switch eval_inner(expr, list{}) {
  //   | Vint(i) => i
  //   | _ => assert false
  //   }
  // }
}

module Resolve = {
  type ident_type = Branch | Function | Var
  type ident = {name: string, stamp: int, ty: ident_type}

  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Sub(expr, expr)
    | Mul(expr, expr)
    | Var(ident)
    | Let(ident, expr, expr)
    | Fn(list<ident>, expr)
    | App(ident, list<expr>)
    | Le(expr, expr)
    | If(expr, expr, expr)

  type env = list<(string, ident)>

  let last_id = ref(0)

  let new_var_ident = (name: string): ident => {
    last_id := last_id.contents + 1
    {name, stamp: last_id.contents, ty: Var}
  }

  let new_branch_ident = (name: string): ident => {
    last_id := last_id.contents + 1
    {name, stamp: last_id.contents, ty: Branch}
  }

  let new_fn_ident = (name: string): ident => {
    last_id := last_id.contents + 1
    {name, stamp: last_id.contents, ty: Function}
  }

  let compile = (expr: Ast.expr) => {
    let rec compile_inner = (expr: Ast.expr, env: env) => {
      switch expr {
      | Cst(i) => Cst(i)
      | Add(a, b) => Add(compile_inner(a, env), compile_inner(b, env))
      | Sub(a, b) => Sub(compile_inner(a, env), compile_inner(b, env))
      | Mul(a, b) => Mul(compile_inner(a, env), compile_inner(b, env))
      | Le(a, b) => Le(compile_inner(a, env), compile_inner(b, env))
      | Var(name) => Var(List.assoc(name, env))
      | Let(name, e1, e2) => {
          let ident = switch e1 {
          | Fn(_, _) => new_fn_ident(name)
          | _ => new_var_ident(name)
          }
          let env = list{(name, ident), ...env}
          Let(ident, compile_inner(e1, env), compile_inner(e2, env))
        }
      | Fn(params, body) => {
          let idents = params->Belt.List.map(new_var_ident)
          let mapping = Belt.List.zip(params, idents)
          Fn(idents, compile_inner(body, list{...mapping, ...env}))
        }
      | App(fn_name, args) =>
        App(List.assoc(fn_name, env), args->Belt.List.map(item => compile_inner(item, env)))
      | If(cond, truthy, falsy) =>
        If(compile_inner(cond, env), compile_inner(truthy, env), compile_inner(falsy, env))
      }
    }
    compile_inner(expr, list{})
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

  // let compile = (expr: Ast.expr): expr => {
  //   let rec compile_inner = (expr: Ast.expr, env: list<string>): expr => {
  //     switch expr {
  //     | Ast.Cst(i) => Cst(i)
  //     | Ast.Add(a, b) => Add(compile_inner(a, env), compile_inner(b, env))
  //     | Ast.Mul(a, b) => Mul(compile_inner(a, env), compile_inner(b, env))
  //     | Ast.Var(name) => Var(findIndex(env, name))
  //     | Ast.Let(name, e1, e2) => Let(compile_inner(e1, env), compile_inner(e2, list{name, ...env}))
  //     | Ast.Fn(params, body) =>
  //       Fn(Belt.List.length(params), compile_inner(body, list{...params, ...env}))
  //     | Ast.App(fn, args) =>
  //       App(compile_inner(fn, env), args->Belt.List.map(item => compile_inner(item, env)))
  //     }
  //   }
  //   compile_inner(expr, list{})
  // }

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
    | Sub
    | Mul
    | Var(int)
    | Pop
    | Swap
    | Call(Resolve.ident, int)
    | Ret(int)
    | Le
    | IfZero(Resolve.ident)
    | Goto(Resolve.ident)
    | Exit
    | Label(Resolve.ident)
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
  let instr_sub = 11
  let instr_le = 12

  type sv = Slocal(Resolve.ident) | Stmp
  type senv = list<sv>

  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Sub(expr, expr)
    | Mul(expr, expr)
    | Var(Resolve.ident)
    | Let(Resolve.ident, expr, expr)
    | App(Resolve.ident, list<expr>)
    | Le(expr, expr)
    | If(expr, expr, expr)

  let ident_main = Resolve.new_fn_ident("main")

  let find_local_index = (env: senv, name: Resolve.ident) => {
    let rec find_recursive = (env: senv, stack_index: int) => {
      switch env {
      | list{Slocal(head), ..._} if head.stamp === name.stamp && head.ty === name.ty => stack_index
      | list{_, ...tail} => find_recursive(tail, stack_index + 1)
      | _ => raise(Not_found)
      }
    }
    find_recursive(env, 0)
  }

  type fun = (Resolve.ident, list<Resolve.ident>, expr)
  type prog = list<fun>

  let preprocess = (expr: Resolve.expr): list<fun> => {
    let rec preprocess_rec = (expr: Resolve.expr): (expr, list<fun>) => {
      switch expr {
      | Cst(i) => (Cst(i), list{})
      | Var(binding) => (Var(binding), list{})
      | Add(a, b) => {
          let (a_expr, a_fns) = preprocess_rec(a)
          let (b_expr, b_fns) = preprocess_rec(b)
          (Add(a_expr, b_expr), list{...a_fns, ...b_fns})
        }
      | Sub(a, b) => {
          let (a_expr, a_fns) = preprocess_rec(a)
          let (b_expr, b_fns) = preprocess_rec(b)
          (Sub(a_expr, b_expr), list{...a_fns, ...b_fns})
        }
      | Mul(a, b) => {
          let (a_expr, a_fns) = preprocess_rec(a)
          let (b_expr, b_fns) = preprocess_rec(b)
          (Mul(a_expr, b_expr), list{...a_fns, ...b_fns})
        }
      | Let(binding, Fn(params, body), expr) => {
          let (main, outer_fns) = preprocess_rec(expr)
          let (fn_body, inner_fns) = preprocess_rec(body)
          (main, list{(binding, params, fn_body), ...inner_fns, ...outer_fns})
        }
      | Let(binding, v1, v2) => {
          let (v1, fns1) = preprocess_rec(v1)
          let (v2, fns2) = preprocess_rec(v2)
          (Let(binding, v1, v2), list{...fns1, ...fns2})
        }
      | App(name, args) => {
          let (args, fns) = args->Belt.List.reduce((list{}, list{}), ((args, fns), arg) => {
            let (expr, inner_fns) = preprocess_rec(arg)
            (list{expr, ...args}, list{...inner_fns, ...fns})
          })
          (App(name, args), fns)
        }
      | Fn(_, _) => assert false
      | Le(a, b) => {
          let (a_expr, a_fns) = preprocess_rec(a)
          let (b_expr, b_fns) = preprocess_rec(b)
          (Le(a_expr, b_expr), list{...a_fns, ...b_fns})
        }
      | If(a, b, c) => {
          let (a_expr, a_fns) = preprocess_rec(a)
          let (b_expr, b_fns) = preprocess_rec(b)
          let (c_expr, c_fns) = preprocess_rec(c)
          (If(a_expr, b_expr, c_expr), list{...a_fns, ...b_fns, ...c_fns})
        }
      }
    }
    let (main, fns) = preprocess_rec(expr)
    list{(ident_main, list{}, main), ...fns}
  }

  let compile_expr = (expr: expr, env: senv): instrs => {
    let rec compile_inner = (expr: expr, env: senv, if_label: int): instrs => {
      switch expr {
      | Cst(i) => list{Cst(i)}
      | Add(e1, e2) =>
        list{
          ...compile_inner(e1, env, if_label + 1),
          ...compile_inner(e2, list{Stmp, ...env}, if_label + 2),
          Add,
        }
      | Sub(e1, e2) =>
        list{
          ...compile_inner(e1, env, if_label + 1),
          ...compile_inner(e2, list{Stmp, ...env}, if_label + 2),
          Sub,
        }
      | Mul(e1, e2) =>
        list{
          ...compile_inner(e1, env, if_label + 1),
          ...compile_inner(e2, list{Stmp, ...env}, if_label + 2),
          Mul,
        }
      | Var(name) => list{Var(find_local_index(env, name))}
      | Let(name, e1, e2) =>
        list{
          ...compile_inner(e1, env, if_label + 1),
          ...compile_inner(e2, list{Slocal(name), ...env}, if_label + 2),
          Swap,
          Pop,
        }
      | App(name, args) => {
          let (args_instrs, _) = args->Belt.List.reduceWithIndex((list{}, env), (
            (args, env),
            arg,
            i,
          ) => {
            (list{...args, ...compile_inner(arg, env, if_label + 1 + i)}, list{Stmp, ...env})
          })
          list{...args_instrs, Call(name, args->Belt.List.length)}
        }
      | Le(a, b) =>
        list{
          ...compile_inner(a, env, if_label + 1),
          ...compile_inner(b, list{Stmp, ...env}, if_label + 2),
          Le,
        }
      | If(cond, pos_expr, neg_expr) => {
          let false_label = Resolve.new_branch_ident("false")
          let end_of_if = Resolve.new_branch_ident("end_of_if")
          list{
            ...compile_inner(cond, env, if_label + 1),
            IfZero(false_label),
            ...compile_inner(pos_expr, env, if_label + 2),
            Goto(end_of_if),
            Label(false_label),
            ...compile_inner(neg_expr, env, if_label + 3),
            Label(end_of_if),
          }
        }
      }
    }
    compile_inner(expr, env, 0)
  }

  let compile_fun = (fun: fun): instrs => {
    let (name, params, body) = fun
    let stack = params->Belt.List.map(name => Slocal(name))->Belt.List.reverse
    list{Label(name), ...compile_expr(body, list{Stmp, ...stack}), Ret(params->Belt.List.length)}
  }

  let compile_prog = (expr: Ast.expr): instrs => {
    let expr = Resolve.compile(expr)
    let fns = preprocess(expr)->Belt.List.map(fn => compile_fun(fn))->Belt.List.flatten
    list{Call(ident_main, 0), Exit, ...fns}
  }

  let get_instr_size = (instr: instr) => {
    switch instr {
    | Label(_) => 0
    | Cst(_) | Var(_) | Ret(_) | Goto(_) | IfZero(_) => 2
    | Add | Sub | Mul | Le | Pop | Swap | Exit => 1
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
    let compile_instr = (instr: instr, label_map: list<(Resolve.ident, int)>) => {
      switch instr {
      | Label(_) => list{}
      | Cst(i) => list{instr_const, i}
      | Add => list{instr_add}
      | Sub => list{instr_sub}
      | Mul => list{instr_mul}
      | Le => list{instr_le}
      | Var(i) => list{instr_var, i}
      | Pop => list{instr_pop}
      | Swap => list{instr_swap}
      | Call(target, args) => list{instr_call, target->List.assoc(label_map), args}
      | Ret(args) => list{instr_ret, args}
      | IfZero(target) => list{instr_if_zero, target->List.assoc(label_map)}
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
      | IfZero(target) => "if_zero " ++ target.name
      | Exit => "exit"
      | Call(fn, args) => "call/" ++ args->Belt.Int.toString ++ " " ++ fn.name
      | Ret(n) => "ret " ++ n->Belt.Int.toString
      | Goto(label) => "goto " ++ label.name
      | Label(label) => label.name ++ ":"
      | Sub => "sub"
      | Le => "le"
      }
      Js.log(instr_text)
    }
  }
}

module Native = {
  type reg = Al | Rax | Rbx | Rcx | Rdx | Rsp
  type mov_arg =
    Constant(int) | Reg(reg) | Addr(int) | RegOffset({base: reg, index: reg, scale: int, disp: int})
  type instr =
    | Mov(reg, mov_arg)
    | Movzx(reg, reg)
    | Push(reg)
    | Pop(reg)
    | Add(reg, reg)
    | Sub(reg, reg)
    | Mul(reg)
    | Test(reg, reg)
    | Cmp(reg, reg)
    | Setna(reg)
    | Retn(int)
    | Ret
    | Label(Resolve.ident)
    | Call(Resolve.ident)
    | Goto(Resolve.ident)
    | Je(Resolve.ident)

  type instrs = list<instr>

  let compile_vm = (instrs: Vm.instrs): instrs => {
    let compile_instr = (instr: Vm.instr) => {
      switch instr {
      | Vm.Cst(i) => list{Mov(Rax, Constant(i)), Push(Rax)}
      | Vm.Add => list{Pop(Rax), Pop(Rbx), Add(Rax, Rbx), Push(Rax)}
      | Vm.Sub => list{Pop(Rbx), Pop(Rax), Sub(Rax, Rbx), Push(Rax)}
      | Vm.Le => list{Pop(Rbx), Pop(Rax), Cmp(Rax, Rbx), Setna(Al), Movzx(Rbx, Al), Push(Rbx)}
      | Vm.Mul => list{Pop(Rax), Pop(Rbx), Mul(Rbx), Push(Rax)}
      | Vm.Var(i) =>
        list{
          Mov(Rbx, Constant(i)),
          Mov(Rax, RegOffset({base: Rsp, index: Rbx, scale: 8, disp: 0})),
          Push(Rax),
        }
      | Vm.Pop => list{Pop(Rax)}
      | Vm.Swap => list{Pop(Rax), Pop(Rbx), Push(Rax), Push(Rbx)}
      | Ret(0) => list{Pop(Rax), Ret}
      | Ret(n) => list{Pop(Rax), Retn(n * 8)}
      | Exit => list{Pop(Rax), Ret}
      | Call(label, _) => list{Call(label), Push(Rax)}
      | Goto(label) => list{Goto(label)}
      | IfZero(label) => list{Pop(Rax), Test(Rax, Rax), Je(label)}
      | Label(label) => list{Label(label)}
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
    compile_inner(instrs)
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

  let to_reg_str = (reg: reg) => {
    switch reg {
    | Al => "al"
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
    let get_label = (label: Resolve.ident) => {
      label.name ++ "_" ++ label.stamp->Belt.Int.toString
    }
    let instr_text = instrs->Belt.List.map(instr =>
      switch instr {
      | Setna(reg) => "setna " ++ to_reg_str(reg)
      | Label(label) => {
          let props = if label.ty === Resolve.Function {
            if is_linux {
              ".type " ++ get_label(label) ++ ",@function\n.global " ++ get_label(label) ++ "\n"
            } else {
              ".def " ++ get_label(label) ++ ";\n.scl 2;\n.type 32;\n.endef\n"
            }
          } else {
            ""
          }
          props ++ get_label(label) ++ ":"
        }
      | Mov(reg, arg) => "mov " ++ to_reg_str(reg) ++ ", " ++ to_mov_arg_str(arg)
      | Movzx(r1, r2) => "movzx " ++ to_reg_str(r1) ++ ", " ++ to_reg_str(r2)
      | Push(reg) => "push " ++ to_reg_str(reg)
      | Pop(reg) => "pop " ++ to_reg_str(reg)
      | Add(r1, r2) => "add " ++ to_reg_str(r1) ++ ", " ++ to_reg_str(r2)
      | Sub(r1, r2) => "sub " ++ to_reg_str(r1) ++ ", " ++ to_reg_str(r2)
      | Mul(reg) => "mul " ++ to_reg_str(reg)
      | Cmp(r1, r2) => "cmp " ++ to_reg_str(r1) ++ ", " ++ to_reg_str(r2)
      | Test(r1, r2) => "test " ++ to_reg_str(r1) ++ ", " ++ to_reg_str(r2)
      | Call(label) => "call " ++ get_label(label)
      | Goto(label) => "jmp " ++ get_label(label)
      | Je(label) => "je " ++ get_label(label)
      | Ret => "ret"
      | Retn(n) => "ret " ++ n->Belt.Int.toString
      }
    )
    instr_text->Belt.List.reduce(".intel_syntax noprefix\n", (res, ins) => res ++ ins ++ "\n")
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
// let my_expr = Ast.Mul(
//   Ast.Add(Ast.Cst(1), Ast.Cst(3)), // 4
//   Ast.Let(
//     "x", // x=3
//     Ast.Add(Ast.Cst(1), Ast.Cst(2)), // 3
//     Ast.Add(
//       Ast.Var("x"), // 3
//       Ast.Let("y", Ast.Add(Ast.Cst(1), Ast.Cst(1)), Ast.Add(Ast.Var("x"), Ast.Var("y"))), // y=2 5
//     ),
//   ),
// )

// fib(n)
// n==0 => n
// n==1 => n
// n => fib(n-1) + fib(n-2)

// n<=1
// n-1<=0

let my_expr = Ast.Let(
  "fib",
  Ast.Fn(
    list{"n"},
    Ast.If(
      Ast.Le(Var("n"), Ast.Cst(1)),
      Cst(1),
      Ast.Add(
        Ast.App("fib", list{Ast.Sub(Ast.Var("n"), Ast.Cst(1))}),
        Ast.App("fib", list{Ast.Sub(Ast.Var("n"), Ast.Cst(2))}),
      ),
    ),
  ),
  Ast.App("fib", list{Ast.Cst(10)}),
)

// let my_nameless = Nameless.compile(my_expr)
// Js.log("Nameless:")
// Js.log(Nameless.print(my_nameless))

// let my_indexed = Indexed.compile(my_nameless)
// Js.log("==> Indexed:")
// Js.log(Indexed.print(my_indexed))

// let instrs = Vm.compile_indexed(my_indexed)
// Js.log("==> multi-level ir:")
// Vm.print(instrs)

let instrs2 = Vm.compile_prog(my_expr)

Js.log("==> single pass:")
Vm.print(instrs2)

// Js.log(Ast.eval(my_expr))
// Js.log(Vm.eval(instrs, list{}))

// bytecode
let bytecode = Vm.to_hex(Vm.to_bytecode(list{...instrs2, Vm.Exit}))
Node.Fs.writeFileSync("bytecode.bin", bytecode, #hex)

// machine code
let assembly = Native.optimize(Native.compile_vm(instrs2))
Js.log("==> native ir:")
Node.Fs.writeFileSync("machine_code.s", Native.print(assembly), #utf8)

// Js.log("==> machine code:")
// let machine_code = Native.to_hex(Native.generate(assembly))
// Js.log(machine_code)

// Node.Fs.writeFileSync("machine_code.bin", machine_code, #hex)
