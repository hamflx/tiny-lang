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

let my_expr = Ast.Add(
  Ast.Add(Ast.Cst(1), Ast.Let("x", Ast.Cst(2), Ast.Add(Ast.Var("x"), Ast.Var("x")))),
  Ast.Cst(3),
)

let my_nameless = Nameless.compile(my_expr)
let my_indexed = Indexed.compile(my_nameless)
let instrs = Vm.compile_indexed(my_indexed)

let instrs2 = Vm.compile_ast(my_expr)

Js.log("==> multi-level ir:")
Vm.print(instrs)

Js.log("==> single pass:")
Vm.print(instrs2)

Js.log(Ast.eval(my_expr))
Js.log(Vm.eval(instrs, list{}))
Js.log(Vm.eval(instrs2, list{}))
