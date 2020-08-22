use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::rc::Rc;

pub fn read_input(filename: &str) -> Vec<String> {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let mut res = Vec::new();
    for line in reader.lines() {
        let line = line.unwrap();
        res.push(line.to_string());
    }
    return res;
}

pub fn split_string(s: &String, pattern: &str) -> Vec<String> {
    let mut res = Vec::new();
    for part in s.split(pattern) {
        res.push(part.to_string());
    }
    return res;
}

pub fn to_lines(s: &str) -> Vec<String> {
    split_string(&s.trim().to_string(), "\n")
}

pub fn parse_i64(s: &String) -> i64 {
    match s.parse::<i64>() {
        Err(e) => {
            assert!(false, "Error parsing '{}': {}", &s, e);
            unreachable!();
        }
        Ok(value) => {
            return value;
        }
    }
}

fn interact(functions: &FunctionMap, state: ExprPtr, event: ExprPtr) -> (ExprPtr, ExprPtr) {
    let expr = ap(ap(atom("galaxy"), state.clone()), event.clone());
    println!("evaluating galaxy!");
    let res = eval(expr, functions);
    println!("res = {}", to_str(&res));
    // tmp:
    (state, event)
}

fn run_galaxy() {
    let lines = read_input("galaxy.txt");
    let mut functions: FunctionMap = HashMap::new();
    for l in &lines {
        let parts = split_string(l, " = ");
        let name = parts[0].to_string();
        let tokens = split_string(&parts[1], " ");
        let expr = parse_from_tokens(&tokens);
        // println!(
        //     "{} -> {} tokens: {:?}, expr = {}",
        //     &name,
        //     tokens.len(),
        //     &tokens,
        //     to_str(&expr)
        // );
        functions.insert(name, expr);
    }
    let pt = (0, 0);
    let click = ap(ap(cons(), atom_int(pt.0)), atom_int(pt.1));
    let state = atom("nil");
    interact(&functions, state, click);
}

// #[derive(Clone, Copy, PartialEq, Debug)]
// enum ExprType {
//     Atom,
//     Ap,
// }

#[derive(Debug, Clone, PartialEq)]
enum ExprImpl {
    Atom(String),
    Ap { fun: ExprPtr, arg: ExprPtr },
}

type ExprPtr = Rc<Expr>;

#[derive(Debug, Clone, PartialEq)]
struct Expr {
    evaluated: Option<RefCell<ExprPtr>>,
    expr_impl: ExprImpl,
    // expr_type: ExprType,
    // // Atom fields:
    // name: String,
    // // Ap fields:
    // fun: Option<ExprPtr>,
    // arg: Option<ExprPtr>,
}

fn to_str(expr: &Expr) -> String {
    match &expr.expr_impl {
        ExprImpl::Atom(name) => format!("{}", name).to_string(),
        ExprImpl::Ap { fun, arg } => {
            let str1 = to_str(&fun);
            let str2 = to_str(&arg);
            format!("(ap {} {})", str1, str2).to_string()
        }
    }
}

fn to_short_str(expr: &ExprPtr) -> String {
    match &expr.expr_impl {
        ExprImpl::Atom(name) => format!("atom({})", name).to_string(),
        ExprImpl::Ap { fun, arg } => format!("ap()").to_string(),
    }
}

// TODO: consider a library class for that
struct TokenStream {
    vec: Vec<String>,
    index: usize,
}

impl TokenStream {
    pub fn read(&mut self) -> String {
        let res = self.vec[self.index].clone();
        self.index += 1;
        res
    }
}

fn parse_from_string(s: &String) -> ExprPtr {
    let tokens = split_string(s, " ");
    parse_from_tokens(&tokens)
}

fn parse_from_tokens(tokens: &Vec<String>) -> ExprPtr {
    let mut stream = TokenStream {
        vec: tokens.clone(),
        index: 0,
    };
    parse_expr(&mut stream)
}

fn parse_expr(stream: &mut TokenStream) -> ExprPtr {
    let token = stream.read();
    if token == "ap" {
        let fun = parse_expr(stream);
        let x = parse_expr(stream);
        return ap(fun, x);
    } else {
        return atom_string(token);
    }
}

type FunctionMap = HashMap<String, ExprPtr>;

fn t() -> ExprPtr {
    atom("t")
}

fn f() -> ExprPtr {
    atom("f")
}

fn cons() -> ExprPtr {
    atom("cons")
}

fn atom_string(name: String) -> ExprPtr {
    Rc::new(Expr {
        evaluated: None,
        expr_impl: ExprImpl::Atom(name),
    })
}

fn atom_int(value: i64) -> ExprPtr {
    atom_string(value.to_string())
}

fn atom(name: &str) -> ExprPtr {
    atom_string(name.to_string())
}

fn ap(fun: ExprPtr, arg: ExprPtr) -> ExprPtr {
    Rc::new(Expr {
        evaluated: None,
        expr_impl: ExprImpl::Ap { fun: fun, arg: arg },
    })
}

fn eval(_expr: ExprPtr, functions: &FunctionMap) -> ExprPtr {
    let mut expr = _expr.clone();
    if let Some(evaluated) = &expr.evaluated {
        return evaluated.borrow().clone();
    }
    let mut initial_expr = (*expr.clone()).clone();
    loop {
        println!("eval on {}", to_short_str(&expr));
        let result = try_eval(expr.clone(), functions);
        // println!("  eval on {} --> {}", to_str(&expr), to_str(&result));
        if result.expr_impl == expr.expr_impl {
            let result_expr = RefCell::new(result.clone());
            initial_expr.evaluated = Some(result_expr);
            return result;
        }
        expr = result;
    }
}

fn try_eval(expr: ExprPtr, functions: &FunctionMap) -> ExprPtr {
    if let Some(evaluated) = &expr.evaluated {
        return evaluated.borrow().clone();
    }
    // println!("    try eval {}", to_str(&expr));
    if let ExprImpl::Atom(name) = &expr.expr_impl {
        if functions.contains_key(name) {
            return functions[name].clone();
        }
    }
    if let ExprImpl::Ap { fun, arg } = &expr.expr_impl {
        let fun = eval(fun.clone(), functions);
        let x = arg.clone();
        if let ExprImpl::Atom(fun_name) = &fun.expr_impl {
            // ap(fun, x)
            if fun_name == "neg" {
                return atom_int(-as_number(eval(x, functions)));
            }
            if fun_name == "i" {
                return x;
            }
            if fun_name == "nil" {
                return t();
            }
            if fun_name == "isnil" {
                return ap(x, ap(t(), ap(t(), f())));
            }
            if fun_name == "car" {
                return ap(x, t());
            }
            if fun_name == "cdr" {
                return ap(x, f());
            }
        }
        if let ExprImpl::Ap {
            fun: fun2_expr,
            arg: arg2,
        } = &fun.expr_impl
        {
            let fun2 = eval(fun2_expr.clone(), functions);
            let y = arg2.clone();
            if let ExprImpl::Atom(fun2_name) = &fun2.expr_impl {
                // ap(ap(fun2, y), x)
                if fun2_name == "t" {
                    return y;
                }
                if fun2_name == "f" {
                    return x;
                }
                if fun2_name == "add" {
                    return atom_int(as_number(eval(x, functions)) + as_number(eval(y, functions)));
                }
                if fun2_name == "mul" {
                    return atom_int(as_number(eval(x, functions)) * as_number(eval(y, functions)));
                }
                if fun2_name == "div" {
                    return atom_int(as_number(eval(y, functions)) / as_number(eval(x, functions)));
                }
                if fun2_name == "lt" {
                    return if as_number(eval(y, functions)) < as_number(eval(x, functions)) {
                        t()
                    } else {
                        f()
                    };
                }
                if fun2_name == "eq" {
                    return if as_number(eval(x, functions)) == as_number(eval(y, functions)) {
                        t()
                    } else {
                        f()
                    };
                }
                if fun2_name == "cons" {
                    return eval_cons(y, x, &functions);
                }
            }
            if let ExprImpl::Ap {
                fun: fun3_expr,
                arg: arg3,
            } = &fun2.expr_impl
            {
                let fun3 = eval(fun3_expr.clone(), functions);
                let z = eval(arg3.clone(), functions);
                if let ExprImpl::Atom(fun3_name) = &fun3.expr_impl {
                    if fun3_name == "s" {
                        println!("Running s with x = {}", to_str(&x));
                        return ap(ap(z, Rc::clone(&x)), ap(y, x));
                    }
                    if fun3_name == "c" {
                        return ap(ap(z, x), y);
                    }
                    if fun3_name == "b" {
                        return ap(z, ap(y, x));
                    }
                    if fun3_name == "cons" {
                        return ap(ap(x, z), y);
                    }
                }
            }
        }
    }
    expr
}

fn eval_cons(a: ExprPtr, b: ExprPtr, functions: &FunctionMap) -> ExprPtr {
    // println!("running eval_cons..");
    let mut res = ap(ap(cons(), eval(a, functions)), eval(b, functions));
    let mut ret = (*res.clone()).clone();
    ret.evaluated = Some(RefCell::new(res));
    Rc::new(ret)
}

fn as_number(expr: ExprPtr) -> i64 {
    match &expr.expr_impl {
        ExprImpl::Atom(name) => {
            return parse_i64(&name);
        }
        _ => {
            panic!("{:?} is not a number", expr);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_parse_eval(input: &str, expected: &str) {
        let expr = parse_from_string(&input.to_string());
        let functions: FunctionMap = HashMap::new();
        let value = eval(expr, &functions);
        if let ExprImpl::Atom(name) = &value.expr_impl {
            assert_eq!(name, expected);
        } else {
            assert!(false, "Unexpected result: {:?}", to_str(&value));
        }
    }

    #[test]
    fn test_eval1() {
        let expr = ap(atom("neg"), atom("14"));
        let functions: FunctionMap = HashMap::new();
        let value = eval(expr, &functions);
        if let ExprImpl::Atom(name) = &value.expr_impl {
            assert_eq!(name, "-14");
        } else {
            assert!(false, "Unexpected result: {:?}", value.expr_impl);
        }
    }

    #[test]
    fn test_add() {
        assert_parse_eval("ap ap add 1 2", "3");
    }

    #[test]
    fn test_mul() {
        assert_parse_eval("ap ap mul 4 2", "8");
        assert_parse_eval("ap ap mul 3 4", "12");
        assert_parse_eval("ap ap mul 3 -2", "-6");
    }

    #[test]
    fn test_div() {
        assert_parse_eval("ap ap div 4 3", "1");
        assert_parse_eval("ap ap div 4 4", "1");
        assert_parse_eval("ap ap div 4 5", "0");
        assert_parse_eval("ap ap div 5 2", "2");
        assert_parse_eval("ap ap div 6 -2", "-3");
        assert_parse_eval("ap ap div 5 -3", "-1");
        assert_parse_eval("ap ap div -5 3", "-1");
        assert_parse_eval("ap ap div -5 -3", "1");
        assert_parse_eval("ap ap div 4 2", "2");
    }

    #[test]
    fn test_eq() {
        assert_parse_eval("ap ap eq 0 0", "t");
        assert_parse_eval("ap ap eq 0 1", "f");
        assert_parse_eval("ap ap eq 0 2", "f");
    }

    #[test]
    fn test_lt() {
        assert_parse_eval("ap ap lt 1 0", "f");
        assert_parse_eval("ap ap lt 1 1", "f");
        assert_parse_eval("ap ap lt 1 2", "t");
    }

    #[test]
    fn test_tf() {
        assert_parse_eval("ap ap t 1 5", "1");
        assert_parse_eval("ap ap f 1 5", "5");
    }

    #[test]
    fn test_cons() {
        assert_parse_eval("ap car ap ap cons 1 5", "1");
        assert_parse_eval("ap cdr ap ap cons 1 5", "5");
    }

    #[test]
    fn test_eval_const() {
        let expr = ap(ap(cons(), atom("1")), atom("5"));
        let functions: FunctionMap = HashMap::new();
        let expr2 = try_eval(expr.clone(), &functions);
        assert_eq!(expr.expr_impl, expr2.expr_impl);
        println!("{:?}", expr.expr_impl);
        println!("{:?}", expr2.expr_impl);
        let exp3 = eval(expr.clone(), &functions);
    }

    #[test]
    fn test_s_combinator() {
        // assert_parse_eval("ap ap ap s mul ap add 1 6", "42");
        assert_parse_eval("ap ap ap s mul ap add 1 ap ap add 2 4", "42");
    }

    #[test]
    fn test_rc_clone() {
        let mut x = parse_from_string(&"ap ap add 2 4".to_string());
        let mut x2 = Rc::clone(&x);
        println!("x = {:?}", &x);
        println!("x2 = {:?}", &x2);

        let functions: FunctionMap = HashMap::new();
        let y = eval(x.clone(), &functions);
        println!("y = {:?}", &y);
        println!("x = {:?}", &x);
        println!("x2 = {:?}", &x2);
    }

    #[test]
    fn test_foo() {
        let foo = Foo {
            evaluated: None,
            value: 42,
        };
        let mut x = Rc::new(RefCell::new(foo));
        let mut x2 = Rc::clone(&x);
        println!("x = {}", foo_to_str(&x));
        println!("x2 = {}", foo_to_str(&x2));

        x.borrow_mut().evaluated = Some(x.clone());
        println!("x = {}", foo_to_str(&x));
        println!("x2 = {}", foo_to_str(&x2));
    }
}

type FooPtr = Rc<RefCell<Foo>>;

fn foo_to_str(foo: &FooPtr) -> String {
    match &foo.borrow().evaluated {
        Some(foo_ptr) => format!(
            "Foo({}, Some({}))",
            foo.borrow().value,
            foo_ptr.borrow().value
        )
        .to_string(),
        _ => format!("Foo({}, None)", foo.borrow().value).to_string(),
    }
}

#[derive(Debug)]
struct Foo {
    value: i32,
    evaluated: Option<FooPtr>,
}

fn main() {
    println!("Hello, Galaxy!");
    run_galaxy();
}
