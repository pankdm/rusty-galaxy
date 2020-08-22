use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::rc::Rc;

const MUL: usize = 2;

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
    let res = eval(expr, functions, 0);
    println!("res = {}", to_str(&res.borrow()));
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

type ExprPtr = Rc<RefCell<Expr>>;

#[derive(Debug)]
struct Expr {
    evaluated: Option<ExprPtr>,
    expr_impl: ExprImpl,
    // expr_type: ExprType,
    // // Atom fields:
    // name: String,
    // // Ap fields:
    // fun: Option<ExprPtr>,
    // arg: Option<ExprPtr>,
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        // compare only body
        self.expr_impl == other.expr_impl
    }
}

fn to_str(expr: &Expr) -> String {
    match &expr.expr_impl {
        ExprImpl::Atom(name) => format!("{}", name).to_string(),
        ExprImpl::Ap { fun, arg } => {
            let str1 = to_str(&fun.borrow());
            let str2 = to_str(&arg.borrow());
            format!("(ap {} {})", str1, str2).to_string()
        }
    }
}

fn to_short_str(expr: &ExprPtr) -> String {
    match &expr.borrow().expr_impl {
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
    Rc::new(RefCell::new(Expr {
        evaluated: None,
        expr_impl: ExprImpl::Atom(name),
    }))
}

fn atom_int(value: i64) -> ExprPtr {
    atom_string(value.to_string())
}

fn atom(name: &str) -> ExprPtr {
    atom_string(name.to_string())
}

fn ap(fun: ExprPtr, arg: ExprPtr) -> ExprPtr {
    Rc::new(RefCell::new(Expr {
        evaluated: None,
        expr_impl: ExprImpl::Ap { fun: fun, arg: arg },
    }))
}

fn eval(_expr: ExprPtr, functions: &FunctionMap, depth: usize) -> ExprPtr {
    let mut expr = Rc::clone(&_expr);
    if let Some(evaluated) = &expr.borrow().evaluated {
        return Rc::clone(&evaluated);
    }
    let mut initial_expr = Rc::clone(&expr);
    let indent = " ".repeat(MUL * depth);
    loop {
        // println!("{}>> eval on {}", indent, to_str(&expr.borrow()));
        let result = try_eval(Rc::clone(&expr), functions, depth + 1);
        // println!(
        //     "{}<< evaled on {} --> {}",
        //     indent,
        //     to_str(&expr.borrow()),
        //     to_str(&result.borrow())
        // );
        // println!("{}checking equality", indent);
        if result.borrow().expr_impl == expr.borrow().expr_impl {
            // println!("{}returning result", indent);
            initial_expr.borrow_mut().evaluated = Some(Rc::clone(&result));
            return result;
        }
        expr = result;
    }
}

fn try_eval(expr: ExprPtr, functions: &FunctionMap, depth: usize) -> ExprPtr {
    if let Some(evaluated) = &expr.borrow().evaluated {
        return Rc::clone(&evaluated);
    }
    let indent = " ".repeat(MUL * depth);
    // println!("{}try eval {}", indent, to_str(&expr.borrow()));
    if let ExprImpl::Atom(name) = &expr.borrow().expr_impl {
        if functions.contains_key(name) {
            return functions[name].clone();
        }
    }
    if let ExprImpl::Ap { fun: fun_expr, arg } = &expr.borrow().expr_impl {
        // ap(fun, x)
        // println!("{}inside ap(fun, x)", indent);
        let fun = eval(fun_expr.clone(), functions, depth + 1);
        let x = arg.clone();
        if let ExprImpl::Atom(fun_name) = &fun.borrow().expr_impl {
            // ap(fun, x)
            if fun_name == "neg" {
                return atom_int(-as_number(eval(x, functions, depth + 1)));
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
        let fun_borrowed = fun.borrow();
        if let ExprImpl::Ap {
            fun: fun2_expr,
            arg: arg2,
        } = &fun_borrowed.expr_impl
        {
            // ap(ap(fun2, y), x)
            // println!("{}inside ap(ap(fun2, y), x), fun2 = {}", indent, to_str(&fun2_expr.borrow()));
            let fun2 = eval(fun2_expr.clone(), functions, depth + 1);
            let y = arg2.clone();            
            if let ExprImpl::Atom(fun2_name) = &fun2.borrow().expr_impl {
                // println!("{}inside ap(ap({}, y), x)", indent, fun2_name);
                if fun2_name == "t" {
                    return y;
                }
                if fun2_name == "f" {
                    return x;
                }
                if fun2_name == "add" {
                    return atom_int(
                        as_number(eval(x, functions, depth + 1))
                            + as_number(eval(y, functions, depth + 1)),
                    );
                }
                if fun2_name == "mul" {
                    return atom_int(
                        as_number(eval(x, functions, depth + 1))
                            * as_number(eval(y, functions, depth + 1)),
                    );
                }
                if fun2_name == "div" {
                    return atom_int(
                        as_number(eval(y, functions, depth + 1))
                            / as_number(eval(x, functions, depth + 1)),
                    );
                }
                if fun2_name == "lt" {
                    return if as_number(eval(y, functions, depth + 1))
                        < as_number(eval(x, functions, depth + 1))
                    {
                        t()
                    } else {
                        f()
                    };
                }
                if fun2_name == "eq" {
                    return if as_number(eval(x, functions, depth + 1))
                        == as_number(eval(y, functions, depth + 1))
                    {
                        t()
                    } else {
                        f()
                    };
                }
                if fun2_name == "cons" {
                    return eval_cons(y, x, &functions, depth + 1);
                }
            }
            let fun2_borrowed = fun2.borrow();
            if let ExprImpl::Ap {
                fun: fun3_expr,
                arg: arg3,
            } = &fun2_borrowed.expr_impl
            {
                let fun3 = eval(fun3_expr.clone(), functions, depth + 1);
                let z = eval(arg3.clone(), functions, depth + 1);
                let fun3_borrowed = fun3.borrow();
                if let ExprImpl::Atom(fun3_name) = &fun3_borrowed.expr_impl {
                    if fun3_name == "s" {
                        // println!("Running s with x = {}", to_str(&x.borrow()));
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

fn eval_cons(a: ExprPtr, b: ExprPtr, functions: &FunctionMap, depth: usize) -> ExprPtr {
    // println!("running eval_cons..");
    let mut res = ap(
        ap(cons(), eval(a, functions, depth + 1)),
        eval(b, functions, depth + 1),
    );
    res.borrow_mut().evaluated = Some(Rc::clone(&res));
    res
}

fn as_number(expr: ExprPtr) -> i64 {
    match &expr.borrow().expr_impl {
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
        let value = eval(expr, &functions, 0);
        let value_borrowed = value.borrow();
        if let ExprImpl::Atom(name) = &value_borrowed.expr_impl {
            assert_eq!(name, expected);
        } else {
            assert!(false, "Unexpected result: {:?}", to_str(&value_borrowed));
        }
    }

    #[test]
    fn test_eval1() {
        let expr = ap(atom("neg"), atom("14"));
        let functions: FunctionMap = HashMap::new();
        let value = eval(expr, &functions, 0);
        let value_borrowed = value.borrow();
        if let ExprImpl::Atom(name) = &value_borrowed.expr_impl {
            assert_eq!(name, "-14");
        } else {
            assert!(false, "Unexpected result: {:?}", &value_borrowed.expr_impl);
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
    fn test_eval_cons() {
        let expr = ap(ap(cons(), atom("1")), atom("5"));
        let functions: FunctionMap = HashMap::new();
        let expr2 = try_eval(expr.clone(), &functions, 0);
        assert_eq!(expr.borrow().expr_impl, expr2.borrow().expr_impl);
        println!("{}", to_str(&expr.borrow()));
        println!("{}", to_str(&expr2.borrow()));
        let exp3 = eval(expr.clone(), &functions, 0);
    }

    #[test]
    fn test_s_combinator() {
        // assert_parse_eval("ap ap ap s mul ap add 1 6", "42");
        assert_parse_eval("ap ap ap s mul ap add 1 ap ap add 2 4", "42");
    }

    #[test]
    fn test_rc_clone() {
        // let mut x = parse_from_string(&"ap ap add 2 4".to_string());
        // let mut x2 = Rc::clone(&x);
        // println!("x = {:?}", &x);
        // println!("x2 = {:?}", &x2);

        // let functions: FunctionMap = HashMap::new();
        // let y = eval(Rc::clone(&x), &functions);
        // println!("y = {:?}", &y);
        // println!("x = {:?}", &x);
        // println!("x2 = {:?}", &x2);
        // assert_eq!(y.borrow().evaluated.is_some(), true);
    }
}

fn main() {
    println!("Hello, Galaxy!");
    run_galaxy();
}
