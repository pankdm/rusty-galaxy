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

fn read_galaxy() {
    let lines = read_input("galaxy.txt");
    for l in &lines {
        let parts = split_string(l, " = ");
        let name = parts[0].to_string();
        let tokens = split_string(&parts[1], " ");
        println!("{} -> {} tokens: {:?}", &name, tokens.len(), &tokens);
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum ExprType {
    Atom,
    Ap,
}

type ExprPtr = Rc<Expr>;
// type ExprLink = Option<ExprPtr>;

#[derive(Debug, Clone, PartialEq)]
struct Expr {
    evaluated: Option<RefCell<ExprPtr>>,
    expr_type: ExprType,
    // Atom fields:
    name: String,
    // Ap fields:
    fun: Option<ExprPtr>,
    arg: Option<ExprPtr>,
}

// TODO: find a library class for that
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

fn create_t() -> ExprPtr {
    atom("t")
}

fn create_f() -> ExprPtr {
    atom("f")
}

fn atom_string(name: String) -> ExprPtr {
    Rc::new(Expr {
        evaluated: None,
        expr_type: ExprType::Atom,
        name: name,
        fun: None,
        arg: None,
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
        expr_type: ExprType::Ap,
        name: "ap".to_string(),
        fun: Some(fun),
        arg: Some(arg),
    })
}

fn eval(_expr: ExprPtr) -> ExprPtr {
    let mut expr = _expr.clone();
    if let Some(evaluated) = &expr.evaluated {
        return evaluated.borrow().clone();
    }
    let mut initial_expr = (*expr.clone()).clone();
    loop {
        let result = try_eval(expr.clone());
        if *result == *expr {
            let result_expr = RefCell::new(result.clone());
            let some = Some(result_expr);
            initial_expr.evaluated = some;
            return result;
        }
        expr = result;
    }
}

fn try_eval(expr: ExprPtr) -> ExprPtr {
    if let Some(evaluated) = &expr.evaluated {
        return evaluated.borrow().clone();
    }
    println!("  Evaluating --> {:?}", expr);
    if expr.expr_type == ExprType::Ap {
        let fun_expr = expr.fun.as_ref().unwrap().clone();
        let fun = eval(fun_expr);
        let x = expr.arg.as_ref().unwrap().clone();
        if fun.expr_type == ExprType::Atom {
            if fun.name == "neg" {
                return atom_int(-as_number(eval(x)));
            }
        } else if fun.expr_type == ExprType::Ap {
            let fun2 = eval(fun.fun.as_ref().unwrap().clone());
            let y = fun.arg.as_ref().unwrap().clone();
            if fun2.expr_type == ExprType::Atom {
                if fun2.name == "add" {
                    return atom_int(as_number(eval(x)) + as_number(eval(y)));
                } else if fun2.name == "mul" {
                    return atom_int(as_number(eval(x)) * as_number(eval(y)));
                } else if fun2.name == "div" {
                    return atom_int(as_number(eval(y)) / as_number(eval(x)));
                } else if fun2.name == "eq" {
                    return if as_number(eval(x)) == as_number(eval(y)) {
                        create_t()
                    } else {
                        create_f()
                    };
                } else if fun2.name == "lt" {
                    return if as_number(eval(y)) < as_number(eval(x)) {
                        create_t()
                    } else {
                        create_f()
                    };
                } else {
                    panic!("Unimplemented operator: {:?}", fun2.name);
                }
            }
        } else {
            panic!("Unimplemented expr_type: {:?}", fun.expr_type);
        }
    }
    expr
}

fn as_number(expr: ExprPtr) -> i64 {
    if expr.expr_type == ExprType::Atom {
        return parse_i64(&expr.name);
    }
    panic!("{:?} is not a number", *expr);
}

// Expr try_eval(Expr expr)
//     if (expr.Evaluated != null)
//         return expr.Evaluated
//     if (expr is Atom && functions[expr.Name] != null)
//         return functions[expr.Name]
//     if (expr is Ap)
//         Expr fun = eval(expr.Fun)
//         Expr x = expr.Arg
//         if (fun is Atom)
//             if (fun.Name == "neg") return Atom(-asNum(eval(x)))
//             if (fun.Name == "i") return x
//             if (fun.Name == "nil") return t
//             if (fun.Name == "isnil") return Ap(x, Ap(t, Ap(t, f)))
//             if (fun.Name == "car") return Ap(x, t)
//             if (fun.Name == "cdr") return Ap(x, f)
//         if (fun is Ap)
//             Expr fun2 = eval(fun.Fun)
//             Expr y = fun.Arg
//             if (fun2 is Atom)
//                 if (fun2.Name == "t") return y
//                 if (fun2.Name == "f") return x
//                 if (fun2.Name == "add") return Atom(asNum(eval(x)) + asNum(eval(y)))
//                 if (fun2.Name == "mul") return Atom(asNum(eval(x)) * asNum(eval(y)))
//                 if (fun2.Name == "div") return Atom(asNum(eval(y)) / asNum(eval(x)))
//                 if (fun2.Name == "lt") return asNum(eval(y)) < asNum(eval(x)) ? t : f
//                 if (fun2.Name == "eq") return asNum(eval(x)) == asNum(eval(y)) ? t : f
//                 if (fun2.Name == "cons") return evalCons(y, x)
//             if (fun2 is Ap)
//                 Expr fun3 = eval(fun2.Fun)
//                 Expr z = fun2.Arg
//                 if (fun3 is Atom)
//                     if (fun3.Name == "s") return Ap(Ap(z, x), Ap(y, x))
//                     if (fun3.Name == "c") return Ap(Ap(z, x), y)
//                     if (fun3.Name == "b") return Ap(z, Ap(y, x))
//                     if (fun3.Name == "cons") return Ap(Ap(x, z), y)
//     return expr



#[cfg(test)]
mod tests {
    use super::*;

    fn assert_parse_eval(input: &str, expected: &str) {
        let expr = parse_from_string(&input.to_string());
        let value = eval(expr);
        assert_eq!(value.expr_type, ExprType::Atom);
        assert_eq!(value.name, expected, "input = {}", input);
    }

    #[test]
    fn test_eval1() {
        let expr = ap(atom("neg"), atom("14"));
        let value = eval(expr);
        println!("{:?}", value);
        assert_eq!(value.expr_type, ExprType::Atom);
        assert_eq!(value.name, "-14");
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

}

fn main() {
    println!("Hello, Galaxy!");
    // read_galaxy();
}
