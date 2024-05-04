use std::collections::{HashMap, HashSet};

use crate::ast::*;

pub type TypeJudgements = HashMap<String, u32>;

type Env = HashMap<String, ()>;

#[derive(Debug)]
pub enum TypeError {
    WrongNumberOfArgs(String, u32, u32),
    NotDefined(Vec<String>),
}

pub fn type_check(ast: &Ast) -> Result<TypeJudgements, TypeError> {
    let mut type_information: TypeJudgements = HashMap::new();
    let mut not_bound = HashSet::new();

    for def in ast {
        type_check_def(def, &mut type_information, &mut not_bound)?;
    }

    if !not_bound.is_empty() {
        return Err(TypeError::NotDefined(not_bound.iter().cloned().collect()));
    }

    Ok(type_information)
}

fn type_check_def(
    def: &Definitions,
    type_info: &mut TypeJudgements,
    not_bound: &mut HashSet<String>,
) -> Result<(), TypeError> {
    match def {
        // Definitions::Import(_, _) => todo!(),
        Definitions::Defun(name, args, expr) => {
            match type_info.get(name) {
                Some(n) => {
                    not_bound.remove(name);
                    if *n == args.len() as u32 {
                        return Err(TypeError::WrongNumberOfArgs(name.clone(), args.len() as u32, *n));
                    }
                }
                None => {
                    type_info.insert(name.clone(), args.len() as u32);
                }
            }

            let mut env: Env = HashMap::new();

            let _ = type_check_expr(expr.clone(), type_info, not_bound, &mut env)?;
        }
        // Definitions::Const(_, _, _) => todo!(),
    }

    return Ok(());
}

fn add_temporary_env<A>(name: &String, env: &mut Env, mut f: impl FnMut(&mut Env) -> A) -> A {

    let old = env.insert(name.clone(), ());
    let ret = f(env);

    match old {
        Some(n) => env.insert(name.clone(), n),
        None => env.remove(name),
    };

    ret
}

fn type_check_expr(
    expr: std::rc::Rc<Expr>,
    type_info: &mut TypeJudgements,
    not_bound: &mut HashSet<String>,
    env: &mut Env,
) -> Result<(), TypeError> {
    match expr.as_ref() {
        Expr::Let(name, value, ret) => {
            type_check_expr(value.clone(), type_info, not_bound, env)?;
            add_temporary_env(name, env, |env| {
                type_check_expr(ret.clone(), type_info, not_bound, env)
            })?;
        }
        Expr::Identifier(id) => (),
        Expr::Primitive(_) => (),
        Expr::App(func, args) => {
            args.iter()
                .map(|arg| type_check_expr(arg.clone(), type_info, not_bound, env))
                .collect::<Result<Vec<()>, TypeError>>()?;

            check_args(func, args.len() as u32, type_info, not_bound)?;
        }
        Expr::Value(_) => {},
    }

    Ok(())
}

fn check_args(
    func: &Expr,
    len: u32,
    type_info: &mut TypeJudgements,
    not_bound: &mut HashSet<String>,
) -> Result<(), TypeError> {
    match func {
        Expr::Identifier(name) => match type_info.get(name) {
            Some(n) => {
                if len != *n {
                    return Err(TypeError::WrongNumberOfArgs(name.clone(), *n, len));
                }
            }
            None => {
                type_info.insert(name.clone(), len);
                not_bound.insert(name.clone());
            }
        },
        Expr::Primitive(prim) => {
            let num_args = match prim {
                Primitive::Print => 1,
                Primitive::Get => 0,
                Primitive::Add => 2,
                Primitive::Sub => 2,
                Primitive::Mult => 2,
                Primitive::Neg => 1,
                Primitive::Div => 2,
                Primitive::Mod => 2,
                Primitive::GT => 2,
                Primitive::GE => 2,
                Primitive::LE => 2,
                Primitive::LT => 2,
                Primitive::IF => 3,
                Primitive::EQ => 2,
            };

            if len != num_args {
                return Err(TypeError::WrongNumberOfArgs(format!("{:?}",prim), num_args, len));
            }
        },
        _ => panic!("not yet supported (calling functions/primitives inderectly)"),
    }

    Ok(())
}
