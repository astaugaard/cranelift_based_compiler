use std::{collections::HashMap, rc::Rc};

use crate::{ast::*, type_checking::TypeJudgements};

pub type Anf = Vec<AnfDefinition>;

pub type GlobalId = usize;
pub type LocalId = usize;

#[derive(Debug)]
pub enum IrType {
    I32,
}

#[derive(Debug)]
pub enum AnfDefinition {
    Function(GlobalId,String,usize,AnfExpr,LocalId), // localId is the next one avaible to be bound
                                                 // ExternFunction(GlobalId, Vec<LocalId>),
}

pub type AnfExpr = (Vec<Rc<AnfClause>>, Rc<AnfVal>);

#[derive(Debug)]
pub enum AnfClause {
    // Let(LocalId, IrType, Rc<AnfVal>),
    LetApp(LocalId, IrType, Rc<AnfVal>, Vec<Rc<AnfVal>>),
    LetIf(LocalId, IrType, Rc<AnfVal>, AnfExpr, AnfExpr),
}

#[derive(Clone, Debug)]
pub enum AnfVal {
    Var(LocalId),
    Func(GlobalId),
    Primitive(Primitive),
    // GlobalId(GlobalId),
    Const(i32),
    // Lambda(LocalId,AnfExpr),
}

pub fn ast_to_anf(ast: Ast, _types: &TypeJudgements) -> (Anf,usize) {
    let mut names_to_ids_map = HashMap::new();
    let mut ids_to_names: Vec<String> = Vec::new();

    for (id, i) in ast.iter().enumerate() {
        match i {
            // Definitions::Import(_, _) => todo!(),
            Definitions::Defun(name, _, _) => {
                names_to_ids_map.insert(name.clone(), id);
                ids_to_names.push(name.clone());
            } // Definitions::Const(_, _, _) => todo!(),
        }
    }

    (ast.iter()
        .map(|ast| {
            definition_to_anf(
                ast,
                &names_to_ids_map,
            )
        })
        .collect::<Vec<AnfDefinition>>(),ast.len())
}

fn definition_to_anf(
    ast: &Definitions,
    // types: &TypeJudgements,
    globals: &HashMap<String, usize>,
) -> AnfDefinition {
    match ast {
        // Definitions::Import(_, _) => todo!(),
        Definitions::Defun(name, args, expr) => {
            let func_id = globals.get(name).unwrap();

            let mut next_id = args.len();

            let mut local_map = HashMap::new();

            for (id, a) in args.iter().enumerate() {
                local_map.insert(a.0.clone(), AnfVal::Var(id));
            }

            let mut clauses = Vec::new();

            let ret_id = expr_to_val(expr, &mut next_id, globals, &mut local_map, &mut clauses);

            AnfDefinition::Function(*func_id,name.to_string(), args.len(), (clauses, Rc::new(ret_id)), next_id)
        } // Definitions::Const(_, _, _) => todo!(),
    }
}

fn expr_to_val(
    expr: &Expr,
    next_id: &mut usize,
    global_map: &HashMap<String, usize>,
    local_map: &mut HashMap<String, AnfVal>,
    expr_vec: &mut Vec<Rc<AnfClause>>,
) -> AnfVal {
    match expr {
        Expr::Let(name, value, rest) => {
            let val = expr_to_val(value, next_id, global_map, local_map, expr_vec);

            local_map.insert(name.clone(), val);

            expr_to_val(rest, next_id, global_map, local_map, expr_vec)
        }
        Expr::Identifier(id) => {
            match local_map.get(id) {
                Some(val) => val.clone(),
                None => {
                    let id = global_map.get(id).unwrap();
                    AnfVal::Func(*id) // todo when adding globals update this to be able to deal with global variables
                }
            }
        }
        Expr::Primitive(prim) => AnfVal::Primitive(*prim),
        Expr::App(fun, args) => {
            let fun = expr_to_val(fun, next_id, global_map, local_map, expr_vec);

            match fun {
                AnfVal::Primitive(Primitive::IF) => {
                    let id = *next_id;
                    *next_id += 1;

                    assert!(args.len() == 3);

                    let cond = expr_to_val(&args[0], next_id, global_map, local_map, expr_vec);

                    expr_vec.push(Rc::new(AnfClause::LetIf(
                        id,
                        IrType::I32,
                        Rc::new(cond),
                        expr_to_expr(&args[1], next_id, global_map, local_map),
                        expr_to_expr(&args[2], next_id, global_map, local_map),
                    )));

                    AnfVal::Var(id)
                }
                _ => {
                    let args = args
                        .iter()
                        .map(|arg| {
                            Rc::new(expr_to_val(arg, next_id, global_map, local_map, expr_vec))
                        })
                        .collect::<Vec<Rc<AnfVal>>>();

                    let id = *next_id;
                    *next_id += 1;

                    expr_vec.push(Rc::new(AnfClause::LetApp(
                        id,
                        IrType::I32,
                        Rc::new(fun),
                        args,
                    )));

                    AnfVal::Var(id)
                }
            }
        }
        Expr::Value(value) => AnfVal::Const(*value),
    }
}

fn expr_to_expr(
    expr: &Expr,
    next_id: &mut usize,
    global_map: &HashMap<String, usize>,
    local_map: &mut HashMap<String, AnfVal>,
) -> (Vec<Rc<AnfClause>>, Rc<AnfVal>) {
    let mut expr_vec = Vec::new();

    let ret = expr_to_val(expr, next_id, global_map, local_map, &mut expr_vec);

    (expr_vec, Rc::new(ret))
}
