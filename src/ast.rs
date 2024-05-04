use std::rc::Rc;

pub type Ast = Vec<Definitions>;

#[derive(Debug)]
pub enum Definitions {
    // Import(String,Option<Vec<String>>),
    Defun(String,Vec<(String,Rc<Type>)>,Rc<Expr>),
    // Const(String,Type,Rc<Expr>),
}

#[derive(Clone,Debug)]
pub enum Type {
    I32,
    // Func(Vec<Rc<Type>>)
}

#[derive(Debug)]
pub enum Expr {
    Let(String,Rc<Expr>,Rc<Expr>),
    Identifier(String),
    Primitive(Primitive),
    App(Rc<Expr>,Vec<Rc<Expr>>),
    Value(i32),
}

#[derive(Clone,Debug,Copy,PartialEq,Eq,PartialOrd,Ord)]
pub enum Primitive {
    Print,
    Get,
    Add,
    Sub,
    Mult,
    Neg,
    Div,
    Mod,
    GT,
    GE,
    LE,
    LT,
    IF,
    EQ,
}
