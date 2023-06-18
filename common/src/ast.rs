use std::collections::HashMap;
use crate::tokens::{Primitive, Literal};

pub struct Identifier(String);

pub struct Block {
    pub statements: Vec<Statement>
}

impl Block {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self{ statements }
    }
}

pub struct FnDef {
    pub name: String,
    pub param_list: HashMap<Identifier, Type>,
}

pub enum Type {
    Primitive(Primitive),
    Name(Identifier),
}

pub struct Conditional {
    cond: Expr,
    body: Block,
}

pub struct WhileLoop {
    cond: Expr,
    body: Block,
}

pub struct ForLoop {
    var: Identifier,
    iter: Expr,
    body: Block,
}

pub enum Statement {
    FnDef(FnDef),
    Let(Identifier, Option<Type>, Option<Box<Expr>>),
    Assign(Identifier, Box<Expr>),
    Expr(Expr),
    Break,
    Conditional(Conditional),
    WhileLoop(WhileLoop),
    ForLoop(ForLoop),
}

pub enum BinaryOperator {
    Add, Sub,
    Mul, Div,
    Less, LessEq, Greater, GreaterEq,
    Eq, NotEq, And, Or
}

pub enum Expr {
    Literal(Literal),
    BinOp(BinaryOperator, Box<Expr>, Box<Expr>),
    Identifier(Identifier),
    FnCall(String, Vec<Expr>)
}

