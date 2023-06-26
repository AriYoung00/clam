use std::collections::HashMap;
use crate::tokens::{Primitive, Literal, UnaryOperator, BinaryOperator, Identifier};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Block {
    pub statements: Vec<Statement>
}

impl Block {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self{ statements }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FnDef {
    pub name: String,
    pub param_list: HashMap<Identifier, Type>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Primitive(Primitive),
    Name(Identifier),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Conditional {
    cond: Expr,
    body: Block,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WhileLoop {
    cond: Expr,
    body: Block,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ForLoop {
    var: Identifier,
    iter: Expr,
    body: Block,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    Block(Block),
    FnDef(FnDef),
    Let(Identifier, Option<Type>, Option<Expr>),
    Assign(Identifier, Expr),
    Expr(Expr),
    Break,
    Conditional(Conditional),
    WhileLoop(WhileLoop),
    ForLoop(ForLoop),
}

// pub enum BinaryOperator {
//     Add, Sub,
//     Mul, Div,
//     Less, LessEq, Greater, GreaterEq,
//     Eq, NotEq, And, Or
// }

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Literal(Literal),
    BinOp(BinaryOperator, Box<Expr>, Box<Expr>),
    UnOp(UnaryOperator, Box<Expr>),
    Identifier(Identifier),
    FnCall(String, Vec<Expr>)
}

