use std::collections::HashMap;

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

pub enum Literal {
    BoolLiteral(bool),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    CommandLiteral(String),
}

impl Literal {
    pub fn bool_from_str(from: &str) -> Self {
        Self::BoolLiteral(from.parse::<bool>().unwrap())
    }

    pub fn int_from_str(from: &str) -> Self {
        Self::IntLiteral(from.parse::<i64>().unwrap())
    }

    pub fn float_from_str(from: &str) -> Self {
        Self::FloatLiteral(from.parse::<f64>().unwrap())
    }

    /// This funcion constructs a string variant and also fixes escape sequences in the passed
    /// string
    pub fn string_from_str(from: &str) -> Self {
        //TODO implement this
        Self::StringLiteral(from.into())
    }

    pub fn command_from_str(from: &str) -> Self {
        Self::CommandLiteral(from.to_string())
    }
}

pub enum Primitive {
    Boolean,
    Int,
    Float,
    String,
}
