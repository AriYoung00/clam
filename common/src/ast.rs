use derive_more::From;
use ordered_float::OrderedFloat;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Primitive {
    Int,
    Float,
    Bool,
    String,
    Command
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Int(i64),
    Float(OrderedFloat<f64>),
    Bool(bool),
    String(String),
    Command(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
    Not,
    BitInvert,
    Negate,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Times,
    Div,
    Mod,
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Ne,
    And,
    Or,
    BitAnd,
    BitOr,
}

#[derive(Clone, Debug, PartialEq, Eq, From)]
#[from(forward)]
pub struct Identifier(String);

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
    pub param_list: Vec<(Identifier, Option<Type>)>,
    pub ret_type: Option<Type>,
    pub body: Expr,
}

/*
fn (a, b) {
    if a {
        "thing"
    }
    else {
        // nothing implicitly returns `()`
    }
}
 */

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Primitive(Primitive),
    Name(Identifier),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Conditional {
    pub cond: Box<Expr>,
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WhileLoop {
    pub cond: Box<Expr>,
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ForLoop {
    pub var: Identifier,
    pub iter: Box<Expr>,
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    FnDef(FnDef),
    Let(Identifier, Option<Type>, Option<Expr>),
    Assign(Identifier, Expr),
    Expr(Expr),
    Break,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Literal(Literal),
    BinOp(BinaryOperator, Box<Expr>, Box<Expr>),
    UnOp(UnaryOperator, Box<Expr>),
    Identifier(Identifier),
    FnCall(Identifier, Vec<Expr>),
    Block(Vec<Statement>),
    Conditional(Box<Expr>, Block),
    WhileLoop(Box<Expr>, Block),
    ForLoop(Identifier, Box<Expr>, Block),
}

