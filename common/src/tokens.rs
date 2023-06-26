use derive_more::{FromStr, From};
use ordered_float::OrderedFloat;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum Keyword {
    Fn,
    If,
    Else,
    For,
    While,
    Break,
    Continue,
    In,
    Let,
    Var,
}

#[derive(Clone, Debug, PartialEq, Hash)]
#[allow(dead_code)]
pub enum Literal {
    Bool(bool),
    Str(String),
    Int(i64),
    Float(OrderedFloat<f64>),
    Command(String),
}

impl Eq for Literal {}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Mul,
    Div,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    And,
    Or
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum UnaryOperator {
    Negate,
    Not,
    Ref,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum Symbol {
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    LAngle,
    RAngle,
    Colon,
    SemiColon,
    Equal,
    Comma,
    Arrow,
    Dot,
    Star,
    Slash,
    Plus,
    Minus,
    Hash,
    Ampersand,
    ExclamationMark,
    QuestionMark,
    LessThanEqual,
    GreaterThanEqual,
    EqualEqual,
    NotEqual,
    And,
    Or,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Primitive {
    Bool,
    Int,
    Float,
    String,
}

#[derive(Clone, Debug, PartialEq, FromStr, From, Eq, Hash)]
#[from(forward)]
pub struct Identifier(String);



#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum Token {
    Keyword(Keyword),
    Literal(Literal),
    Identifier(Identifier),
    BinOp(BinaryOperator),
    UnOp(UnaryOperator),
    Symbol(Symbol),
    Primitive(Primitive)
}

