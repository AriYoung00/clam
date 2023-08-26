use derive_more::{From, Constructor};
use either::Either;
use ordered_float::OrderedFloat;

pub type SpannedRes<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;
pub type Spanned<T, Loc> = (Loc, T, Loc);
pub type Span<T> = Spanned<T, usize>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntType;
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FloatType;
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BoolType;
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringType;
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CommandType;
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Primitive {
    Int(IntType),
    Float(FloatType),
    Bool(BoolType),
    String(StringType),
    Command(CommandType)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Int(i64),
    Float(OrderedFloat<f64>),
    Bool(bool),
    String(String),
    Command(String),
    Struct(Span<Identifier>, Vec<(Span<Identifier>, Span<Box<Expr>>)>),
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
    BitXor,
}

#[derive(Hash, Clone, Debug, PartialEq, Eq, From)]
#[from(forward)]
pub struct Identifier(pub String);

#[derive(Clone, Debug, PartialEq, Eq, From, Constructor)]
#[from(forward)]
pub struct Block {
    pub body: Vec<Span<Statement>>,
    pub last: Option<Span<Statement>>,
}

pub type Param = (Span<Identifier>, Option<Span<Type>>);

#[derive(Clone, Debug, PartialEq, Eq, Constructor)]
pub struct FnDef {
    pub name: Span<Identifier>,
    pub param_list: Vec<Param>,
    pub ret_type: Option<Span<Type>>,
    pub body: Span<Box<Expr>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Constructor)]
pub struct Let {
    pub id: Span<Identifier>,
    pub r#type: Option<Span<Type>>,
    pub expr: Option<Span<Box<Expr>>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Constructor)]
pub struct Assign {
    pub id: Span<Identifier>,
    pub expr: Span<Box<Expr>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Constructor)]
pub struct FunctionType {
    pub params: Vec<Type>,
    pub return_type: Option<Box<Type>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Any;
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Primitive(Primitive),
    Name(Identifier),
    Sum(Vec<Type>),
    Any(Any),
    Function(FunctionType),
}

#[derive(Clone, Debug, PartialEq, Eq, Constructor)]
pub struct Conditional {
    pub cond: Span<Box<Expr>>,
    pub body: Block,
    pub r#else: Option<Block>,
}

#[derive(Clone, Debug, PartialEq, Eq, Constructor)]
pub struct WhileLoop {
    pub cond: Span<Box<Expr>>,
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq, Eq, Constructor)]
pub struct ForLoop {
    pub var: Span<Identifier>,
    pub iter: Span<Box<Expr>>,
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq, Eq, Constructor)]
pub struct BinOp {
    pub op: BinaryOperator,
    pub lhs: Span<Box<Expr>>,
    pub rhs: Span<Box<Expr>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Constructor)]
pub struct UnOp {
    pub op: UnaryOperator,
    pub rhs: Span<Box<Expr>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Constructor)]
pub struct FnCall {
    pub id: Span<Identifier>,
    pub args: Vec<Span<Box<Expr>>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Constructor)]
pub struct LambdaDef {
    pub params: Vec<Param>,
    pub body: Span<Box<Expr>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    FnDef(FnDef),
    Let(Let),
    Assign(Assign),
    Expr(Span<Box<Expr>>),
    Break,
    Return(Option<Span<Box<Expr>>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Literal(Literal),
    BinOp(BinOp),
    UnOp(UnOp),
    Identifier(Identifier),
    FnCall(FnCall),
    LambdaDef(LambdaDef),
    Block(Block),
    Conditional(Conditional),
    WhileLoop(WhileLoop),
    ForLoop(ForLoop),
}

#[derive(Clone, Debug, PartialEq, Eq, Constructor)]
pub struct StructDef {
    name: Span<Identifier>,
    fields: Vec<(Span<Identifier>, Option<Span<Type>>)>,
}

pub struct Mod(pub Vec<Either<FnDef, StructDef>>);
