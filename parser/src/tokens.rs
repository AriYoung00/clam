use logos::{Logos, Lexer};
use ordered_float::OrderedFloat;

#[derive(Logos, Clone, Debug, PartialEq, Eq)]
pub enum Token {
    #[token("struct")]
    Struct,
    #[token("fn")]
    Fn,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("return")]
    Return,
    #[token("in")]
    In,
    #[token("let")]
    Let,
    #[token("var")]
    Var,

    #[token("bool")]
    Bool,
    #[token("int")]
    Int,
    #[token("float")]
    Float,
    #[token("string")]
    String,
    #[token("cmd")]
    Cmd,

    #[regex("[_a-zA-Z][_0-9a-zA-Z]*", |lex| lex.slice().parse().ok())]
    Identifier(String),

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Asterisk,
    #[token("/")]
    Slash,
    #[token("%")]
    Modulus,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,
    #[token("==")]
    EqualEqual,
    #[token("!=")]
    NotEqual,
    #[token("&&")]
    LogicalAnd,
    #[token("||")]
    LogicalOr,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(":")]
    Colon,
    #[token(";")]
    SemiColon,
    #[token("=")]
    Equal,
    #[token(",")]
    Comma,
    #[token("->")]
    Arrow,
    #[token("=>")]
    ThiccArrow,
    #[token(".")]
    Dot,
    #[token("#")]
    Hash,
    #[token("&")]
    Ampersand,
    #[token("!")]
    ExclamationMark,
    #[token("?")]
    QuestionMark,
    #[token("|")]
    Pipe,
    #[token("\\")]
    Bslash,

    #[regex("true|false", |tok| tok.slice().parse())]
    BoolLit(bool),
    #[regex(r#""(\\.|[^\\"\n])*""#, |tok| tok.slice()[1..tok.slice().len()-1].parse())]
    StrLit(String),
    #[regex(r"\d+", |tok| tok.slice().parse())]
    IntLit(i64),
    #[regex(r"\d+\.\d*", |tok| tok.slice().parse())]
    FloatLit(OrderedFloat<f64>),
    #[regex(r"`(\\.|[^\\`\n])*`", |tok| tok.slice()[1..tok.slice().len()-1].parse())]
    CommandLit(String),
    
    #[regex(r"//.*\n?", logos::skip)]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[error]
    Error,
}

