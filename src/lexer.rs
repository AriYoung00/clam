use chumsky::prelude::*;
use chumsky::Parser;

#[derive(Clone, Debug, PartialEq)]
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
}

#[derive(Clone, Debug, PartialEq)]
#[allow(dead_code)]
pub enum Literal {
    Bool(bool),
    Str(String),
    Int(i64),
    Float(f64),
    Command(String),
}

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
#[allow(dead_code)]
pub enum UnaryOperator {
    Negate,
    Not,
    Ref,
}

#[derive(Clone, Debug, PartialEq)]
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
    QuestionMark,
    LessThanEqual,
    GreaterThanEqual,
    EqualEqual,
    NotEqual,
    And,
    Or,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Primitive {
    Bool,
    Int,
    Float,
    String,
}


#[derive(Clone, Debug, PartialEq)]
#[allow(dead_code)]
pub enum Token {
    Keyword(Keyword),
    Literal(Literal),
    Identifier(String),
    BinOp(BinaryOperator),
    UnOp(UnaryOperator),
    Symbol(Symbol),
    Primitive(Primitive)
}

pub fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    let tok = |c| just(c).padded();

    let primitive = text::keyword("bool").to(Primitive::Bool)
        .or(text::keyword("int")     .to(Primitive::Int))
        .or(text::keyword("float")   .to(Primitive::Float))
        .or(text::keyword("string")  .to(Primitive::String))
        .padded()
        .map(Token::Primitive);

    let keyword = text::keyword("fn").to(Keyword::Fn)
        .or(text::keyword("if")      .to(Keyword::If))
        .or(text::keyword("else")    .to(Keyword::Else))
        .or(text::keyword("for")     .to(Keyword::For))
        .or(text::keyword("while")   .to(Keyword::While))
        .or(text::keyword("break")   .to(Keyword::Break))
        .or(text::keyword("continue").to(Keyword::Continue))
        .or(text::keyword("in")      .to(Keyword::In))
        .or(text::keyword("let")     .to(Keyword::Let))
        .padded()
        .map(Token::Keyword);


    let _str_interior = just::<_, &str, _>("\"")
        .not()
        .repeated();

    let str_lit = just('"')
        .ignore_then(_str_interior)
        .then_ignore(just('"'))
        .padded()
        .collect()
        .map(Literal::Str)
        .map(Token::Literal);

    let int_lit = text::int(10)
        .padded()
        .map(|s: String| Literal::Int(s.parse().unwrap()))
        .map(Token::Literal);

    // TODO: Make this not suck
    let float_lit = text::int(10)
        .then_ignore(just("."))
        .then(text::int(10))
        .padded()
        .map(|(whole, part)| Literal::Float(format!("{whole}.{part}").parse().unwrap()))
        .map(Token::Literal);

    let bool_lit = text::keyword("true").to(true).map(Literal::Bool).map(Token::Literal)
        .or(text::keyword("false").to(false).map(Literal::Bool).map(Token::Literal));

    let cmd_lit_interior = just("```")
        .not()
        .repeated();

    let cmd_lit = just("```")
        .ignore_then(cmd_lit_interior)
        .then_ignore(just("```"))
        .padded()
        .collect()
        .map(Literal::Command)
        .map(Token::Literal);

    // For now just let chumsky do identifiers
    let ident = text::ident()
        .padded()
        .map(|i| Token::Identifier(i));


    let braces = tok(")").to(Symbol::RParen)
        .or(tok("{").to(Symbol::LBrace))
        .or(tok("}").to(Symbol::RBrace))
        .or(tok("(").to(Symbol::LParen))
        .or(tok(")").to(Symbol::RParen))
        .or(tok("[").to(Symbol::LBracket))
        .or(tok("]").to(Symbol::RBracket))
        .or(tok("<").to(Symbol::LAngle))
        .or(tok(">").to(Symbol::RAngle));

    let math = tok("+").to(Symbol::Plus)
        .or(tok("=").to(Symbol::Equal))
        .or(tok("*").to(Symbol::Star))
        .or(tok("/").to(Symbol::Slash));

    let binop = tok("->").to(Symbol::Arrow)
        .or(tok("<=").to(Symbol::LessThanEqual))
        .or(tok(">=").to(Symbol::GreaterThanEqual))
        .or(tok("==").to(Symbol::EqualEqual))
        .or(tok("!=").to(Symbol::NotEqual))
        .or(tok("&&").to(Symbol::And))
        .or(tok("||").to(Symbol::Or));

    let random = tok(";").to(Symbol::SemiColon)
        .or(tok(":").to(Symbol::Colon))
        .or(tok(",").to(Symbol::Comma))
        .or(tok(".").to(Symbol::Dot))
        .or(just("-").to(Symbol::Minus))
        .or(tok("#").to(Symbol::Hash))
        .or(tok("?").to(Symbol::QuestionMark));

    let symbol = binop
        .or(braces)
        .or(math)
        .or(random)
        .map(Token::Symbol);

    let literal = str_lit
        .or(cmd_lit)
        .or(bool_lit)
        .or(float_lit)
        .or(int_lit);

    let token = keyword
        .or(primitive)
        .or(literal)
        .or(cmd_lit)
        .or(ident)
        .or(symbol);

    token
        .repeated()
        .then_ignore(end())
}



