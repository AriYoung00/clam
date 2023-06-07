use chumsky::prelude::*;
use chumsky::Parser;

enum Keyword {
    Bool,
    Int,
    Float,
    String,
    Fn,
    If,
    Else,
    For,
    While,
    Break,
    Continue,
    In,
    Let,
    True,
    False
}

enum Literal {
    Str(String),
    Int(i64),
    Float(f64),
    Command(String),
}

enum BinaryOperator {
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

enum UnaryOperator {
    Negate,
    Not,
    Ref,
}

enum Symbol {
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    SemiColon,
    Equal,
    Comma,
    Arrow,
    Dot,
    QuestionMark,
}

enum Token {
    Keyword(Keyword),
    Literal(Literal),
    Identifier(String),
    BinOp(BinaryOperator),
    UnOp(UnaryOperator),
    Symbol(Symbol),
}

fn lexer() -> impl Parser<char, Token, Error = Simple<char>> {
    recursive(|token| {
        let keyword = text::keyword("bool")
            .or(text::keyword("int"))
            .or(text::keyword("float"))
            .or(text::keyword("string"))
            .or(text::keyword("fn"))
            .or(text::keyword("if"))
            .or(text::keyword("else"))
            .or(text::keyword("for"))
            .or(text::keyword("while"))
            .or(text::keyword("break"))
            .or(text::keyword("continue"))
            .or(text::keyword("in"))
            .or(text::keyword("let"))
            .or(text::keyword("true"))
            .or(text::keyword("false"));

        let _str_interior = just('"')
            .not()
            .repeated();

        let str_lit = just('"')
            .ignore_then(_str_interior)
            .then_ignore(just('"'))
            .collect()
            .map(Literal::Str)
            .map(Token::Literal);

        let int_lit = text::int(10)
            .map(|s: String| Literal::Int(s.parse().unwrap()))
            .map(Token::Literal);

        // who needs floating point anyways...
        // let float_lit = text::int(10)
        //     .then(just("."))
        //     .then(text::int(10))
        //     .map(|s| Literal::Float(s.parse().unwrap()));

        
        keyword
    })
}
