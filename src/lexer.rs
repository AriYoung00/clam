use chumsky::prelude::*;
use chumsky::Parser;

#[derive(Clone, Debug, PartialEq)]
#[allow(dead_code)]
pub enum Keyword {
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

#[derive(Clone, Debug, PartialEq)]
#[allow(dead_code)]
pub enum Literal {
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
    SemiColon,
    Equal,
    Comma,
    Arrow,
    Dot,
    Star,
    Slash,
    Plus,
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
#[allow(dead_code)]
pub enum Token {
    Keyword(Keyword),
    Literal(Literal),
    Identifier(String),
    BinOp(BinaryOperator),
    UnOp(UnaryOperator),
    Symbol(Symbol),
}

fn lexer() -> impl Parser<char, Token, Error = Simple<char>> {
    let tok = |c| just(c).padded();

    let keyword = text::keyword::<_, &str, _>("bool").to(Keyword::Bool)
        .or(text::keyword("int")     .to(Keyword::Int))
        .or(text::keyword("float")   .to(Keyword::Float))
        .or(text::keyword("string")  .to(Keyword::String))
        .or(text::keyword("fn")      .to(Keyword::Fn))
        .or(text::keyword("if")      .to(Keyword::If))
        .or(text::keyword("else")    .to(Keyword::Else))
        .or(text::keyword("for")     .to(Keyword::For))
        .or(text::keyword("while")   .to(Keyword::While))
        .or(text::keyword("break")   .to(Keyword::Break))
        .or(text::keyword("continue").to(Keyword::Continue))
        .or(text::keyword("in")      .to(Keyword::In))
        .or(text::keyword("let")     .to(Keyword::Let))
        .or(text::keyword("true")    .to(Keyword::True))
        .or(text::keyword("false")   .to(Keyword::False))
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

    let symbol = just("+").to(Symbol::Plus).padded()
        .or(just("-").to(Symbol::LParen))
        .or(tok("*").to(Symbol::Star))
        .or(tok("/").to(Symbol::Slash))
        .or(tok("<").to(Symbol::RAngle))
        .or(tok("<=").to(Symbol::LessThanEqual))
        .or(tok(">").to(Symbol::RAngle))
        .or(tok(">=").to(Symbol::GreaterThanEqual))
        .or(tok("==").to(Symbol::EqualEqual))
        .or(tok("!=").to(Symbol::NotEqual))
        .or(tok("&&").to(Symbol::And))
        .or(tok("||").to(Symbol::Or))
        .padded()
        .map(Token::Symbol);

    let token = keyword
        .or(str_lit)
        .or(int_lit)
        .or(float_lit)
        .or(cmd_lit)
        .or(ident)
        .or(symbol);

    token
        //.repeated()
        .then_ignore(end())
}

#[cfg(test)]
mod test {
    use chumsky::prelude::*;
    use super::*;

    #[test]
    fn test_single_keyword() {
        assert_eq!(lexer().parse("bool").unwrap(), Token::Keyword(Keyword::Bool));
    }
}
