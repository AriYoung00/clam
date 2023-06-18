extern crate chumsky;
use chumsky::prelude::*;
use chumsky::Parser;

use chumsky::text::Character;
use clam_common::tokens::*;

fn symbol() -> impl Parser<char, Token, Error = Simple<char>> {
    let sym = |c| just(c).padded();

    let braces = choice::<_, Simple<char>>((
        sym(")").to(Symbol::RParen),
        sym("{").to(Symbol::LBrace),
        sym("}").to(Symbol::RBrace),
        sym("(").to(Symbol::LParen),
        sym(")").to(Symbol::RParen),
        sym("[").to(Symbol::LBracket),
        sym("]").to(Symbol::RBracket),
        sym("<").to(Symbol::LAngle),
        sym(">").to(Symbol::RAngle),
    ));

    let math = choice::<_, Simple<char>>((
        sym("+").to(Symbol::Plus),
        sym("=").to(Symbol::Equal),
        sym("*").to(Symbol::Star),
        sym("/").to(Symbol::Slash),
    ));

    let binop = choice::<_, Simple<char>>((
        sym("->").to(Symbol::Arrow),
        sym("<=").to(Symbol::LessThanEqual),
        sym(">=").to(Symbol::GreaterThanEqual),
        sym("==").to(Symbol::EqualEqual),
        sym("!=").to(Symbol::NotEqual),
        sym("&&").to(Symbol::And),
        sym("||").to(Symbol::Or),
    ));

    let unop = choice::<_, Simple<char>>((
        sym("!").to(Symbol::ExclamationMark),
        sym("-").to(Symbol::Minus),
        sym("&").to(Symbol::Ampersand),
    ));

    let random = choice::<_, Simple<char>>((
        sym(";").to(Symbol::SemiColon),
        sym(":").to(Symbol::Colon),
        sym(",").to(Symbol::Comma),
        sym(".").to(Symbol::Dot),
        sym("#").to(Symbol::Hash),
        sym("?").to(Symbol::QuestionMark),
    ));

    let symbol = binop
        .or(unop)
        .or(braces)
        .or(math)
        .or(random)
        .map(Token::Symbol);

    symbol
}

fn literal() -> impl Parser<char, Token, Error = Simple<char>> {
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

    let literal = str_lit
        .or(cmd_lit)
        .or(bool_lit)
        .or(float_lit)
        .or(int_lit);

    literal
}

pub fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    let primitive = choice((
        text::keyword("bool")  .to(Primitive::Bool),
        text::keyword("int")   .to(Primitive::Int),
        text::keyword("float") .to(Primitive::Float),
        text::keyword("string").to(Primitive::String),
    ))
        .padded()
        .map(Token::Primitive);

    let keyword = choice((
        text::keyword("fn")      .to(Keyword::Fn),
        text::keyword("if")      .to(Keyword::If),
        text::keyword("else")    .to(Keyword::Else),
        text::keyword("for")     .to(Keyword::For),
        text::keyword("while")   .to(Keyword::While),
        text::keyword("break")   .to(Keyword::Break),
        text::keyword("continue").to(Keyword::Continue),
        text::keyword("in")      .to(Keyword::In),
        text::keyword("let")     .to(Keyword::Let),
        text::keyword("var")     .to(Keyword::Var),
    ))
        .padded()
        .map(Token::Keyword);

    // For now just let chumsky do identifiers
    let ident = text::ident()
        .padded()
        .map(|i: <char as Character>::Collection| Token::Identifier(i.into()));

    let token = keyword
        .or(primitive)
        .or(literal())
        .or(ident)
        .or(symbol());

    token
        .repeated()
        .then_ignore(end())
}
