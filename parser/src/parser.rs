use chumsky::prelude::*;
use chumsky::text::Character;
use clam_common::ast::*;
use clam_common::tokens::*;

use crate::lexer::literal;


// fn r#type() -> impl Parser<Token, Type, Error = Simple<char>> {
//     let primitive = just(Token::Primitive).map(Type::Primitive);
//     let name = just(Token::Identifier).map()
// }

type Stupid = <char as Character>::Collection;

// fn make_precedence_chain(chain: Vec<)

pub fn parser() -> impl Parser<char, Block, Error = Simple<char>> {
    let binop_prec1 = choice((
        just("*").to(BinaryOperator::Mul),
        just("/").to(BinaryOperator::Div),
    ));

    let binop_prec2 = choice((
        just("+").to(BinaryOperator::Plus),
        just("-").to(BinaryOperator::Minus),
    ));

//    let binop_prec3 = choice((
//        just("<").to(BinaryOperator::Less),
//        just("<=").to(BinaryOperator::LessEqual),
//        just(">").to(BinaryOperator::Greater),
//        just(">=").to(BinaryOperator::GreaterEqual),
//    ));
//
//    let binop_prec4 = choice((
//        just("==").to(BinaryOperator::Equal),
//        just("!=").to(BinaryOperator::NotEqual),
//    ));
//
//    let binop_prec5 = choice((
//        just("&&").to(BinaryOperator::And),
//        just("||").to(BinaryOperator::Or),
//    ));

    let unop = choice((
        just("-").to(UnaryOperator::Negate),
        just("!").to(UnaryOperator::Not),
        just("&").to(UnaryOperator::Ref),
    ));

//    let binop_ = recursive(|binop| {
//        binop_prec1.then(binop)
//            .or(binop_prec2.then(binop))
//            .or(binop_prec3.then(binop))
//            .or(binop_prec4.then(binop))
//            .or(binop_prec5.then(binop))
//    });

    let expr = recursive::<'_, char, Expr, _, _, _>(|expr| {
        let atom = || literal().map(|l| Expr::Literal(l))
                        .or(expr.clone().delimited_by(just("("), just(")")))
                        .padded();

        let un_operation = unop
            .then(atom())
            .map(|(op, expr)| Expr::UnOp(op, Box::new(expr)));

        let literal = literal()
            .map(|lit| Expr::Literal(lit));

        let arg_list = expr.clone()
            .padded()
            .separated_by(just(","));

        let fn_call = text::ident()
            .then(
                arg_list
                    .delimited_by(just("("), just(")"))
            )
            .map(|(name, args)| Expr::FnCall(name, args));

        let expr_ = expr.delimited_by(just("("), just(")"))
            .or(un_operation)
            .or(literal)
            .or(text::ident().map(|i: Stupid| Expr::Identifier(i.into())))
            .or(fn_call);

        let bin_operation1 = recursive(|b|
            expr_
                .then(binop_prec1
                    .then(b)
                    .repeated())
                .foldl(|lhs, (op, rhs)| Expr::BinOp(op, Box::new(lhs), Box::new(rhs)))
        );

        let bin_operation2 = bin_operation1.clone()
            .then(binop_prec2
                .then(bin_operation1)
                .repeated())
            .foldl(|lhs, (op, rhs)| Expr::BinOp(op, Box::new(lhs), Box::new(rhs)));

        bin_operation2
    });

    let var_decl = just("let")
        .padded()
        .ignore_then(text::ident())
        .then(just(":")
                .ignore_then(text::ident())
                .map(|t: String| Some(Type::Name(t.into())))
                .or(empty().to(None))
        );

    let r#let = var_decl.then(
        just("=")
            .ignore_then(expr.clone())
            .map(|e| Some(e))
        .or(empty().to(None))
    )
        .map(|((id, r#type), expr)| Statement::Let(id.into(), r#type, expr));

    let assignment = text::ident()
        .then_ignore(just("="))
        .then(expr.clone())
        .map(|(id, expr)| Statement::Assign(id.into(), expr));

    let statement = expr.map(Statement::Expr)
        .or(r#let)
        .or(assignment)
        .padded()
        .then_ignore(just(";"));

    statement.repeated()
        .then_ignore(end())
        .map(Block::new)
}
