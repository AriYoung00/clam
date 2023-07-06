use chumsky::prelude::*;
use chumsky::text::Character;
use chumsky::text::keyword;
use clam_common::ast::*;
use clam_common::tokens::*;

use crate::lexer::literal;


fn r#type() -> impl Parser<char, Type, Error = Simple<char>> {
    let primitive = choice((
        keyword("bool").to(Primitive::Bool),
        keyword("int").to(Primitive::Int),
        keyword("float").to(Primitive::Float),
        keyword("string").to(Primitive::String),
    )).padded();

    // let primitive = just(Token::Primitive).map(Type::Primitive);
    // let name = just(Token::Identifier).map();

    primitive.map(Type::Primitive)
        .or(text::ident().map(|i: String| Type::Name(i.into())))
}

type Stupid = <char as Character>::Collection;

fn expr() -> impl Parser<char, Expr, Error = Simple<char>> {
    let binop_prec1 = choice((
        just("*").to(BinaryOperator::Mul),
        just("/").to(BinaryOperator::Div),
    ));

    let binop_prec2 = choice((
        just("+").to(BinaryOperator::Plus),
        just("-").to(BinaryOperator::Minus),
    ));

    let binop_prec3 = choice((
        just("<").to(BinaryOperator::Less),
        just("<=").to(BinaryOperator::LessEqual),
        just(">").to(BinaryOperator::Greater),
        just(">=").to(BinaryOperator::GreaterEqual),
    ));

    let binop_prec4 = choice((
        just("==").to(BinaryOperator::Equal),
        just("!=").to(BinaryOperator::NotEqual),
    ));

    let binop_prec5 = choice((
        just("&&").to(BinaryOperator::And),
        just("||").to(BinaryOperator::Or),
    ));

    let unop = choice((
        just("-").to(UnaryOperator::Negate),
        just("!").to(UnaryOperator::Not),
        just("&").to(UnaryOperator::Ref),
    ));

    recursive(|expr| {
        let statement = {
            let var_decl = just("let")
                .padded()
                .ignore_then(text::ident().map(|t: String| Identifier(t)))
                .then(just(":")
                        .ignore_then(text::ident())
                        .map(|t: String| Some(Type::Name(t.into())))
                        .or(empty().to(None))
                );


            let r#let = var_decl.then(
                just("=")
                    .padded()
                    .ignore_then(expr)
                    .map(|e| Some(e))
                .or(empty().to(None))
            )
                .map(|((id, r#type), expr)| Statement::Let(id.into(), r#type, expr));

            let assignment = text::ident()
                .then_ignore(just("=").padded())
                .then(expr)
                .map(|(id, expr): (String, Expr)| Statement::Assign(id.into(), expr));

            let statement = r#let
                .or(assignment)
                .or(expr.map(Statement::Expr))
                .padded()
                .then_ignore(just(";"));

            statement
        };

        let block = || statement.repeated()
            .delimited_by(just("{"), just("}"))
            .map(Block::new);

        let conditional = keyword("if").padded()
            .ignore_then(expr)
            .then(block())
            .map(|(cond, body)| Conditional{cond: Box::new(cond), body});

        let while_loop = keyword("while").padded()
            .ignore_then(expr)
            .then(block())
            .map(|(cond, body)| WhileLoop{cond: Box::new(cond), body});

        let for_loop = keyword("for").padded()
            .ignore_then(text::ident())
            .then_ignore(just("in"))
            .then(expr())
            .then(block())
            .map(|((var, iter), body)| ForLoop{var: var.into(), iter: Box::new(iter), body});


        // closure-hoisting is easier than making atom Clone
        let atom = || literal().map(|l| Expr::Literal(l))
            .or(expr.clone().delimited_by(just("("), just(")")))
            .padded();

        let un_operation = unop.repeated()
            .then(atom())
            .foldr(|op, rhs| Expr::UnOp(op, Box::new(rhs)));

        let literal = literal()
            .map(|lit| Expr::Literal(lit));

        let arg_list = expr.clone()
            .padded()
            .separated_by(just(","));

        let fn_call = text::ident()
            .then(
                arg_list.delimited_by(just("("), just(")")))
            .map(|(name, args)| Expr::FnCall(name.into(), args));


        let tmp1 = choice((
            expr.delimited_by(just("("), just(")")),
            fn_call,
            conditional().map(Expr::Conditional),
        ));

        let tmp2 = choice((
            while_loop.map(Expr::WhileLoop),
            for_loop.map(Expr::ForLoop),
            block.map(Expr::Block),
        ));

        let tmp3 = choice((
            literal,
            text::ident().map(|i: Stupid| Expr::Identifier(i.into())),
            un_operation,
        ));

        let expr_ = choice((
            tmp1, tmp2, tmp3
        ));

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

        let bin_operation3 = bin_operation2.clone()
            .then(binop_prec3
                .then(bin_operation2)
                .repeated())
            .foldl(|lhs, (op, rhs)| Expr::BinOp(op, Box::new(lhs), Box::new(rhs)));

        let bin_operation4 = bin_operation3.clone()
            .then(binop_prec4
                .then(bin_operation3)
                .repeated())
            .foldl(|lhs, (op, rhs)| Expr::BinOp(op, Box::new(lhs), Box::new(rhs)));

        let bin_operation5 = bin_operation4.clone()
            .then(binop_prec5
                .then(bin_operation4)
                .repeated())
            .foldl(|lhs, (op, rhs)| Expr::BinOp(op, Box::new(lhs), Box::new(rhs)));

        bin_operation5
    })
}

fn fn_def() -> impl Parser<char, FnDef, Error = Simple<char>> {
    let formal_params = text::ident()
        .then(
            just(":").ignore_then(r#type().or_not())
        )
        .separated_by(just(",").padded())
        .padded()
        .map(|stuff: Vec<(String, Option<Type>)>| {
            stuff.into_iter()
                .map(|(name, r#type)| (name.into(), r#type))
                .collect::<Vec<(Identifier, Option<Type>)>>()
        });

    keyword("fn").padded()
        .ignore_then(text::ident())
        .then_ignore(just("("))
        .then(formal_params)
        .then_ignore(just(")"))
        .then(r#type().or_not())
        .then_ignore(just("=>").padded())
        .then(expr())
        .map(|(((name, params), ret_type), body)| {
            FnDef{
                name,
                param_list: params,
                ret_type,
                body,
            }
        })
}

pub fn parser(input: &str) -> Result<Vec<FnDef>, Vec<Simple<char>>> {
    fn_def()
        .repeated()
        .parse(input)
}
