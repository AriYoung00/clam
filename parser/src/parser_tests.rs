// fn parse(s: &str) -> clam_common::ast::Block {
//     chumsky::Parser::parse(&crate::parser::block(), s).unwrap()
// }

// fn parse_top(s: &str) -> Vec<clam_common::ast::FnDef> {
//     crate::parser::parser(s).unwrap()
// }

mod lalrpop {
    use clam_common::ast::*;
    use lalrpop_util::lalrpop_mod;
    use logos::Logos;
    use ordered_float::OrderedFloat;
    lalrpop_mod!(pub parser);

    use crate::lexer::Lexer;

    #[test]
    fn test_literal() {
        let plex = |thing| {
            let lexer = Lexer::new(thing);
            let parser_ = parser::ExprParser::new();
            parser_.parse(lexer)
        };

        let expr = plex("5").unwrap();
        assert_eq!(expr, Box::new(Expr::Literal(Literal::Int(5))));
        let expr = plex("true").unwrap();
        assert_eq!(expr, Box::new(Expr::Literal(Literal::Bool(true))));
        let expr = plex("3.14").unwrap();
        assert_eq!(expr, Box::new(Expr::Literal(Literal::Float(OrderedFloat(3.14)))));
        let expr = plex(r#""hello there""#).unwrap();
        assert_eq!(expr, Box::new(Expr::Literal(Literal::String("hello there".into()))));
        let expr = plex("`hello there`").unwrap();
        assert_eq!(expr, Box::new(Expr::Literal(Literal::Command("hello there".into()))));
    }

    #[test]
    fn test_binop() {
        use Expr::BinOp;
        use BinaryOperator::Plus;
        use Expr::Literal;
        use self::Literal::Int;
        let plex = |thing| {
            let lexer = Lexer::new(thing);
            let parser_ = parser::ExprParser::new();
            parser_.parse(lexer)
        };

        let b = |a| Box::new(a);
        let expr = plex("5 + 5").unwrap();
        assert_eq!(expr, Box::new(
            BinOp(Plus, b(Literal(Int(5))), b(Literal(Int(5))))
        ));

    }
}

/*
mod expr {
    // use clam_common::ast::{Block, FnDef};
    use clam_common::{ast, tokens::Literal};


    use clam_common::{ast::*, tokens::{BinaryOperator, Identifier}};
    use maplit::hashmap;
    use super::{parse, parse_top};


    fn block(exprs: Vec<Expr>) -> ast::Block {
        Block::new(exprs.into_iter().map(Statement::Expr as fn(_) -> _).collect())
    }

    #[test]
    fn parse_top_fn_expr() {
        use clam_common::tokens::Primitive::*;
        let res = parse_top("
fn test_thing(a: int, b: float) -> int => a == b
        ");

        let ty = clam_common::ast::Type::Primitive as fn (_) -> Type;
        let id = |s: &str| Identifier(s.into());

        let expected = vec![
FnDef{ 
    name: "test_thing".into(),
    param_list: vec![(id("a"), Some(ty(Int))), (id("b"), Some(ty(Float)))],
    ret_type: Some(ty(Int)),
    body: Expr::BinOp(BinaryOperator::Equal,
        Box::new(Expr::Identifier("a".into())),
        Box::new(Expr::Identifier("b".into())))
}];

        assert_eq!(res, expected);
    }

    #[test]
    fn test_literal() {
        use Expr::Literal;
        use clam_common::tokens::Literal::*;

        // bool literal
        assert_eq!(parse("true;"), Block::new(vec![Statement::Expr(Expr::Literal(Bool(true)))]));
        assert_eq!(parse("false;"), Block::new(vec![Statement::Expr(Expr::Literal(Bool(false)))]));

        // int literal
        assert_eq!(parse("11300;"), Block::new(vec![Statement::Expr(Expr::Literal(Int(11300)))]));

        // float literal
        assert_eq!(parse("11300.15;"), block(vec![Literal(Float(11300.15.into()))]));

        // string literal
        assert_eq!(parse(r#""hello there";"#), block(vec![Literal(Str("hello there".into()))]));

        // command literal
        assert_eq!(parse(r#"```ls```;"#), block(vec![Literal(Command("ls".into()))]));
        assert_eq!(parse(r#"```
                ls | grep "hello"
                mkdir test
                cd test
                touch thing.txt```;"#),
                block(vec![Literal(Command(r#"
                ls | grep "hello"
                mkdir test
                cd test
                touch thing.txt"#.into()))]));
    }

    #[test]
    fn test_identifier() {
        assert_eq!(parse("qwertyuiop;"), block(vec![Expr::Identifier("qwertyuiop".into())]));
    }

    #[test]
    fn test_fn_call() {
        use clam_common::tokens::UnaryOperator::Negate;
        use clam_common::tokens::Literal::Int;

        let res = parse("f(a, b);");
        assert_eq!(res, block(vec![Expr::FnCall("f".into(), vec![Expr::Identifier("a".into()), Expr::Identifier("b".into())])]));

        let res = parse("f(-1, b);");
        assert_eq!(res, block(vec![Expr::FnCall("f".into(), 
                    vec![Expr::UnOp(Negate, Box::new(Expr::Literal(Int(1)))), Expr::Identifier("b".into())])]));

    }

    #[test]
    fn test_precedence() {
        use clam_common::tokens::Literal::*;

        assert_eq!(parse("1 + 1 * 1;"), Block::new(vec![Statement::Expr(
            Expr::BinOp(
                BinaryOperator::Plus, 
                Box::new(Expr::Literal(Int(1))),
                Box::new(Expr::BinOp(
                    BinaryOperator::Mul,
                    Box::new(Expr::Literal(Int(1))),
                    Box::new(Expr::Literal(Int(1))),
                )),
            )
        )]));
        assert_eq!(parse("1 * 1 + 1;"), Block::new(vec![Statement::Expr(
            Expr::BinOp(
                BinaryOperator::Plus, 
                Box::new(Expr::BinOp(
                    BinaryOperator::Mul,
                    Box::new(Expr::Literal(Int(1))),
                    Box::new(Expr::Literal(Int(1))),
                )),
                Box::new(Expr::Literal(Int(1))),
            )
        )]));
        assert_eq!(parse("1 * (1 + 1);"), Block::new(vec![Statement::Expr(
            Expr::BinOp(
                BinaryOperator::Mul, 
                Box::new(Expr::Literal(Int(1))),
                Box::new(Expr::BinOp(
                    BinaryOperator::Plus,
                    Box::new(Expr::Literal(Int(1))),
                    Box::new(Expr::Literal(Int(1))),
                )),
            )
        )]));
        assert_eq!(parse("1 * 1 + 1 * 1;"), Block::new(vec![Statement::Expr(
            Expr::BinOp(
                BinaryOperator::Plus, 
                Box::new(Expr::BinOp(
                    BinaryOperator::Mul,
                    Box::new(Expr::Literal(Int(1))),
                    Box::new(Expr::Literal(Int(1))),
                )),
                Box::new(Expr::BinOp(
                    BinaryOperator::Mul,
                    Box::new(Expr::Literal(Int(1))),
                    Box::new(Expr::Literal(Int(1))),
                )),
            )
        )]));
    }
}

mod r#let {
    use clam_common::ast::*;
    use super::parse;

    #[test]
    fn test_simple_let() {
        let res = parse("let thing;");
        assert_eq!(res, Block::new(vec![Statement::Let("thing".into(), None, None)]));

        let res = parse("let thing = true;");
        assert_eq!(res, Block::new(vec![Statement::Let("thing".into(), None, Some(Expr::Literal(clam_common::tokens::Literal::Bool(true))))]));
    }
}

mod assignment {
    use clam_common::{ast::*, tokens::Literal};
    use super::parse;

    #[test]
    fn test_simple_assignment() {
        let res = parse("thing = true;");
        assert_eq!(res, Block::new(vec![Statement::Assign("thing".into(), Expr::Literal(Literal::Bool(true)))]));
    }
}

mod conditional {
    // use clam_common::{ast::* tokens::Literal};
    // use super::parse;

    // #[test]
    // fn test_simple_conditional() {
    //     let res = parse("if true then a = b;");
    //     assert_eq!(res, Block::new(vec![
    //         Statement::Expr(Expr::Conditional(
    //             Box::new(Expr::Literal(Boolean(true))),
    //             Block{ statements: vec![
    //                 Statement::Assign(Identifier("a"), Expr::Identifier("b"))
    //             ]}
    //         )
    //     ]))
    // }
}
*/