mod lalrpop {
    use crate::lexer::*;
    use crate::tokens::*;
    use clam_common::ast::*;
    use lalrpop_util::{lalrpop_mod, ParseError};
    use ordered_float::OrderedFloat;
    lalrpop_mod!(pub parser);

    use crate::lexer::Lexer;

    #[inline]
    fn b<T>(thing: T) -> Box<T> {
        Box::new(thing)
    }

    fn plex(s: &str) -> Result<Box<Expr>, ParseError<usize, Token, LexicalError>> {
        let lexer = Lexer::new(s);
        let parser_ = parser::ExprParser::new();
        parser_.parse(lexer)
    }

    #[test]
    fn test_literal() {
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
    fn test_simple_binop() {
        use Expr::BinOp;
        use BinaryOperator::{Plus, Times};
        use Expr::Literal;
        use self::Literal::Int;

        let expr = plex("5 + 5").unwrap();
        assert_eq!(expr, Box::new(
            BinOp(Plus, b(Literal(Int(5))), b(Literal(Int(5))))
        ));

        let expr = plex("5 + 5 * 3").unwrap();
        assert_eq!(expr, b(
            BinOp(
                Plus, 
                b(Literal(Int(5))), 
                b(BinOp(Times, b(Literal(Int(5))), b(Literal(Int(3)))))
            )
        ));
    }

    #[test]
    fn test_binop_with_unop() {
        use Expr::{UnOp, BinOp, Literal};
        use self::Literal::Int;
        use BinaryOperator::Plus;
        use UnaryOperator::Negate;

        let expr = plex("5 + -1").unwrap();
        assert_eq!(expr, b(
            BinOp(Plus, b(Literal(Int(5))), b(UnOp(Negate, b(Literal(Int(1))))))
        ));
    }

    #[test]
    fn test_binop_with_parens() {
        use Expr::{BinOp, Literal};
        use self::Literal::Int;
        use BinaryOperator::{Plus, Times};
        
        let expr = plex("(1 + 1) * 3").unwrap();
        assert_eq!(expr, b(
            BinOp(
                Times, 
                b(BinOp(Plus, b(Literal(Int(1))), b(Literal(Int(1))))), 
                b(Literal(Int(3)))
            )
        ))
    }

    #[test]
    fn test_identifier() {
        use Expr::Identifier;

        let expr = plex("qwertyuiop").unwrap();
        assert_eq!(expr, b(Identifier("qwertyuiop".into())));
    }

//    #[test]
//    fn test_block() {
//        use Expr::{Block, BinOp, Literal};
//        use self::Literal::Int;
//        use BinaryOperator::{Plus, Times};
//
//        let expr = plex("
//{
//    a = b;
//    b = 1 * 1 + 3 * 4;
//}     
//        ").unwrap();
//        let res = b(
//            Block(vec![
//                Statement::Assign(
//                    Identifier("a".into()),
//                    Expr::Identifier("b".into())
//                ),
//                Statement::Assign(
//                    Identifier("b".into()),
//                    BinOp(
//                        Plus, 
//                        b(BinOp(Times, b(Literal(Int(1))), b(Literal(Int(1))))),
//                        b(BinOp(Times, b(Literal(Int(3))), b(Literal(Int(4)))))
//                    )
//                ),
//            ])
//        );
//
//        assert_eq!(expr, res);
//    }
//
//    #[test]
//    fn test_simple_conditional() {
//        use Expr::{Conditional, Literal};
//        use self::Literal::Bool;
//
//        let expr = plex("if true { a = b }").unwrap();
//        assert_eq!(expr, b(
//            Conditional(
//                b(Literal(Bool(true))),
//                Block::new(vec![
//                    Statement::Assign(
//                        Identifier("a".into()), 
//                        Expr::Identifier("b".into())
//                    )
//                ])
//            )
//        ))
//    }
//
//    #[test]
//    fn test_simple_while_loop() {
//        use Expr::WhileLoop;
//        use self::Literal::Bool;
//
//        let expr = plex("while false { a = b }").unwrap();
//        assert_eq!(expr, b(
//            WhileLoop(
//                b(Literal(Bool(false))),
//                Block::new(vec![
//                    Statement::Assign(
//                        Identifier("a".into()), 
//                        Expr::Identifier("b".into())
//                    )
//                ])
//            )
//        ))
//    }
}

/*
mod expr {
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
    fn test_fn_call() {
        use clam_common::tokens::UnaryOperator::Negate;
        use clam_common::tokens::Literal::Int;

        let res = parse("f(a, b);");
        assert_eq!(res, block(vec![Expr::FnCall("f".into(), vec![Expr::Identifier("a".into()), Expr::Identifier("b".into())])]));

        let res = parse("f(-1, b);");
        assert_eq!(res, block(vec![Expr::FnCall("f".into(), 
                    vec![Expr::UnOp(Negate, Box::new(Expr::Literal(Int(1)))), Expr::Identifier("b".into())])]));

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
*/
