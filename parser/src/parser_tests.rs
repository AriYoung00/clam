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

    fn pl_stmt(s: &str) -> Result<Statement, ParseError<usize, Token, LexicalError>> {
        let lexer = Lexer::new(s);
        let parser_ = parser::StatementParser::new();
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

    #[test]
    fn test_block() {
        use Expr::{Block, BinOp, Literal};
        use self::Literal::Int;
        use BinaryOperator::{Plus, Times};

        let expr = plex("
{
    a = b;
    b = 1 * 1 + 3 * 4;
}     
        ").unwrap();
        let res = b(
            Block(vec![
                Statement::Assign(
                    Identifier("a".into()),
                    b(Expr::Identifier("b".into()))
                ),
                Statement::Assign(
                    Identifier("b".into()),
                    b(BinOp(
                        Plus, 
                        b(BinOp(Times, b(Literal(Int(1))), b(Literal(Int(1))))),
                        b(BinOp(Times, b(Literal(Int(3))), b(Literal(Int(4)))))
                    ))
                ),
            ].into()
        ));

        assert_eq!(expr, res);
    }

    #[test]
    fn test_simple_conditional() {
        use Expr::{Conditional, Literal};
        use self::Literal::Bool;

        let expr = plex("if true { a = b }").unwrap();
        assert_eq!(expr, b(
            Conditional(self::Conditional::new(
                b(Literal(Bool(true))),
                Block::new(vec![
                    Statement::Assign(
                        Identifier("a".into()), 
                        b(Expr::Identifier("b".into()))
                    )
                ])
            )
        )))
    }

    #[test]
    fn test_simple_while_loop() {
        use Expr::WhileLoop;
        use self::Literal::Bool;

        let expr = plex("while false { a = b }").unwrap();
        assert_eq!(expr, b(
            WhileLoop(self::WhileLoop::new(
                b(Expr::Literal(Bool(false))),
                Block::new(vec![
                    Statement::Assign(
                        Identifier("a".into()), 
                        b(Expr::Identifier("b".into()))
                    )
                ])
            )
        )))
    }

    #[test]
    fn test_simple_fn_call() {
        use Expr::FnCall;

        let expr = plex("f(a, b)").unwrap();
        assert_eq!(expr, b(FnCall("f".into(), vec![Expr::Identifier("a".into()), Expr::Identifier("b".into())])));

        let expr = plex("f(-1, b)").unwrap();
        assert_eq!(expr, b(FnCall(
            "f".into(), 
            vec![
                Expr::UnOp(UnaryOperator::Negate, Box::new(Expr::Literal(Literal::Int(1)))), 
                Expr::Identifier("b".into())
            ]
        )));
    }

    #[test]
    fn test_simple_fn_def() {

        let stmt = pl_stmt("
fn thing(a, b: int)
    a
        ").unwrap();
        assert_eq!(stmt, Statement::FnDef(FnDef{
            name: Identifier("thing".into()),
            param_list: vec![
                (Identifier("a".into()), None), 
                (Identifier("b".into()), Some(Type::Primitive(Primitive::Int)))
            ],
            ret_type: None,
            body: b(Expr::Identifier("a".into()))
        }));

        let stmt = pl_stmt("
fn thing(a, b: int) -> bool
    a == b
        ").unwrap();
        assert_eq!(stmt, Statement::FnDef(FnDef{
            name: Identifier("thing".into()),
            param_list: vec![
                (Identifier("a".into()), None), 
                (Identifier("b".into()), Some(Type::Primitive(Primitive::Int)))
            ],
            ret_type: Some(Type::Primitive(Primitive::Bool)),
            body: b(Expr::BinOp(
                BinaryOperator::Eq,
                b(Expr::Identifier("a".into())),
                b(Expr::Identifier("b".into()))
            ))
        }));

        let stmt = pl_stmt("
fn thing(a, b: int) -> bool {
    a = b;
    a = 1 + 1
}
        ").unwrap();
        assert_eq!(stmt, Statement::FnDef(FnDef{
            name: Identifier("thing".into()),
            param_list: vec![
                (Identifier("a".into()), None), 
                (Identifier("b".into()), Some(Type::Primitive(Primitive::Int)))
            ],
            ret_type: Some(Type::Primitive(Primitive::Bool)),
            body: b(Expr::Block(Block::new(vec![
                Statement::Assign(Identifier("a".into()), b(Expr::Identifier("b".into()))),
                Statement::Assign(
                    Identifier("a".into()), 
                    b(Expr::BinOp(
                        BinaryOperator::Plus,
                        b(Expr::Literal(Literal::Int(1))),
                        b(Expr::Literal(Literal::Int(1)))
                    ))
                )
            ])))
        }));
    }

    #[test]
    fn test_simple_assignment() {
        use Statement::Assign;

        let stmt = pl_stmt("thing = true").unwrap();
        assert_eq!(stmt, Assign(
            "thing".into(), 
            b(Expr::Literal(Literal::Bool(true)))
        ));
    }

    #[test]
    fn test_simple_let() {
        use Statement::Let;

        let stmt = pl_stmt("let thing").unwrap();
        assert_eq!(stmt, Let("thing".into(), None, None));

        let stmt = pl_stmt("let thing = true").unwrap();
        assert_eq!(stmt, Let("thing".into(), None, Some(b(Expr::Literal(Literal::Bool(true))))));
    }
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
}

*/
