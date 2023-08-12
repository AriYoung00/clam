mod lalrpop {
    use crate::lexer::*;
    use crate::tokens::*;
    use clam_common::ast::*;
    use lalrpop_util::{lalrpop_mod, ParseError};
    use ordered_float::OrderedFloat;
    lalrpop_mod!(pub parser);

    use crate::lexer::Lexer;

    trait DerefInner {
        type Target: Sized;
        fn deref_inner(self) -> Self::Target;
    }

    impl<T> DerefInner for Span<Box<T>> {
        type Target = Span<T>;

        fn deref_inner(self) -> Self::Target {
            let (l, t, r) = self;
            (l, *t, r)
        }
    }

    trait NullSpan: Sized {
        fn null_span(self) -> Span<Self>;
    }

    impl<T> NullSpan for T where T: Sized {
        fn null_span(self) -> Span<Self> {
            (0, self, 0)
        }
    }

    #[inline]
    fn b<T>(thing: T) -> Span<Box<T>> {
        (0, Box::new(thing), 0)
    }

    fn strip_loc<T>((_, t, _): Span<T>) -> Span<T> {
        (0, t, 0)
    }

    fn strip_option_loc<T>(o: Option<Span<T>>) -> Option<Span<T>> {
        o.map(strip_loc)
    }

    fn strip_stmt_loc((_, s, _): Span<Statement>) -> Span<Statement> {
        let strip_block_loc = |Block { body, last } | Block::new(
            body.into_iter().map(strip_stmt_loc).collect(),
            last.map(strip_stmt_loc)
        );

        let strip_fndef_loc = |FnDef { name, param_list, ret_type, body }| {
            let param_list = param_list.into_iter().map(|(id, t)|
                (strip_loc(id), strip_option_loc(t)))
                .collect();
            FnDef{ name: strip_loc(name), param_list, ret_type: strip_option_loc(ret_type), body: strip_expr_loc(body) }
        };

        let strip_let_loc = |Let { id, r#type, expr}| {
            Let {
                id: strip_loc(id),
                r#type: strip_option_loc(r#type),
                expr: expr.map(strip_expr_loc)
            }
        };

        let strip_assign_loc = |Assign { id, expr }| {
            Assign {
                id: strip_loc(id),
                expr: strip_expr_loc(expr)
            }
        };

        match s {
            Statement::FnDef(f) => Statement::FnDef(strip_fndef_loc(f)),
            Statement::Let(l) => Statement::Let(strip_let_loc(l)),
            Statement::Assign(a) => Statement::Assign(strip_assign_loc(a)),
            Statement::Expr(e) => Statement::Expr(strip_expr_loc(e)),
            rest@_ => rest
        }.null_span()
    }

    fn strip_expr_loc(e: Span<Box<Expr>>) -> Span<Box<Expr>> {
        let (_, exp, _) = e;

        let strip_params_loc = |params: Vec<Param>| {
            params.into_iter().map(|((_, id, _), t)| {
                ((0, id, 0), t.map(|(_, t, _)| (0, t, 0)))
            }).collect()
        };

        let strip_binop_loc = |BinOp { op, lhs, rhs }| {
            BinOp {
                op,
                lhs: strip_expr_loc(lhs),
                rhs: strip_expr_loc(rhs),
            }
        };

        let strip_unop_loc = |UnOp { op, rhs }| {
            UnOp {
                op,
                rhs: strip_expr_loc(rhs)
            }
        };

        let strip_fncall_loc = |FnCall { id, args }| {
            FnCall {
                id: (0, id.1, 0),
                args: args.into_iter().map(strip_expr_loc).collect()
            }
        };

        let strip_lambdadef_loc = |LambdaDef { params, body }| {
            LambdaDef {
                params: strip_params_loc(params),
                body: strip_expr_loc(body)
            }
        };

        let strip_block_loc = |Block { body, last } | Block::new(
            body.into_iter().map(strip_stmt_loc).collect(),
            last.map(strip_stmt_loc)
        );

        let stripped = Box::new(match *exp {
            Expr::BinOp(b) => Expr::BinOp(strip_binop_loc(b)),
            Expr::UnOp(u) => Expr::UnOp(strip_unop_loc(u)),
            Expr::FnCall(f) => Expr::FnCall(strip_fncall_loc(f)),
            Expr::LambdaDef(l) => Expr::LambdaDef(strip_lambdadef_loc(l)),
            Expr::Block(b) => Expr::Block(strip_block_loc(b)),
            Expr::Conditional(Conditional { cond, body }) => Expr::Conditional(Conditional::new(strip_expr_loc(cond), strip_block_loc(body))),
            Expr::WhileLoop(WhileLoop { cond, body }) => Expr::WhileLoop(WhileLoop::new(strip_expr_loc(cond), strip_block_loc(body))),
            Expr::ForLoop(ForLoop { var, iter, body }) => Expr::ForLoop(ForLoop { var: strip_loc(var), iter: strip_expr_loc(iter), body: strip_block_loc(body) }),
            rest@_ => rest,
        });

        (0, stripped, 0)
    }

    fn plex(s: &str) -> Result<Span<Box<Expr>>, ParseError<usize, Token, LexicalError>> {
        let lexer = Lexer::new(s);
        let parser_ = parser::ExprParser::new();
        let res = parser_.parse(lexer)?;
        Ok(strip_expr_loc((0, res, 0)))
    }

    fn pl_stmt(s: &str) -> Result<Statement, ParseError<usize, Token, LexicalError>> {
        let lexer = Lexer::new(s);
        let parser_ = parser::StatementParser::new();
        let res = parser_.parse(lexer)?;
        let (_, res, _) = strip_stmt_loc((0, res, 0));
        Ok(res)
    }

    #[test]
    fn test_literal() {
        let expr = plex("5").unwrap();
        assert_eq!(expr, b(Expr::Literal(Literal::Int(5))));
        let expr = plex("true").unwrap();
        assert_eq!(expr, b(Expr::Literal(Literal::Bool(true))));
        let expr = plex("3.14").unwrap();
        assert_eq!(expr, b(Expr::Literal(Literal::Float(OrderedFloat(3.14)))));
        let expr = plex(r#""hello there""#).unwrap();
        assert_eq!(expr, b(Expr::Literal(Literal::String("hello there".into()))));
        let expr = plex("`hello there`").unwrap();
        assert_eq!(expr, b(Expr::Literal(Literal::Command("hello there".into()))));
    }

    
    #[test]
    fn test_simple_binop() {
        use BinaryOperator::{Plus, Times};
        use Expr::Literal;
        use self::Literal::Int;

        let expr = plex("5 + 5").unwrap();
        assert_eq!(expr, b(
            Expr::BinOp(BinOp::new(Plus, b(Literal(Int(5))), b(Literal(Int(5)))))
        ));

        let expr = plex("5 + 5 * 3").unwrap();
        assert_eq!(expr, b(
            Expr::BinOp(BinOp::new(
                Plus, 
                b(Literal(Int(5))), 
                b(Expr::BinOp(BinOp::new(Times, b(Literal(Int(5))), b(Literal(Int(3))))))
            ))
        ));
    }

    #[test]
    fn test_binop_with_unop() {
        use self::Literal::Int;
        use BinaryOperator::Plus;
        use UnaryOperator::Negate;

        let expr = plex("5 + -1").unwrap();
        assert_eq!(expr, b(
            Expr::BinOp(BinOp::new(
                Plus,
                b(Expr::Literal(Int(5))),
                b(Expr::UnOp(UnOp::new(Negate, b(Expr::Literal(Int(1))))))
            ))
        ));
    }

    #[test]
    fn test_binop_with_parens() {
        use self::Literal::Int;
        use BinaryOperator::{Plus, Times};
        
        let expr = plex("(1 + 1) * 3").unwrap();
        assert_eq!(expr, b(
            Expr::BinOp(BinOp::new(
                Times, 
                b(Expr::BinOp(BinOp::new(Plus, b(Expr::Literal(Int(1))), b(Expr::Literal(Int(1)))))),
                b(Expr::Literal(Int(3)))
            ))
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
        use self::Literal::Int;
        use BinaryOperator::{Plus, Times};
        use clam_common::ast;

        let expr = plex("
{
    a = b;
    b = 1 * 1 + 3 * 4;
}     
        ").unwrap();
        let res = b(
            Expr::Block(ast::Block::new(vec![
                Statement::Assign(Assign::new(
                    Identifier("a".into()).null_span(),
                    b(Expr::Identifier("b".into()))
                )).null_span(),
                Statement::Assign(Assign::new(
                    Identifier("b".into()).null_span(),
                    b(Expr::BinOp(BinOp::new(
                        Plus, 
                        b(Expr::BinOp(BinOp::new(Times, b(Expr::Literal(Int(1))), b(Expr::Literal(Int(1)))))),
                        b(Expr::BinOp(BinOp::new(Times, b(Expr::Literal(Int(3))), b(Expr::Literal(Int(4))))))
                    )))
                )).null_span(),
            ],
            None
            )
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
                Block::new(
                    vec![],
                Some(
                    Statement::Assign(Assign::new(
                        Identifier("a".into()).null_span(),
                        b(Expr::Identifier("b".into()))
                    )).null_span()
                ))
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
                Block::new(
                    vec![],
                Some(
                    Statement::Assign(Assign::new(
                        Identifier("a".into()).null_span(),
                        b(Expr::Identifier("b".into()))
                    )).null_span()
                ))
            )
        )))
    }

    #[test]
    fn test_simple_fn_call() {
        let expr = plex("f(a, b)").unwrap();
        let id: Identifier = "f".into();
        assert_eq!(expr, b(Expr::FnCall(FnCall::new(
            id.null_span(),
            vec![b(Expr::Identifier("a".into())),
            b(Expr::Identifier("b".into()))]))
        ));

        let expr = plex("f(-1, b)").unwrap();
        let id: Identifier = "f".into();
        assert_eq!(expr, b(Expr::FnCall(FnCall::new(
            id.null_span(), 
            vec![
                b(Expr::UnOp(UnOp::new(UnaryOperator::Negate, b(Expr::Literal(Literal::Int(1)))))),
                b(Expr::Identifier("b".into()))
            ]
        ))));
    }

    #[test]
    fn test_simple_fn_def() {

        let stmt = pl_stmt("
fn thing(a, b: int)
    a
        ").unwrap();
        assert_eq!(stmt, Statement::FnDef(FnDef{
            name: Identifier("thing".into()).null_span(),
            param_list: vec![
                (Identifier("a".into()).null_span(), None), 
                (Identifier("b".into()).null_span(), Some(Type::Primitive(Primitive::Int).null_span()))
            ],
            ret_type: None,
            body: b(Expr::Identifier("a".into()))
        }));

        let stmt = pl_stmt("
fn thing(a, b: int) -> bool
    a == b
        ").unwrap();
        assert_eq!(stmt, Statement::FnDef(FnDef{
            name: Identifier("thing".into()).null_span(),
            param_list: vec![
                (Identifier("a".into()).null_span(), None), 
                (Identifier("b".into()).null_span(), Some(Type::Primitive(Primitive::Int).null_span()))
            ],
            ret_type: Some(Type::Primitive(Primitive::Bool).null_span()),
            body: b(Expr::BinOp(BinOp::new(
                BinaryOperator::Eq,
                b(Expr::Identifier("a".into())),
                b(Expr::Identifier("b".into()))
            )))
        }));

        let stmt = pl_stmt("
fn thing(a, b: int) -> bool {
    a = b;
    a = 1 + 1
}
        ").unwrap();
        assert_eq!(stmt, Statement::FnDef(FnDef{
            name: Identifier("thing".into()).null_span(),
            param_list: vec![
                (Identifier("a".into()).null_span(), None), 
                (Identifier("b".into()).null_span(), Some(Type::Primitive(Primitive::Int).null_span()))
            ],
            ret_type: Some(Type::Primitive(Primitive::Bool).null_span()),
            body: b(Expr::Block(Block::new(vec![
                Statement::Assign(Assign::new(Identifier("a".into()).null_span(), b(Expr::Identifier("b".into())))).null_span(),
            ],
            Some(
                Statement::Assign(Assign::new(
                    Identifier("a".into()).null_span(),
                    b(Expr::BinOp(BinOp::new(
                        BinaryOperator::Plus,
                        b(Expr::Literal(Literal::Int(1))),
                        b(Expr::Literal(Literal::Int(1)))
                    )))
                )).null_span()
            )
            )))
        }));
    }

    #[test]
    fn test_span() {
        let expr = "if true { 5 + 5 }";
        let res = parser::ExprParser::new()
            .parse(Lexer::new(expr)).unwrap();
        let tokens: Vec<_> = Lexer::new(expr).collect();
        println!("tokens are {tokens:?}");
        println!("res is {res:?}");

    }

/*
    #[test]
    fn test_simple_assignment() {
        use Statement::Assign;

        let stmt = pl_stmt("thing = true").unwrap();
        let id: Identifier = "thing".into();
        assert_eq!(stmt, Assign(
            id.null_span(), 
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

    #[test]
    fn test_simple_lambda_def() {
        use Expr::LambdaDef;

        // let expr = plex("|a, b: int| a + b").unwrap();
        let expr = plex(r"\ a, b: int => a + b").unwrap();
        assert_eq!(expr, b(LambdaDef(
            vec![
                (Identifier("a".into()), None), 
                (Identifier("b".into()), Some(Type::Primitive(Primitive::Int)))
            ],
            b(Expr::BinOp(
                BinaryOperator::Plus,
                b(Expr::Identifier("a".into())),
                b(Expr::Identifier("b".into()))
            ))
        )));
    }
*/
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
