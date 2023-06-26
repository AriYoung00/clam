#[allow(unused_imports)]

#[cfg(test)]
mod test {
    use chumsky::prelude::*;
    use clam_common::tokens::*;
    use clam_common::ast::*;
    use crate::parser::*;


    fn parse(s: &str) -> Block {
        parser().parse(s).unwrap()
    }

    fn block(exprs: Vec<Expr>) -> Block {
        Block::new(exprs.into_iter().map(Statement::Expr as fn(_) -> _).collect())
    }

    // #[test]
    // fn test_single_primitive() {
    //     use self::Token::Primitive;
    //     use self::Primitive::*;
    //     assert_eq!(parse("bool"),   vec![Primitive(Bool)]);
    //     assert_eq!(parse("int"),    vec![Primitive(Int)]);
    //     assert_eq!(parse("float"),  vec![Primitive(Float)]);
    //     assert_eq!(parse("string"), vec![Primitive(String)]);
    // }

    #[test]
    fn test_literal() {
        use Expr::Literal;
        use self::Literal::*;

        //assert_eq!(parse("1 + 1 * 1;"), Block::new(vec![Statement::Expr(
        //    Expr::BinOp(
        //        BinaryOperator::Plus, 
        //        Box::new(Expr::Literal(Int(1))),
        //        Box::new(Expr::BinOp(
        //            BinaryOperator::Mul,
        //            Box::new(Expr::Literal(Int(1))),
        //            Box::new(Expr::Literal(Int(1))),
        //        )),
        //    )
        //)]));
        //assert_eq!(parse("1 * 1 + 1;"), Block::new(vec![Statement::Expr(
        //    Expr::BinOp(
        //        BinaryOperator::Plus, 
        //        Box::new(Expr::BinOp(
        //            BinaryOperator::Mul,
        //            Box::new(Expr::Literal(Int(1))),
        //            Box::new(Expr::Literal(Int(1))),
        //        )),
        //        Box::new(Expr::Literal(Int(1))),
        //    )
        //)]));
        //assert_eq!(parse("1 * (1 + 1);"), Block::new(vec![Statement::Expr(
        //    Expr::BinOp(
        //        BinaryOperator::Mul, 
        //        Box::new(Expr::Literal(Int(1))),
        //        Box::new(Expr::BinOp(
        //            BinaryOperator::Plus,
        //            Box::new(Expr::Literal(Int(1))),
        //            Box::new(Expr::Literal(Int(1))),
        //        )),
        //    )
        //)]));

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
}
