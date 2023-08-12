/*
#[cfg(test)]
mod test {
    use chumsky::prelude::*;
    use clam_common::tokens::*;
    use crate::lexer::*;


    fn lex(s: &str) -> Vec<Token> {
        lexer().parse(s).unwrap()
    }

    #[test]
    fn test_single_primitive() {
        use self::Token::Primitive;
        use self::Primitive::*;
        assert_eq!(lex("bool"),   vec![Primitive(Bool)]);
        assert_eq!(lex("int"),    vec![Primitive(Int)]);
        assert_eq!(lex("float"),  vec![Primitive(Float)]);
        assert_eq!(lex("string"), vec![Primitive(String)]);
    }

    #[test]
    fn test_literal() {
        use self::Token::Literal;
        use self::Literal::*;
        
        let lex = |s| lexer().parse(s).unwrap();

        // bool literal
        assert_eq!(lex("true"), vec![Literal(Bool(true))]);
        assert_eq!(lex("false"), vec![Literal(Bool(false))]);

        // int literal
        assert_eq!(lex("11300"), vec![Literal(Int(11300))]);
        assert_eq!(lex("113 10"), vec![Literal(Int(113)), Literal(Int(10))]);

        // float literal
        assert_eq!(lex("11300.15"), vec![Literal(Float(11300.15.into()))]);

        // string literal
        assert_eq!(lex(r#""hello there""#), vec![Literal(Str("hello there".into()))]);
        assert_eq!(lex(r#""hello"   "there""#), vec![Literal(Str("hello".into())), Literal(Str("there".into()))]);
        assert_eq!(lex("\"hello\"\n\n\"there\""), vec![Literal(Str("hello".into())), Literal(Str("there".into()))]);
        assert_eq!(lex("\"hello\n\nthere\""), vec![Literal(Str("hello\n\nthere".into()))]);

        // command literal
        assert_eq!(lex(r#"```ls```"#), vec![Literal(Command("ls".into()))]);
        assert_eq!(lex(r#"```
                ls | grep "hello"
                mkdir test
                cd test
                touch thing.txt```"#),
                vec![Literal(Command(r#"
                ls | grep "hello"
                mkdir test
                cd test
                touch thing.txt"#.into()))]);
    }

    #[test]
    fn test_single_keyword() {
        assert_eq!(lex("fn"), vec![Token::Keyword(Keyword::Fn)]);
        assert_eq!(lex("if"), vec![Token::Keyword(Keyword::If)]);
        assert_eq!(lex("else"), vec![Token::Keyword(Keyword::Else)]);
        assert_eq!(lex("for"), vec![Token::Keyword(Keyword::For)]);
        assert_eq!(lex("while"), vec![Token::Keyword(Keyword::While)]);
        assert_eq!(lex("break"), vec![Token::Keyword(Keyword::Break)]);
        assert_eq!(lex("continue"), vec![Token::Keyword(Keyword::Continue)]);
        assert_eq!(lex("in"), vec![Token::Keyword(Keyword::In)]);
        assert_eq!(lex("let"), vec![Token::Keyword(Keyword::Let)]);
        assert_eq!(lex("var"), vec![Token::Keyword(Keyword::Var)]);
    }

    #[test]
    fn test_comment() {
        // assert_eq!(lex("// this is a comment"), vec![]);
        assert_eq!(lex("
            a
            /*
             * a multi line String
             * for this test case
             */
            "), vec![Token::Identifier("a".into())]);
        assert_eq!(lex("// both types of comments
                a
                /* in the same
                 * input
                 * wow
                 */"), vec![Token::Identifier("a".into())]);
    }

    #[test]
    fn test_multi_keyword() {
        use self::Primitive::*;
        use self::Token::Keyword;
        use self::Token::Primitive;
        use self::Keyword::*;

        assert_eq!(lexer().parse("fn bool float").unwrap(), vec![Keyword(Fn), Primitive(Bool), Primitive(Float)]);
    }

    #[test]
    fn test_single_symbol() {
        use self::Token::Symbol;
        use self::Symbol::*;

        assert_eq!(lex("("), vec![Symbol(LParen)]);
        assert_eq!(lex("            (         "), vec![Symbol(LParen)]);
        assert_eq!(lex(r#"            (         
                
"#), vec![Symbol(LParen)]);
        assert_eq!(lex(")"), vec![Symbol(RParen)]);
        assert_eq!(lex("{"), vec![Symbol(LBrace)]);
        assert_eq!(lex("}"), vec![Symbol(RBrace)]);
        assert_eq!(lex("["), vec![Symbol(LBracket)]);
        assert_eq!(lex("]"), vec![Symbol(RBracket)]);
        assert_eq!(lex("<"), vec![Symbol(LAngle)]);
        assert_eq!(lex(">"), vec![Symbol(RAngle)]);
        assert_eq!(lex(";"), vec![Symbol(SemiColon)]);
        assert_eq!(lex("="), vec![Symbol(Equal)]);
        assert_eq!(lex(","), vec![Symbol(Comma)]);
        assert_eq!(lex("->"), vec![Symbol(Arrow)]);
        assert_eq!(lex("."), vec![Symbol(Dot)]);
        assert_eq!(lex("*"), vec![Symbol(Star)]);
        assert_eq!(lex("/"), vec![Symbol(Slash)]);
        assert_eq!(lex("+"), vec![Symbol(Plus)]);
        assert_eq!(lex("#"), vec![Symbol(Hash)]);
        assert_eq!(lex("?"), vec![Symbol(QuestionMark)]);
        assert_eq!(lex("<="), vec![Symbol(LessThanEqual)]);
        assert_eq!(lex(">="), vec![Symbol(GreaterThanEqual)]);
        assert_eq!(lex("=="), vec![Symbol(EqualEqual)]);
        assert_eq!(lex("!="), vec![Symbol(NotEqual)]);
        assert_eq!(lex("&&"), vec![Symbol(And)]);
        assert_eq!(lex("||"), vec![Symbol(Or)]);
        assert_eq!(lex("&"),  vec![Symbol(Ampersand)]);
        assert_eq!(lex("-"),  vec![Symbol(Minus)]);
        assert_eq!(lex("!"),  vec![Symbol(ExclamationMark)]);
    }

    #[test]
    fn test_identifier() {
        use self::Token::Symbol;
        use self::Token::Identifier;
        use self::Symbol::*;

        assert_eq!(lex("helloThere"), vec![Identifier("helloThere".into())]);
        assert_eq!(lex("helloThere()"), vec![Identifier("helloThere".into()), Symbol(LParen), Symbol(RParen)]);
    }

    #[test]
    fn test_var_decl() {
        use self::Token::*;
        use self::Symbol::*;
        use self::Keyword::*;
        use self::Primitive::*;

        let var_decl = r#"
let thing: int = 42;
            "#;
        assert_eq!(lex(var_decl), vec![Keyword(Let), Identifier("thing".into()), Symbol(Colon), Primitive(Int),
                                       Symbol(Equal), Literal(self::Literal::Int(42)), Symbol(SemiColon)]);

        let var_decl = r#"
let thing = "hello there world";
            "#;
        assert_eq!(lex(var_decl), vec![Keyword(Let), Identifier("thing".into()), Symbol(Equal),
                                       Literal(self::Literal::Str("hello there world".into())), Symbol(SemiColon)]);

        let var_decl = r#"
let thing1 = thing2;
            "#;
        assert_eq!(lex(var_decl), vec![Keyword(Let), Identifier("thing1".into()), Symbol(Equal),
                                      Identifier("thing2".into()), Symbol(SemiColon)]);
    }

    #[test]
    fn test_func_decl() {
        use self::Token::*;
        use self::Keyword::*;
        use self::Symbol::*;
        use self::Primitive::*;

        let func_decl = r#"
/*
 * This is a multiline comment
 */
fn test(i: int) -> int {
    // add 1 to i
    i + 1
}
        "#;
        assert_eq!(lex(func_decl), vec![
            Keyword(Fn), Identifier("test".into()), Symbol(LParen), Identifier("i".into()), Symbol(Colon),
            Primitive(Int), Symbol(RParen), Symbol(Arrow), Primitive(Int), Symbol(LBrace), 
                Identifier("i".into()), Symbol(Plus), Literal(self::Literal::Int(1)),
            Symbol(RBrace)
        ])
    }
}
*/