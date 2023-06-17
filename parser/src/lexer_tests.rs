#[cfg(test)]
mod test {
    use chumsky::prelude::*;
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

        assert_eq!(lex("true"), vec![Literal(Bool(true))]);
        assert_eq!(lex("false"), vec![Literal(Bool(false))]);
        assert_eq!(lex("11300"), vec![Literal(Int(11300))]);
        assert_eq!(lex("11300.15"), vec![Literal(Float(11300.15))]);
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
        assert_eq!(lex("var"), vec![Token::Keyword(Keyword::Let)]);
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
    fn test_func_decl() {
        use self::Token::*;
        use self::Keyword::*;
        use self::Symbol::*;
        use self::Primitive::*;

        let func_decl = r#"
fn test(i: int) -> int {
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
