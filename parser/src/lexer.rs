use logos::{Logos, SpannedIter};
use crate::Token;

#[derive(Debug)]
pub enum LexicalError {
    InvalidToken,
}

pub struct Lexer<'input> {
    token_stream: SpannedIter<'input, Token>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            token_stream: Token::lexer(input).spanned()
        }
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;
impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.token_stream.next().map(|(token, span)| {
            match token {
                Token::Error => Err(LexicalError::InvalidToken),
                _ => Ok((span.start, token, span.end)),
            }
        })
    }
}