pub mod lexer;

#[cfg(test)]
mod lexer_tests;

#[cfg(test)]
mod parser_tests;

mod tokens;
use lalrpop_util::lalrpop_mod;
pub use tokens::Token;

lalrpop_mod!(pub parser);