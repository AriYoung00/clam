#![allow(dead_code)]
#![allow(unused_variables)]

use criterion::{black_box, criterion_main, criterion_group, Criterion, Bencher};
use clam_parser::lexer::*;
use chumsky::Parser;

fn lex(s: &str) {
    lexer().parse(black_box(s)).unwrap();
}

fn lex_single_keyword(bencher: &mut Bencher) {
    bencher.iter(|| {
        lex("fn");
        lex("if");
        lex("else");
        lex("for");
        lex("while");
        lex("break");
        lex("continue");
        lex("in");
        lex("let");
        lex("var");
    });
}

fn lex_single_symbol(bencher: &mut Bencher) {
    bencher.iter(|| {
        lex("(");
        lex(")");
        lex("{");
        lex("}");
        lex("[");
        lex("]");
        lex("<");
        lex(">");
        lex(";");
        lex("=");
        lex(",");
        lex("->");
        lex(".");
        lex("*");
        lex("/");
        lex("+");
        lex("#");
        lex("?");
        lex("<=");
        lex(">=");
        lex("==");
        lex("!=");
        lex("&&");
        lex("||");
    });
}

fn lex_simple_fn(bencher: &mut Bencher) {
    const FUNC_DECL: &str = r#"
fn test(i: int) -> int {
    i + 1
}
        "#;

    bencher.iter(|| lex(FUNC_DECL));
}


pub fn lexer_benchmark(c: &mut Criterion) {
    c.bench_function("lex single keyword", lex_single_keyword);
    c.bench_function("lex simple fn", lex_simple_fn);
    c.bench_function("lex single symbol", lex_single_symbol);
}

criterion_group!(benches, lexer_benchmark);
criterion_main!(benches);
