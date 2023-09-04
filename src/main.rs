extern crate clam_interpreter;
extern crate clam_common;
extern crate clam_parser;
extern crate im_rc;

use std::cell::{RefCell, Ref};
use std::hash::Hash;
use std::io::{Stdout, Cursor};
use std::sync::Arc;

use clam_common::ast::{Identifier, FnDef};
use clam_parser::lexer::Lexer;
use clam_parser::parser::ModuleParser;
use clam_interpreter::data::{ClamRuntimeError, ClamInt, Heap};
use clam_interpreter::{eval_expr, Ctx};

#[derive(Debug)]
enum ClamInterpreterError {
    FileNotFound,
    MainNotFount,
    Runtime(ClamRuntimeError),
    Unknown,
}

impl From<std::io::Error> for ClamInterpreterError {
    fn from(value: std::io::Error) -> Self {
        println!("error is {}", value);
        match value.kind() {
            std::io::ErrorKind::NotFound => ClamInterpreterError::FileNotFound,
            _ => ClamInterpreterError::Unknown
        }
    }
}

fn interpret_file(name: &str) -> Result<String, ClamInterpreterError> {
    use std::fs;
    use im_rc::HashMap;

    let contents = fs::read_to_string(name).unwrap();
    let token_stream = Lexer::new(&contents);
    let parser = ModuleParser::new();
    let mut clam_mod = parser.parse(token_stream).unwrap().0;

    let mut main_idx = -1_i32;
    for (idx, func) in clam_mod.iter().enumerate() {
        if func.clone().unwrap_left().name.1.0.as_str() == "main" {
            main_idx = idx as i32;
            break;
        }
    }

    if main_idx == -1 {
        return Err(ClamInterpreterError::MainNotFount);
    }
    
    let main_func = clam_mod.remove(main_idx as usize);
    let funcs: HashMap<Identifier, FnDef> = clam_mod.into_iter()
        .map(|func| (func.clone().unwrap_left().name.1.clone(), func.unwrap_left()))
        .collect();
    let vars = HashMap::new();
    let structs = HashMap::new();
    let heap = Arc::new(RefCell::new(Heap::default()));
    let stdout_vec = Vec::with_capacity(10000);
    let stdout = Arc::new(RefCell::new(stdout_vec));

    let mut ctx = Ctx{
        heap,
        vars,
        funcs,
        structs,
        stdout,
    };

    let _main_res = eval_expr(&main_func.unwrap_left().body, &mut ctx).unwrap();
    let Ctx{ stdout, .. } = ctx;
    Ok(stdout.take().into_iter().map(|c| char::from(c)).collect())
}

fn main() {
    use std::env;
    let args: Vec<String> = env::args().collect();

    let res = interpret_file(&args[1]).unwrap();
    println!("{}", res);
}

