#![feature(custom_test_frameworks)]
#![test_runner(datatest::runner)]
extern crate clam_interpreter;
extern crate clam_common;
extern crate clam_parser;
extern crate im_rc;

use std::cell::{RefCell};


use std::sync::{Arc, Mutex};
use std::sync::mpsc;

use clam_common::ast::{Identifier, FnDef};
use clam_parser::lexer::Lexer;
use clam_parser::parser::ModuleParser;
use clam_interpreter::data::{ClamRuntimeError, Heap};
use clam_interpreter::{eval_expr, Ctx};

#[cfg(test)]
mod test;

#[derive(Debug)]
pub enum ClamInterpreterError {
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

pub fn interpret_file_contents(contents: &str) -> Result<String, ClamInterpreterError> {
    use im_rc::HashMap;

    let token_stream = Lexer::new(contents);
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
    let (stdout, stdout_recv) = mpsc::channel();
    let res: Arc<Mutex<Vec<char>>> = Arc::new(Mutex::new(Vec::new()));
    let stdout_thread = {
        let res = res.clone();
        std::thread::spawn(move || {
            while let Ok(c) = stdout_recv.recv() {
                let c = char::from(c);
                print!("{c}");
                res.lock().unwrap().push(c);
            }
        })
    };

    // need to move stdout so the recv closes
    let _main_res = {
        let stdout = stdout;
        let mut ctx = Ctx::new(heap, funcs, structs, stdout, vars, Vec::new());

        eval_expr(&main_func.unwrap_left().body, &mut ctx).unwrap()
    };

    // Ok(res.lock().unwrap().iter().collect())
    stdout_thread.join().unwrap();
    let x = Ok(res.lock().unwrap().iter().collect());
    x
}

fn main() {
    use std::env;
    let args: Vec<String> = env::args().collect();

    let contents = std::fs::read_to_string(args[1].as_str()).unwrap();
    let _res = interpret_file_contents(contents.as_str()).unwrap();
    // println!("{}", res);
}

