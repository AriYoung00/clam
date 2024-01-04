#![feature(custom_test_frameworks)]
#![test_runner(datatest::runner)]
extern crate clam_interpreter;
extern crate clam_common;
extern crate clam_parser;
extern crate im_rc;
extern crate ariadne;

use std::cell::RefCell;


use std::sync::{Arc, Mutex};
use std::sync::mpsc;

use clam_common::ast::{Identifier, FnDef, StructDef};
use clam_parser::lexer::Lexer;
use clam_parser::parser::ModuleParser;
use clam_interpreter::data::{ClamRuntimeError,NewHeap};
use clam_interpreter::{eval_expr, Ctx, EvalResult};

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

fn into_std_result<OutType>(res: EvalResult<OutType, ClamRuntimeError>) -> std::result::Result<OutType, ClamInterpreterError> {
    use EvalResult::*;
    match res {
        Err(e)    => Result::Err(ClamInterpreterError::Runtime(e)),
        Ok(v)     => Result::Ok(v),
        Return(_) => panic!("called Into<Result> on ClamError::Return"),
        Break     => panic!("called Into<Result> on ClamError::Break"),
        Continue  => panic!("called Into<Result> on ClamError::Break"),
    }
}


pub fn interpret_file_contents(contents: &str) -> Result<(), ClamInterpreterError> {
    use im_rc::HashMap;

    let token_stream = Lexer::new(contents);
    let parser = ModuleParser::new();
    let mut clam_mod = parser.parse(token_stream).unwrap().0;

    let mut main_idx = -1_i32;
    for (idx, func) in clam_mod.iter().enumerate() {
        if func.is_left() && func.clone().unwrap_left().name.1.0.as_str() == "main" {
            main_idx = idx as i32;
            break;
        }
    }

    if main_idx == -1 {
        return Err(ClamInterpreterError::MainNotFount);
    }

    let main_func = clam_mod.remove(main_idx as usize);
    let (funcs, structs): (Vec<_>, Vec<_>) = clam_mod.into_iter()
        .partition(|e| e.is_left());
    let funcs: HashMap<Identifier, FnDef> = funcs.into_iter()
        .map(|func| func.unwrap_left())
        .map(|func| (func.name.1.clone(), func))
        .collect();
    let structs: HashMap<Identifier, StructDef> = structs.into_iter()
        .map(|maybe_struct| maybe_struct.unwrap_right())
        .map(|struct_def| (struct_def.name.1.clone(), struct_def))
        .collect();

    let vars = HashMap::new();
    let heap = Arc::new(RefCell::new(NewHeap::default()));
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
    let main_res = {
        let stdout = stdout;
        let mut ctx = Ctx::new(heap, funcs, structs, stdout, vars, Vec::new());

        eval_expr(&main_func.unwrap_left().body, &mut ctx)
    };

    // Ok(res.lock().unwrap().iter().collect())
    stdout_thread.join().unwrap();
    
    // lol toilet operator
    into_std_result(main_res.map_ok(|_|()))
}

fn print_error(err: ClamInterpreterError, src: &str, name: &str) {
    match err {
        ClamInterpreterError::FileNotFound => todo!(),
        ClamInterpreterError::MainNotFount => todo!(),
        ClamInterpreterError::Runtime(e) => {
            use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};

            let mut colors = ColorGenerator::new();

            // Generate & choose some colours for each of our elements
            let a = colors.next();
            let _b = colors.next();
            let _out = Color::Fixed(81);

            Report::build(ReportKind::Error, name, e.loc.0)
                .with_code(3)
                .with_message(format!("Runtime error"))
                .with_label(
                    Label::new((name, e.loc.0..e.loc.1))
                        .with_message(format!("This is an {}", "error".fg(a)))
                        .with_color(a),
                )
                .with_note(format!("You should probably fix this"))
                .finish()
                .print((name, Source::from(src)))
                .unwrap();
        },
        ClamInterpreterError::Unknown => todo!(),
    }
}

fn main() {
    use std::env;
    let args: Vec<String> = env::args().collect();

    let fpath = args[1].as_str();
    let contents = std::fs::read_to_string(fpath).unwrap();
    let fname = fpath.split("/").last().unwrap();
    let _res = interpret_file_contents(contents.as_str());


    match _res {
        Ok(_)  => std::process::exit(0),
        Err(e) => {
            print_error(e, contents.as_str(), fname);
            std::process::exit(1);
        }
    }
}

