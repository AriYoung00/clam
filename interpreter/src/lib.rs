#![feature(if_let_guard)]
#![feature(try_trait_v2)]

pub mod data;
// mod type_checker;

use std::{
    cell::RefCell,
    convert::Infallible,
    ops::{ControlFlow, FromResidual, Try},
    sync::Arc,
    sync::mpsc::Sender,
};

use clam_common::ast::{
    Assign, BinOp, Block, Conditional, Expr, FnCall, FnDef, Identifier, Let, Span, Statement, UnOp,
    WhileLoop, StructDef,
};
use clam_common::util::SendAll;
use data::{
    ClamBool, ClamData, ClamRuntimeError, ClamString, ClamUserType,
};
use derive_more::Constructor;
use im_rc::HashMap;

use crate::data::{ErrorSource, ErrorSource::*, NewGcRef, NewHeap};

// type Result<T> = std::result::Result<T, ClamRuntimeError>;
pub enum EvalResult<T, E> {
    Ok(T),
    Err(E),
    Return(ClamData),
    Break,
    Continue,
}

use EvalResult::*;

#[allow(dead_code)]
impl<T, E> EvalResult<T, E>
where
    E: std::fmt::Debug,
{
    pub fn map_err<NewE>(self, f: impl FnOnce(E) -> NewE) -> EvalResult<T, NewE> {
        match self {
            Err(e) => Err(f(e)),
            Ok(v) => Ok(v),
            Return(e) => Return(e),
            Break => Break,
            Continue => Continue,
        }
    }
    pub fn unwrap(self) -> T {
        match self {
            Ok(val) => val,
            Err(e) => panic!("Called `unwrap` on EvalResult::Err({:?})", e),
            Return(val) => panic!("Called `unwrap` on EvalResult::Return({val:?})"),
            Break => panic!("Called `unwrap` on EvalResult::Break"),
            Continue => panic!("Called `unwrap` on EvalResult::Continue"),
        }
    }
    pub fn map_ok<NewOk>(self, f: impl FnOnce(T) -> NewOk) -> EvalResult<NewOk, E> {
        match self {
            Err(e) => Err(e),
            Ok(v) => Ok(f(v)),
            Return(e) => Return(e),
            Break => Break,
            Continue => Continue,
        }
    }

    pub fn is_err(&self) -> bool {
        matches!(self, EvalResult::Err(_))
    }
}

impl<T, E> Try for EvalResult<T, E> {
    type Output = T;
    type Residual = EvalResult<Infallible, E>;

    fn from_output(output: Self::Output) -> Self {
        Self::Ok(output)
    }

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self {
            Ok(v) => ControlFlow::Continue(v),
            Err(e) => ControlFlow::Break(Err(e)),
            Return(v) => ControlFlow::Break(Return(v)),
            Break => ControlFlow::Break(Break),
            Continue => ControlFlow::Break(Continue),
        }
    }
}

impl<T, E> FromResidual<EvalResult<Infallible, E>> for EvalResult<T, E> {
    fn from_residual(residual: EvalResult<Infallible, E>) -> Self {
        match residual {
            Ok(_) => unreachable!("from residual on infallible"),
            Err(e) => Err(e),
            Return(v) => Return(v),
            Break => Break,
            Continue => Continue,
        }
    }
}

type Result<T> = EvalResult<T, ClamRuntimeError>;

trait ErrSpan {
    fn add_loc(self, start: usize, end: usize) -> Self;
    fn likely_has_loc(&self) -> bool;

    fn map_no_loc(self, start: usize, end: usize) -> Self
    where
        Self: Sized,
    {
        if self.likely_has_loc() {
            self
        } else {
            self.add_loc(start, end)
        }
    }
}

impl<T> ErrSpan for Result<T> {
    fn add_loc(self, start: usize, end: usize) -> Self {
        self.map_err(|ClamRuntimeError { src, .. }| ClamRuntimeError {
            loc: (start, end),
            src,
        })
    }

    fn likely_has_loc(&self) -> bool {
        match self {
            Err(e) => e.loc != (0, 0),
            _ => true,
        }
    }
}

#[derive(Constructor, Clone)]
pub struct Ctx {
    pub heap: Arc<RefCell<NewHeap>>,
    pub funcs: HashMap<Identifier, FnDef>,
    pub structs: HashMap<Identifier, StructDef>,

    // TODO: figure out a better way to represent this
    pub stdout: Sender<u8>,

    vars: HashMap<Identifier, ClamData>,
    current_scope: Vec<Identifier>,
}

#[allow(dead_code)]
impl Ctx {
    pub fn to_heap(&mut self, data: ClamUserType) -> NewGcRef<ClamUserType> {
        self.heap.borrow_mut().move_to_heap(data)
    }

    pub fn new_scope_with_vars(&self, vars: HashMap<Identifier, ClamData>) -> Self {
        Ctx {
            heap: self.heap.clone(),
            vars,
            funcs: self.funcs.clone(),
            structs: self.structs.clone(),
            stdout: self.stdout.clone(),
            current_scope: Vec::new(),
        }
    }

    pub fn new_scope(&self) -> Self {
        Ctx {
            heap: self.heap.clone(),
            funcs: self.funcs.clone(),
            structs: self.structs.clone(),
            stdout: self.stdout.clone(),
            vars: self.vars.clone(),
            current_scope: Vec::new(),
        }
    }

    pub fn end_scope(&mut self, mut scope: Self) {
        for new_var in scope.current_scope.as_slice() {
            scope.vars.remove(new_var);
        }

        self.vars = scope.vars.union(self.vars.clone())
    }

    #[inline]
    pub fn insert_var(&mut self, id: Identifier, val: ClamData) {
        self.vars.insert(id.clone(), val);
        self.current_scope.push(id);
    }

    pub fn contains_var(&self, id: &Identifier) -> bool {
        self.vars.contains_key(id)
    }

    pub fn set_var(&mut self, id: &Identifier, val: ClamData) {
        self.vars[id] = val;
    }

    pub fn get_var(&mut self, id: &Identifier) -> Option<&ClamData> {
        self.vars.get(id)
    }
}


fn call_user_fn(def: &FnDef, args: &Vec<Span<Box<Expr>>>, ctx: &mut Ctx) -> Result<ClamData> {
    let mut arg_vals = Vec::new();
    for arg in args {
        arg_vals.push(eval_expr(arg, ctx)?);
    }

    let mappings = def
        .param_list
        .iter()
        .map(|((_, id, _), _)| id.clone())
        .zip(arg_vals);
    let mut new_vars = HashMap::new();
    new_vars.extend(mappings);

    let mut new_ctx = ctx.new_scope_with_vars(new_vars);

    match eval_expr(&def.body, &mut new_ctx) {
        Return(val) => Ok(val),
        other => other,
    }
}

fn call_builtin_fn(id: &Identifier, args: &[Span<Box<Expr>>], ctx: &mut Ctx) -> Result<ClamData> {
    match id.0.as_ref() {
        "println" => {
            let mut args = args.iter();
            if let Some(arg) = args.next() {
                let res = eval_expr(arg, ctx)?;
                ctx.stdout.send_all(res.to_string().bytes()).expect("stdout was unexpectedly severed!");
            }

            for arg in args {
                let res = eval_expr(arg, ctx)?;
                ctx.stdout.send_all(res.to_string().bytes()).expect("stdout was unexpectedly severed!");
            }

            ctx.stdout.send(b'\n').expect("stdout was unexpectedly severed!");

            Ok(ClamData::Empty)
        }
        _ => unreachable!("should not be able to call_builtin_fn with nonexistent builtin"),
    }
}

// stupid hack
fn is_builtin_fn(id: &Identifier) -> bool {
    matches!(id.0.as_ref(), "println")
}

#[allow(dead_code)]
pub fn eval_expr(exp: &Span<Box<Expr>>, ctx: &mut Ctx) -> Result<ClamData> {
    use clam_common::ast::Literal;
    let (start, exp, end) = exp;
    let (start, end) = (*start, *end);

    match exp.as_ref() {
        Expr::Literal(lit) => Ok(match lit {
            Literal::Int(i) => ClamData::Int((*i).into()),
            Literal::Float(f) => ClamData::Float((*f).into()),
            Literal::Bool(b) => ClamData::Bool((*b).into()),
            Literal::String(s) => {
                ClamData::String(ClamString((s.clone()).into()))
            }
            Literal::Command(_c) => todo!("Evaluate commands"),
            Literal::Struct(_, _) => {
                todo!()
            }
        }),

        Expr::BinOp(BinOp { op, lhs, rhs }) => {
            use clam_common::ast::BinaryOperator;
            let lhs = eval_expr(lhs, ctx)?;
            let rhs = eval_expr(rhs, ctx)?;
            let cbool = |b| ClamData::Bool(ClamBool(b));

            Ok(match op {
                BinaryOperator::Plus   => lhs + rhs,
                BinaryOperator::Minus  => lhs - rhs,
                BinaryOperator::Times  => lhs * rhs,
                BinaryOperator::Div    => (lhs / rhs).add_loc(start, end)?,
                BinaryOperator::Mod    => (lhs % rhs).add_loc(start, end)?,
                BinaryOperator::Lt     => cbool(lhs < rhs),
                BinaryOperator::Lte    => cbool(lhs <= rhs),
                BinaryOperator::Gt     => cbool(lhs > rhs),
                BinaryOperator::Gte    => cbool(lhs >= rhs),
                BinaryOperator::Eq     => cbool(lhs == rhs),
                BinaryOperator::Ne     => cbool(lhs != rhs),
                BinaryOperator::And    => cbool(ensure_bool(&lhs) && ensure_bool(&rhs)),
                BinaryOperator::Or     => cbool(ensure_bool(&lhs) || ensure_bool(&rhs)),
                BinaryOperator::BitAnd => lhs & rhs,
                BinaryOperator::BitOr  => lhs | rhs,
                BinaryOperator::BitXor => lhs ^ rhs,
            })
        }

        Expr::UnOp(UnOp { op, rhs }) => {
            let res = eval_expr(rhs, ctx)?;
            Ok(match op {
                clam_common::ast::UnaryOperator::Not
                 | clam_common::ast::UnaryOperator::BitInvert => !res,
                clam_common::ast::UnaryOperator::Negate => -res,
            })
        }

        Expr::Identifier(i) => match ctx.get_var(i) {
            Some(val) => Ok(val.clone()),
            None => VariableNotFound.with_loc(start, end),
        },

        Expr::FieldAccess(_) => todo!(),

        Expr::FnCall(FnCall { id, args }) => {
            let (start, id, end) = id;
            if let Some(def) = ctx.funcs.get(id) {
                // TODO: figure out if clone here screws something up
                call_user_fn(def, args, &mut ctx.clone())
            }
            else if is_builtin_fn(id) {
                call_builtin_fn(id, args, ctx)
            }
            else {
                FunctionNotFound.with_loc(*start, *end)
            }
        },

        Expr::LambdaDef(_) => todo!(),

        Expr::Block(block) => eval_block(block, ctx),

        Expr::Conditional(Conditional { cond, body, r#else }) => {
            let res = eval_expr(cond, ctx)?;
            match res {
                ClamData::Bool(ClamBool(true)) => eval_block(body, ctx),
                ClamData::Bool(ClamBool(false)) if let Some(r#else) = r#else 
                    => eval_block(r#else, ctx),
                _ => Ok(ClamData::Empty)
            }
        }

        Expr::WhileLoop(WhileLoop { cond, body }) => {
            // let mut cond = eval_expr(cond, ctx)?;
            let mut res = ClamData::Empty;

            while ensure_bool(&eval_expr(cond, ctx)?) {
                match eval_block(body, ctx) {
                    Ok(data) => res = data,
                    Break => return Ok(ClamData::Empty),
                    // somewhere in the eval process, we hit a continue.
                    // in this case, we want to do nothing and execute the next iteration
                    Continue => (),
                    // for Err(_), Return(_), we want to interrupt iteration and short-circuit
                    other => return other,
                };
            }

            Ok(res)
        }

        Expr::ForLoop(_) => todo!(),
    }
}

fn ensure_bool(data: &ClamData) -> bool {
    match data {
        ClamData::Bool(ClamBool(b)) => *b,
        _ => unreachable!("found non-boolean type where boolean was expected"),
    }
}

fn eval_block(block: &Block, ctx: &mut Ctx) -> Result<ClamData> {
    // we need to figure out a way to update variables in the enclosing scope, but discard new variables declared within this block
    // clone the context
    let mut new_ctx = ctx.new_scope();

    for (start, line, end) in &block.body {
        let _ = eval_statement(line, &mut new_ctx).map_no_loc(*start, *end)?;
    }

    let res = if let Some((start, last, end)) = &block.last {
        eval_statement(last, &mut new_ctx).map_no_loc(*start, *end)
    } else {
        Ok(ClamData::Empty)
    };

    ctx.end_scope(new_ctx);
    res
}

fn eval_statement(stmt: &Statement, ctx: &mut Ctx) -> Result<ClamData> {
    use ClamData::*;
    match stmt {
        Statement::FnDef(_) => unreachable!("Function def should be removed"),
        Statement::Let(Let {
            id,
            r#type: _,
            expr,
        }) => {
            let (_, id, _) = id;
            let res = expr.as_ref().map(|exp| eval_expr(exp, ctx)).unwrap()?;

            ctx.insert_var(id.clone(), res);

            Ok(Empty)
        }
        Statement::Assign(Assign { id, expr }) => {
            let (var_start, id, var_end) = id;

            if !ctx.contains_var(id) {
                return ErrorSource::VariableNotFound.with_loc(*var_start, *var_end);
            }

            let res = eval_expr(expr, ctx)?;
            ctx.set_var(id, res);

            Ok(Empty)
        }
        Statement::Expr(e) => eval_expr(e, ctx),
        Statement::Break => Break,
        Statement::Return(expr) => Return(match expr {
            Some(expr) => eval_expr(expr, ctx)?,
            None => ClamData::Empty,
        }),
    }
}

#[cfg(test)]
mod test {
    use std::sync::mpsc::channel;

    use super::*;
    use crate::{data::*, eval_expr};
    use clam_common::ast::*;
    use im_rc::HashMap;

    fn bin_op(op: BinaryOperator, lhs: Span<Box<Expr>>, rhs: Span<Box<Expr>>) -> Span<Box<Expr>> {
        (0, Box::new(Expr::BinOp(BinOp { op, lhs, rhs })), 0)
    }

    fn fun_call<const NARGS: usize>(id: &str, args: [Span<Box<Expr>>; NARGS]) -> Span<Box<Expr>> {
        (0, Box::new(Expr::FnCall(FnCall { id: (0, id.into(), 0), args: args.into() })), 0)
    }

    fn cint_lit(i: i64) -> Span<Box<Expr>> {
        (0, Box::new(Expr::Literal(Literal::Int(i))), 0)
    }

    fn cint_data(i: i64) -> ClamData {
        ClamData::Int(i.into())
    }

    #[test]
    fn eval_arith_expr() {
        use BinaryOperator::*;
        let ast = bin_op(Plus, cint_lit(1), cint_lit(2));
        // let heap = Arc::new(RefCell::new(Heap::default()));
        let heap = Arc::new(RefCell::new(NewHeap::default()));
        let (stdout, _stdout_recv) = channel::<u8>();
        let res = {
            let mut ctx = Ctx::new(heap, HashMap::new(), HashMap::new(), stdout, HashMap::new(), Vec::new());
            eval_expr(&ast, &mut ctx).unwrap()
        };

        assert_eq!(res, cint_data(3));
    }

    #[test]
    fn eval_print_expr() {
        let ast = fun_call("println", [bin_op(BinaryOperator::Plus, cint_lit(3), cint_lit(7))]);

        // let heap = Arc::new(RefCell::new(Heap::default()));
        let heap = Arc::new(RefCell::new(NewHeap::default()));

        let (stdout, stdout_recv) = channel::<u8>();
        let res = {
            let mut ctx = Ctx::new(heap, HashMap::new(), HashMap::new(), stdout, HashMap::new(), Vec::new());
            eval_expr(&ast, &mut ctx).unwrap()
        };

        assert_eq!(res, ClamData::Empty);
        let expected: Vec<u8> = "10\n".into();
        let actual: Vec<u8> = stdout_recv.into_iter().collect();
        assert_eq!(actual, expected);
    }
}
