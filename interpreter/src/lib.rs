#![feature(if_let_guard)]

mod data;
mod type_checker;

use clam_common::ast::{Identifier, Expr, Block, Span, UnOp, BinOp, FnCall, FnDef, Statement};
use data::{ClamData, ClamRuntimeError, ClamString, GcRef, Heap, ErrorSource::*, ClamUserType};
use derive_more::Constructor;
use im_rc::HashMap;



type Result<T> = std::result::Result<T, ClamRuntimeError>;

trait ErrSpan {
    fn add_loc(self, start: usize, end: usize) -> Self;
    fn likely_has_loc(&self) -> bool;

    fn map_no_loc(self, start: usize, end: usize) -> Self
    where Self: Sized {
        if self.likely_has_loc() {
            self
        }
        else {
            self.add_loc(start, end)
        }
    }
}

impl<T> ErrSpan for Result<T> {
    fn add_loc(self, start: usize, end: usize) -> Self {
        self.map_err(|ClamRuntimeError{ src, .. }| ClamRuntimeError{
            loc: (start, end),
            src,
        })
    }

    fn likely_has_loc(&self) -> bool {
        match self {
            Ok(_) => true,
            Err(e) => e.loc == (0, 0),
        }
    }
}

#[derive(Constructor)]
struct Ctx<'a> {
    heap: &'a mut Heap<ClamUserType>,
    vars: HashMap<Identifier, ClamData>,
    funcs: HashMap<Identifier, FnDef>,
}

impl Ctx<'_> {
    pub fn to_heap(&mut self, data: ClamUserType) -> GcRef<ClamUserType> {
        self.heap.move_to_heap(data)
    }
}

#[allow(dead_code)]
fn eval_expr<'b>(exp: &Span<Box<Expr>>, ctx: &mut Ctx<'b>) -> Result<ClamData> {
    let (start, exp, end) = exp;
    let (start, end) = (*start, *end);

    match exp.as_ref() {
        Expr::Literal(lit) => Ok(match lit {
            clam_common::ast::Literal::Int(i)      => ClamData::Int((*i).into()),
            clam_common::ast::Literal::Float(f)    => ClamData::Float((*f).into()),
            clam_common::ast::Literal::Bool(b)     => ClamData::Bool((*b).into()),
            clam_common::ast::Literal::String(s)   => ClamData::String(ClamString((s.clone()).into())),
            clam_common::ast::Literal::Command(_c) => todo!("Evaluate commands"),
        }),

        Expr::BinOp(BinOp{ op, lhs, rhs }) => {
            let lhs = eval_expr(&lhs, ctx)?;
            let rhs = eval_expr(&rhs, ctx)?;

            Ok(match op {
                clam_common::ast::BinaryOperator::Plus => lhs + rhs,
                clam_common::ast::BinaryOperator::Minus => lhs - rhs,
                clam_common::ast::BinaryOperator::Times => lhs * rhs,
                clam_common::ast::BinaryOperator::Div => (lhs / rhs).add_loc(start, end)?,
                clam_common::ast::BinaryOperator::Mod => todo!(),
                clam_common::ast::BinaryOperator::Lt => todo!(),
                clam_common::ast::BinaryOperator::Lte => todo!(),
                clam_common::ast::BinaryOperator::Gt => todo!(),
                clam_common::ast::BinaryOperator::Gte => todo!(),
                clam_common::ast::BinaryOperator::Eq => todo!(),
                clam_common::ast::BinaryOperator::Ne => todo!(),
                clam_common::ast::BinaryOperator::And => todo!(),
                clam_common::ast::BinaryOperator::Or => todo!(),
                clam_common::ast::BinaryOperator::BitAnd => todo!(),
                clam_common::ast::BinaryOperator::BitOr => todo!(),
            })
        },

        Expr::UnOp(UnOp{ op , rhs }) => {
            let res = eval_expr(&rhs, ctx)?;
            Ok(match op {
                clam_common::ast::UnaryOperator::Not => !res,
                clam_common::ast::UnaryOperator::BitInvert => todo!(),
                clam_common::ast::UnaryOperator::Negate => todo!(),
            })
        },

        Expr::Identifier(i) => match ctx.vars.get(&i) {
            Some(val) => Ok(val.clone()),
            None => VariableNotFound.with_loc(start, end),
        },
        
        Expr::FnCall(FnCall{ id, args }) => {
            let (start, id, end) = id;

            let arg_vals: Vec<_> = args.into_iter()
                .map(|exp| eval_expr(exp, ctx))
                .collect::<Result<_>>()?;

            let Some(def) = ctx.funcs.get(&id) else {
                return FunctionNotFound.with_loc(*start, *end);
            };

            let mappings = def.param_list.iter()
                .map(|((_, id, _), _)| id.clone())
                .zip(arg_vals.into_iter());
            let mut new_vars = ctx.vars.clone();
            new_vars.extend(mappings);

            let mut new_ctx = Ctx{ heap: ctx.heap, vars: new_vars, funcs: ctx.funcs.clone() };
            
            eval_expr(&def.body, &mut new_ctx)
        },

        Expr::LambdaDef(_) => todo!(),

        Expr::Block(block) => {
            let new_ctx = Ctx{ heap: ctx.heap, vars: ctx.vars.clone(), funcs: ctx.funcs.clone() };
            eval_block(block, new_ctx)
        },

        Expr::Conditional(_) => todo!(),

        Expr::WhileLoop(_) => todo!(),

        Expr::ForLoop(_) => todo!(),
    }
}

fn eval_block(block: &Block, mut ctx: Ctx) -> Result<ClamData> {
    for (start, line, end) in &block.body {
        use Statement::Return;
        let _ = match line {
            Return(expr) => return eval_expr(&expr, &mut ctx),
            other => eval_statement(&other, &mut ctx),
        }.map_no_loc(*start, *end)?;
    }

    if let Some((start, last, end)) = &block.last {
        eval_statement(&last, &mut ctx)
            .map_no_loc(*start, *end)
    }
    else {
        Ok(ClamData::Empty)
    }
}

fn eval_statement(stmt: &Statement, ctx: &mut Ctx) -> Result<ClamData> {
    todo!()
}


#[cfg(test)]
mod test {
    use std::hash::Hash;

    use clam_common::ast::*;
    use im_rc::HashMap;
    use crate::{data::*, eval_expr};
    use super::*;

    fn bin_op(op: BinaryOperator, lhs: Span<Box<Expr>>, rhs: Span<Box<Expr>>) -> Span<Box<Expr>> {
        (0, Box::new(Expr::BinOp(BinOp { op, lhs, rhs })), 0)
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
        let mut heap = Heap::default();
        let mut ctx = Ctx::new(&mut heap, HashMap::new(), HashMap::new());

        let res = eval_expr(&ast, &mut ctx).unwrap();
        assert_eq!(res, cint_data(3));
    }

}
