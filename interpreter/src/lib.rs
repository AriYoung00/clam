#![feature(if_let_guard)]

mod data;
mod type_checker;

use std::collections::HashMap;

use clam_common::ast::{Identifier, Expr, Block, Span, UnOp, BinOp, FnCall, FnDef};
use data::{ClamData, ClamRuntimeError, ClamString, GcRef, Heap, ErrorSource::*};
use im::{hashmap, ordmap};



type Result<T> = std::result::Result<T, ClamRuntimeError>;

struct Ctx<'a> {
    heap: &'a mut Heap<ClamData>,
    vars: im::HashMap<Identifier, GcRef<ClamData>>,
    funcs: &'a HashMap<Identifier, FnDef>,
}

impl Ctx<'_> {
    pub fn to_heap(&mut self, data: ClamData) -> GcRef<ClamData> {
        self.heap.move_to_heap(data)
    }
}

#[allow(dead_code)]
fn eval_expr<'b>(exp: &Span<Box<Expr>>, ctx: &mut Ctx<'b>) -> Result<GcRef<ClamData>> {
    let (start, exp, end) = exp;
    match exp.as_ref() {
        Expr::Literal(lit) => Ok(ctx.to_heap(match lit {
            clam_common::ast::Literal::Int(i)      => ClamData::Int((*i).into()),
            clam_common::ast::Literal::Float(f)    => ClamData::Float((*f).into()),
            clam_common::ast::Literal::Bool(b)     => ClamData::Bool((*b).into()),
            clam_common::ast::Literal::String(s)   => ClamData::String(ClamString((s.clone()).into())),
            clam_common::ast::Literal::Command(_c) => todo!("Evaluate commands"),
        })),

        Expr::BinOp(BinOp{ op, lhs, rhs }) => {
            let lhs = eval_expr(&lhs, ctx)?;
            let rhs = eval_expr(&rhs, ctx)?;

            Ok(ctx.to_heap(match op {
                clam_common::ast::BinaryOperator::Plus => lhs.as_ref() + rhs.as_ref(),
                clam_common::ast::BinaryOperator::Minus => lhs.as_ref() - rhs.as_ref(),
                clam_common::ast::BinaryOperator::Times => todo!(),
                clam_common::ast::BinaryOperator::Div => todo!(),
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
            }))
        },

        Expr::UnOp(UnOp{ op , rhs }) => {
            let res = eval_expr(&rhs, ctx)?;
            Ok(ctx.to_heap(match op {
                clam_common::ast::UnaryOperator::Not => !res.as_ref(),
                clam_common::ast::UnaryOperator::BitInvert => todo!(),
                clam_common::ast::UnaryOperator::Negate => todo!(),
            }))
        },

        Expr::Identifier(i) => match ctx.vars.get(&i) {
            Some(val) => Ok(*val),
            None => VariableNotFound.with_loc(*start, *end),
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

            let mut new_ctx = Ctx{ heap: ctx.heap, vars: new_vars, funcs: ctx.funcs };
            let res = eval_expr(&def.body, &mut new_ctx);

            todo!()
        },

        Expr::LambdaDef(_) => todo!(),

        Expr::Block(_) => todo!(),

        Expr::Conditional(_) => todo!(),

        Expr::WhileLoop(_) => todo!(),

        Expr::ForLoop(_) => todo!(),
    }
}

fn eval_block(block: Block) -> Result<GcRef<ClamData>> {
    todo!()
}
