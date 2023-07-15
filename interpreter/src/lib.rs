mod data;

use std::collections::HashMap;

use clam_common::ast::{Identifier, Expr, Block};
use data::{ClamData, ClamRuntimeError};



type Result<T> = std::result::Result<T, ClamRuntimeError>;
type Ref<T> = std::rc::Rc<T>;
#[allow(dead_code)]
type Weak<T> = std::rc::Weak<T>;

struct Ctx {
    vars: HashMap<Identifier, Ref<ClamData>>,
}

fn eval_expr(exp: Box<Expr>, ctx: &mut Ctx) -> Result<Ref<ClamData>> {
    match *exp {
        Expr::Literal(lit) => Ok(Ref::new(match lit {
            clam_common::ast::Literal::Int(i)      => ClamData::Int(i.into()),
            clam_common::ast::Literal::Float(f)    => ClamData::Float(f.into()),
            clam_common::ast::Literal::Bool(b)     => ClamData::Bool(b.into()),
            clam_common::ast::Literal::String(s)   => ClamData::String(s.into()),
            clam_common::ast::Literal::Command(_c) => todo!("Evaluate commands"),
        })),

        Expr::BinOp(op, lhs, rhs) => {
            let lhs = eval_expr(lhs, ctx)?;
            let rhs = eval_expr(rhs, ctx)?;

            Ok(Ref::new(match op {
                clam_common::ast::BinaryOperator::Plus => (lhs.as_ref() + rhs.as_ref())?,
                clam_common::ast::BinaryOperator::Minus => todo!(),
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

        Expr::UnOp(op, rhs) => Ok(Ref::new(match op {
            clam_common::ast::UnaryOperator::Not => todo!(),
            clam_common::ast::UnaryOperator::BitInvert => todo!(),
            clam_common::ast::UnaryOperator::Negate => todo!(),
        })),

        Expr::Identifier(i) => Ok(match ctx.vars.get(&i) {
            Some(val) => val.clone(),
            None => todo!(),
        }),
        
        Expr::FnCall(_, _) => todo!(),

        Expr::LambdaDef(_, _) => todo!(),

        Expr::Block(_) => todo!(),

        Expr::Conditional(_) => todo!(),

        Expr::WhileLoop(_) => todo!(),

        Expr::ForLoop(_) => todo!(),
    }
}

fn eval_block(block: Block) -> Result<Ref<ClamData>> {
    todo!()
}
