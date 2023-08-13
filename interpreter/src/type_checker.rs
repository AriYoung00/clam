use std::any::Any;
use std::collections::HashMap;
use derive_more::Constructor;
use clam_common::ast::*;
use clam_common::ast_visitor::*;

#[derive(Constructor)]
struct TypeCheckerCtx {
    vars: HashMap<Identifier, Type>,
    fns: HashMap<Identifier, Type>,
}

impl TypeCheckerCtx {
    fn empty() -> TypeCheckerCtx {
        TypeCheckerCtx::new(HashMap::new(), HashMap::new())
    }
}

#[derive(Constructor)]
struct TypeCheckerOutput {
    t: Option<Type>,
    ctx: TypeCheckerCtx,
}

enum TypeCheckerErrorSource {
    ExpectedBooleanExpr,
    MismatchedTypes(Option<Type>, Option<Type>),
    UndefinedIdentifier,
}

#[derive(Constructor)]
struct TypeCheckerError {
    loc: (usize, usize),
    src: TypeCheckerErrorSource,
}

impl TypeCheckerError {
    fn from_span<T>(span: &Span<T>, src: TypeCheckerErrorSource) -> TypeCheckerError {
        TypeCheckerError::new(
            (span.0, span.2),
            src
        )
    }
}

pub struct TypeChecker {}

impl TypeChecker {
    /// Visit a control flow structure with a boolean condition, e.g. a conditional or a while loop
    fn visit_boolean_controller(cond: &Span<Box<Expr>>, body: &Block, ctx: TypeCheckerCtx)
                                -> Result<TypeCheckerOutput, TypeCheckerError> {
        let cond_out = Self::visit_expr(&cond.1, ctx)?;

        match cond_out.t {
            Some(Type::Primitive(Primitive::Bool)) => Self::visit_block(&body, cond_out.ctx),
            _ => Err(TypeCheckerError::from_span(&cond, TypeCheckerErrorSource::ExpectedBooleanExpr))
        }
    }
}

impl AstVisitor<TypeCheckerCtx, TypeCheckerError> for TypeChecker {
    type OutputType = TypeCheckerOutput;

    fn default() -> Result<Self::OutputType, TypeCheckerError> {
        Ok(TypeCheckerOutput::new(None, TypeCheckerCtx::empty()))
    }

    fn visit_primitive(p: &Primitive, ctx: TypeCheckerCtx)
        -> Result<Self::OutputType, TypeCheckerError> {
        Ok(TypeCheckerOutput::new(
            Some(Type::Primitive(p.clone())),
            ctx
        ))
    }

    fn visit_literal(l: &Literal, ctx: TypeCheckerCtx)
        -> Result<Self::OutputType, TypeCheckerError> {
        Ok(TypeCheckerOutput::new(
            Some(Type::Primitive(
                match l {
                    Literal::Int(_) => Primitive::Int,
                    Literal::Float(_) => Primitive::Float,
                    Literal::Bool(_) => Primitive::Bool,
                    Literal::String(_) => Primitive::String,
                    Literal::Command(_) => Primitive::Command,
                }
            )),
            ctx
        ))
    }

    fn visit_identifier(i: &Identifier, ctx: TypeCheckerCtx)
        -> Result<Self::OutputType, TypeCheckerError> {
        Ok(TypeCheckerOutput::new(
            ctx.vars.get(i).map(|t| t.clone()),
            ctx
        ))
    }

    fn visit_fn_def(f: &FnDef, mut ctx: TypeCheckerCtx)
        -> Result<Self::OutputType, TypeCheckerError> {
        // Add parameters to ctx
        ctx.vars.extend(
            f.param_list.iter().map(|(id, t)| (id.1.clone(), t.as_ref().unwrap().1.clone()))
        );

        let expr_out = Self::visit_expr(&f.body.1, ctx)?;
        let ret_type = f.ret_type.as_ref().map(|t| t.1.clone());

        // Check return type
        if ret_type.type_id() != expr_out.t.type_id() {
            return Err(TypeCheckerError::from_span(
                &f.body,
                TypeCheckerErrorSource::MismatchedTypes(ret_type, expr_out.t)
            ))
        }

        Ok(TypeCheckerOutput::new(
            Some(Type::Function(FunctionType::new(
                f.param_list.iter().map(|(_, t)| t.as_ref().unwrap().1.clone()).collect(),
                ret_type.map(|t| Box::new(t))
            ))),
            expr_out.ctx
        ))
    }

    fn visit_let(l: &Let, ctx: TypeCheckerCtx) -> Result<Self::OutputType, TypeCheckerError> {
        let id_type = l.r#type.as_ref().unwrap().1.clone();

        let mut new_ctx = match &l.expr {
            Some(expr) => {
                let expr_out = Self::visit_expr(&expr.1, ctx)?;

                if expr_out.t.is_none() || (id_type.type_id() != expr_out.t.as_ref().unwrap().type_id()) {
                    return Err(TypeCheckerError::from_span(
                        &expr,
                        TypeCheckerErrorSource::MismatchedTypes(Some(id_type.clone()), expr_out.t)
                    ))
                }

                expr_out.ctx
            },
            None => ctx
        };

        // Update context
        new_ctx.vars.insert(l.id.1.clone(), id_type);

        Ok(TypeCheckerOutput::new(None, new_ctx))
    }

    fn visit_assign(a: &Assign, ctx: TypeCheckerCtx) -> Result<Self::OutputType, TypeCheckerError> {
        let id_type_opt = ctx.vars.get(&a.id.1).map(|t| t.clone());
        let expr_out = Self::visit_expr(&a.expr.1, ctx)?;

        match (id_type_opt, expr_out.t) {
            (None, _) => Err(TypeCheckerError::from_span(
                &a.id,
                TypeCheckerErrorSource::UndefinedIdentifier)
            ),
            (Some(id_type), Some(expr_type)) if id_type.type_id() == expr_type.type_id() =>
                Ok(TypeCheckerOutput::new(None, expr_out.ctx)),
            (id_opt, expr_opt) => Err(TypeCheckerError::new(
                (a.id.0, a.expr.2),
                TypeCheckerErrorSource::MismatchedTypes(
                    id_opt,
                    expr_opt.map(|t| t.clone())
                )
            ))
        }
    }

    fn visit_block(b: &Block, ctx: TypeCheckerCtx) -> Result<Self::OutputType, TypeCheckerError> {
        todo!()
    }

    fn visit_bin_op(b: &BinOp, ctx: TypeCheckerCtx) -> Result<Self::OutputType, TypeCheckerError> {
        todo!()
    }

    fn visit_un_op(u: &UnOp, ctx: TypeCheckerCtx) -> Result<Self::OutputType, TypeCheckerError> {
        todo!()
    }

    fn visit_fn_call(f: &FnCall, ctx: TypeCheckerCtx) -> Result<Self::OutputType, TypeCheckerError> {
        todo!()
    }

    fn visit_lambda_def(l: &LambdaDef, ctx: TypeCheckerCtx) -> Result<Self::OutputType, TypeCheckerError> {
        todo!()
    }

    fn visit_conditional(c: &Conditional, ctx: TypeCheckerCtx)
        -> Result<Self::OutputType, TypeCheckerError> {
        TypeChecker::visit_boolean_controller(&c.cond, &c.body, ctx)
    }

    fn visit_while_loop(w: &WhileLoop, ctx: TypeCheckerCtx) -> Result<Self::OutputType, TypeCheckerError> {
        TypeChecker::visit_boolean_controller(&w.cond, &w.body, ctx)
    }

    fn visit_for_loop(f: &ForLoop, ctx: TypeCheckerCtx) -> Result<Self::OutputType, TypeCheckerError> {
        todo!()
    }
}
