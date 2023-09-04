use std::collections::HashMap;
use derive_more::Constructor;
use clam_common::ast::*;
use clam_common::ast_visitor::*;

#[derive(Clone, Constructor, Debug)]
struct TypeCheckerCtx {
    vars: HashMap<Identifier, Type>,
    fns: HashMap<Identifier, Option<Type>>,
}

impl TypeCheckerCtx {
    fn empty() -> TypeCheckerCtx {
        TypeCheckerCtx::new(HashMap::new(), HashMap::new())
    }
}

#[derive(Constructor, Debug)]
struct TypeCheckerOutput {
    t: Option<Type>,
    ctx: TypeCheckerCtx,
}

#[derive(Debug)]
enum TypeCheckerErrorSource {
    ExpectedBooleanExpr(Option<Type>),
    ExpectedIntegerExpr(Option<Type>),
    ExpectedNumericalType(Option<Type>),
    MismatchedTypes(Option<Type>, Option<Type>),
}

#[derive(Constructor, Debug)]
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

        match cond_out.t.as_ref() {
            Some(Type::Primitive(Primitive::Bool(BoolType))) => Self::visit_block(&body, cond_out.ctx),
            _ => Err(TypeCheckerError::from_span(
                &cond,
                TypeCheckerErrorSource::ExpectedBooleanExpr(cond_out.t)
            ))
        }
    }

    /// Check if two types are equivalent
    fn try_type_eq(t1: Option<&Type>, t2: Option<&Type>) -> Result<Option<Type>, ()> {
        use Type::{Function, Name, Primitive, Sum};
        use clam_common::ast::Primitive::*;

        match (t1, t2) {
            (None, None) => Ok(None),
            (t1, t2) if Self::is_numeric(t1) && Self::is_numeric(t2) =>
                match (t1, t2)  {
                    (Some(Primitive(Float(FloatType))), _) | (_, Some(Primitive(Float(FloatType)))) =>
                        Ok(Some(Primitive(Float(FloatType)))),
                    _ => Ok(Some(Primitive(Int(IntType)))),
                },
            (Some(Primitive(p1)), Some(Primitive(p2)))
            if std::mem::discriminant(p1) == std::mem::discriminant(p2) => Ok(t1.cloned()),
            (Some(Name(n1)), Some(Name(n2))) if n1 == n2 => Ok(t1.cloned()),
            (Some(Sum(v1)), Some(Sum(v2))) if v1 == v2 => Ok(t1.cloned()),
            (Some(Type::Any(Any)), Some(Type::Any(Any))) => Ok(t1.cloned()),
            (Some(Function(f1)), Some(Function(f2)))
            if f1 == f2 => Ok(t1.cloned()),
            _ => Err(()),
        }
    }

    /// Check if a type is a numeric type
    fn is_numeric(from: Option<&Type>) -> bool {
        match from {
            Some(Type::Primitive(Primitive::Int(IntType)))
            | Some(Type::Primitive(Primitive::Float(FloatType)))
                => true,
            _ => false,
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
        use Type::Primitive;
        use clam_common::ast::Primitive::*;

        Ok(TypeCheckerOutput::new(
            Some(
                match l {
                    Literal::Int(_) => Primitive(Int(IntType)),
                    Literal::Float(_) => Primitive(Float(FloatType)),
                    Literal::Bool(_) => Primitive(Bool(BoolType)),
                    Literal::String(_) => Primitive(String(StringType)),
                    Literal::Command(_) => Primitive(Command(CommandType)),
                    Literal::Struct((_, id, _), _) => Type::Name(id.clone()),
                }
            ),
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
        // Update context
        ctx.vars.extend(
            f.param_list.iter().map(|(id, t)| (id.1.clone(), t.as_ref().unwrap().1.clone()))
        );

        let mut expr_out = Self::visit_expr(&f.body.1, ctx)?;
        let ret_type = f.ret_type.as_ref().map(|t| t.1.clone());

        // Check return type
        if Self::try_type_eq(ret_type.as_ref(), expr_out.t.as_ref()).is_err() {
            return Err(TypeCheckerError::from_span(
                &f.body,
                TypeCheckerErrorSource::MismatchedTypes(ret_type, expr_out.t)
            ))
        }

        // Update context
        expr_out.ctx.fns.insert(f.name.1.clone(), ret_type.clone());

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

                if Self::try_type_eq(Some(&id_type), expr_out.t.as_ref()).is_err() {
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

        match Self::try_type_eq(id_type_opt.as_ref(), expr_out.t.as_ref()) {
            Ok(t) => Ok(TypeCheckerOutput::new(t, expr_out.ctx)),
            Err(()) => Err(TypeCheckerError::new(
                (a.id.0, a.expr.2),
                TypeCheckerErrorSource::MismatchedTypes(
                    id_type_opt,
                    expr_out.t.map(|t| t.clone())
                )
            ))
        }
    }

    fn visit_block(b: &Block, ctx: TypeCheckerCtx) -> Result<Self::OutputType, TypeCheckerError> {
        let new_ctx = b.body.iter().try_fold(ctx.clone(), |ctx, (_, stmt, _)| {
            Self::visit_statement(stmt, ctx).map(|out| out.ctx)
        })?;

        match b.last.as_ref() {
            Some((_, stmt, _)) => {
                let out = Self::visit_statement(stmt, new_ctx)?;
                Ok(TypeCheckerOutput::new(out.t, ctx))
            },
            None => Ok(TypeCheckerOutput::new(None, ctx)),
        }
    }

    fn visit_bin_op(b: &BinOp, ctx: TypeCheckerCtx) -> Result<Self::OutputType, TypeCheckerError> {
        use Type::Primitive;
        use clam_common::ast::Primitive::{Bool, Float, Int};

        let lhs_out = Self::visit_expr(&b.lhs.1, ctx)?;
        let rhs_out = Self::visit_expr(&b.rhs.1, lhs_out.ctx)?;

        let lhs_type = lhs_out.t.as_ref();
        let rhs_type = rhs_out.t.as_ref();

        match &b.op {
            // Numerical operations that return numerical types
            BinaryOperator::Plus
            | BinaryOperator::Minus
            | BinaryOperator::Times
            | BinaryOperator::Div =>
                match (Self::is_numeric(lhs_type), Self::is_numeric(rhs_type)) {
                    (true, true) => Ok(TypeCheckerOutput::new(
                        Some(Primitive(
                            match (lhs_type, rhs_type) {
                                (Some(Primitive(Float(FloatType))), _)
                                | (_, Some(Primitive(Float(FloatType)))) =>
                                    Float(FloatType),
                                _ => Int(IntType),
                            }
                        )),
                        rhs_out.ctx
                    )),
                    (true, _) => Err(TypeCheckerError::from_span(
                        &b.rhs,
                        TypeCheckerErrorSource::ExpectedNumericalType(rhs_out.t)
                    )),
                    _ => Err(TypeCheckerError::from_span(
                        &b.lhs,
                        TypeCheckerErrorSource::ExpectedNumericalType(lhs_out.t)
                    )),
                },

            // Numerical operations that return boolean type
            BinaryOperator::Lt
            | BinaryOperator::Lte
            | BinaryOperator::Gt
            | BinaryOperator::Gte => match (Self::is_numeric(lhs_type), Self::is_numeric(rhs_type)) {
                (true, true) =>
                    Ok(TypeCheckerOutput::new(Some(Primitive(Bool(BoolType))), rhs_out.ctx)),
                (true, _) => Err(TypeCheckerError::from_span(
                    &b.rhs,
                    TypeCheckerErrorSource::ExpectedNumericalType(rhs_out.t)
                )),
                _ => Err(TypeCheckerError::from_span(
                    &b.lhs,
                    TypeCheckerErrorSource::ExpectedNumericalType(lhs_out.t)
                )),
            }

            // Integer operations
            BinaryOperator::Mod
            | BinaryOperator::BitAnd
            | BinaryOperator::BitOr
            | BinaryOperator::BitXor => match (lhs_type, rhs_type) {
                (Some(Primitive(Int(IntType))), Some(Primitive(Int(IntType)))) =>
                    Ok(TypeCheckerOutput::new(Some(Primitive(Int(IntType))), rhs_out.ctx)),
                (Some(Primitive(Int(IntType))), _) => Err(TypeCheckerError::from_span(
                    &b.rhs,
                    TypeCheckerErrorSource::ExpectedNumericalType(rhs_out.t)
                )),
                _ => Err(TypeCheckerError::from_span(
                    &b.lhs,
                    TypeCheckerErrorSource::ExpectedNumericalType(lhs_out.t)
                )),
            },

            // Boolean operations
            BinaryOperator::And | BinaryOperator::Or => match (lhs_type, rhs_type) {
                (Some(Primitive(Bool(BoolType))), Some(Primitive(Bool(BoolType)))) =>
                    Ok(TypeCheckerOutput::new(Some(Primitive(Bool(BoolType))), rhs_out.ctx)),
                (Some(Primitive(Bool(BoolType))), _) => Err(TypeCheckerError::from_span(
                    &b.rhs,
                    TypeCheckerErrorSource::ExpectedNumericalType(rhs_out.t)
                )),
                _ => Err(TypeCheckerError::from_span(
                    &b.lhs,
                    TypeCheckerErrorSource::ExpectedNumericalType(lhs_out.t)
                )),
            },

            // Equality operator
            BinaryOperator::Eq | BinaryOperator::Ne =>
                match Self::try_type_eq(lhs_type, rhs_type) {
                    Ok(t) => Ok(TypeCheckerOutput::new(t, rhs_out.ctx)),
                    Err(()) => Err(TypeCheckerError::new(
                        (b.rhs.0, b.lhs.2),
                        TypeCheckerErrorSource::MismatchedTypes(lhs_out.t, rhs_out.t))
                    )
                },
        }
    }

    fn visit_un_op(u: &UnOp, ctx: TypeCheckerCtx) -> Result<Self::OutputType, TypeCheckerError> {
        use Type::Primitive;
        use clam_common::ast::Primitive::{Bool, Int};

        let out = Self::visit_expr(&u.rhs.1, ctx)?;
        let out_type = out.t.as_ref();

        match u.op {
            UnaryOperator::Not => match out_type {
                Some(Primitive(Bool(BoolType))) =>
                    Ok(TypeCheckerOutput::new(Some(Primitive(Bool(BoolType))), out.ctx)),
                _ => Err(TypeCheckerError::from_span(
                    &u.rhs,
                    TypeCheckerErrorSource::ExpectedBooleanExpr(out_type.cloned())
                )),
            },

            UnaryOperator::BitInvert => match out_type {
                Some(Primitive(Int(IntType))) =>
                    Ok(TypeCheckerOutput::new(Some(Primitive(Int(IntType))), out.ctx)),
                _ => Err(TypeCheckerError::from_span(
                    &u.rhs,
                    TypeCheckerErrorSource::ExpectedIntegerExpr(out_type.cloned())
                )),
            },

            UnaryOperator::Negate =>
                if Self::is_numeric(out_type) {
                    Ok(TypeCheckerOutput::new(out_type.cloned(), out.ctx))
                }
                else {
                    Err(TypeCheckerError::from_span(
                        &u.rhs,
                        TypeCheckerErrorSource::ExpectedNumericalType(out_type.cloned())
                    ))
                }
        }
    }

    fn visit_fn_call(f: &FnCall, ctx: TypeCheckerCtx) -> Result<Self::OutputType, TypeCheckerError> {
        // TODO after structs are added
        Ok(TypeCheckerOutput::new(Some(Type::Any(Any)), ctx))
    }

    fn visit_lambda_def(l: &LambdaDef, ctx: TypeCheckerCtx) -> Result<Self::OutputType, TypeCheckerError> {
        let mut lambda_ctx = ctx.clone();
        lambda_ctx.vars.extend(
            l.params.iter().map(|((_, id, _), t)| (id.clone(), t.as_ref().unwrap().1.clone()))
        );

        let out = Self::visit_expr(&l.body.1, lambda_ctx)?;
        Ok(TypeCheckerOutput::new(
            Some(Type::Function(FunctionType::new(
                l.params.iter().map(|(_, t)| t.as_ref().unwrap().1.clone()).collect(),
                out.t.map(|t| Box::new(t))
            ))),
            ctx
        ))
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

#[cfg(test)]
mod test {
    use ordered_float::OrderedFloat;
    use clam_common::ast::*;
    use clam_common::ast_visitor::AstVisitor;
    use crate::data::ClamData::String;
    use crate::type_checker::*;

    fn type_check_expr(ast: &Expr) -> Result<TypeCheckerOutput, TypeCheckerError> {
        TypeChecker::visit_expr(ast, TypeCheckerCtx::empty())
    }

    #[test]
    fn test_literal() {
        use Literal::*;

        let ast = Expr::Literal(Int(0));
        let out = type_check_expr(&ast);
        assert_eq!(out.unwrap().t, Some(Type::Primitive(Primitive::Int(IntType))));

        let ast = Expr::Literal(Float(OrderedFloat::from(0.0)));
        let out = type_check_expr(&ast);
        assert_eq!(out.unwrap().t, Some(Type::Primitive(Primitive::Float(FloatType))));

        let ast = Expr::Literal(Bool(false));
        let out = type_check_expr(&ast);
        assert_eq!(out.unwrap().t, Some(Type::Primitive(Primitive::Bool(BoolType))));

        let ast = Expr::Literal(String("hello world".to_string()));
        let out = type_check_expr(&ast);
        assert_eq!(out.unwrap().t, Some(Type::Primitive(Primitive::String(StringType))));

        let ast = Expr::Literal(Command("hello world".to_string()));
        let out = type_check_expr(&ast);
        assert_eq!(out.unwrap().t, Some(Type::Primitive(Primitive::Command(CommandType))));

        // TODO: struct literal
    }

    #[test]
    fn test_bin_op_plus() {
        let ast = Expr::BinOp(BinOp::new(
            BinaryOperator::Plus,
            (0, Box::new(Expr::Literal(Literal::Int(1))), 1),
            (2, Box::new(Expr::Literal(Literal::Int(1))), 3)
        ));
        let out = type_check_expr(&ast);
        assert_eq!(out.unwrap().t, Some(Type::Primitive(Primitive::Int(IntType))));
    }
}