use crate::ast::*;

pub trait AstVisitor<CtxType, ErrorType> {
    type OutputType;

    fn default() -> Result<Self::OutputType, ErrorType>;

    fn visit_primitive(p: &Primitive, ctx: CtxType) -> Result<Self::OutputType, ErrorType> {
        Self::default()
    }

    fn visit_literal(l: &Literal, ctx: CtxType) -> Result<Self::OutputType, ErrorType> {
        Self::default()
    }

    fn visit_identifier(i: &Identifier, ctx: CtxType) -> Result<Self::OutputType, ErrorType> {
        Self::default()
    }

    fn visit_fn_def(f: &FnDef, ctx: CtxType) -> Result<Self::OutputType, ErrorType> {
        Self::default()
    }

    fn visit_let(l: &Let, ctx: CtxType) -> Result<Self::OutputType, ErrorType> {
        Self::default()
    }

    fn visit_assign(a: &Assign, ctx: CtxType) -> Result<Self::OutputType, ErrorType> {
        Self::default()
    }

    fn visit_break(ctx: CtxType) -> Result<Self::OutputType, ErrorType> {
        Self::default()
    }

    fn visit_block(b: &Block, ctx: CtxType) -> Result<Self::OutputType, ErrorType> {
        Self::default()
    }

    fn visit_bin_op(b: &BinOp, ctx: CtxType) -> Result<Self::OutputType, ErrorType> {
        Self::default()
    }

    fn visit_un_op(u: &UnOp, ctx: CtxType) -> Result<Self::OutputType, ErrorType> {
        Self::default()
    }

    fn visit_fn_call(f: &FnCall, ctx: CtxType) -> Result<Self::OutputType, ErrorType> {
        Self::default()
    }

    fn visit_lambda_def(l: &LambdaDef, ctx: CtxType) -> Result<Self::OutputType, ErrorType> {
        Self::default()
    }

    fn visit_conditional(c: &Conditional, ctx: CtxType) -> Result<Self::OutputType, ErrorType> {
        Self::default()
    }

    fn visit_while_loop(w: &WhileLoop, ctx: CtxType) -> Result<Self::OutputType, ErrorType> {
        Self::default()
    }

    fn visit_for_loop(f: &ForLoop, ctx: CtxType) -> Result<Self::OutputType, ErrorType> {
        Self::default()
    }

    fn visit_statement(s: &Statement, ctx: CtxType) -> Result<Self::OutputType, ErrorType> {
        match s {
            Statement::FnDef(f)  => Self::visit_fn_def(&f, ctx),
            Statement::Let(l)    => Self::visit_let(&l, ctx),
            Statement::Assign(a) => Self::visit_assign(&a, ctx),
            Statement::Expr(e)   => Self::visit_expr(&e.1, ctx),
            Statement::Break     => Self::visit_break(ctx),
            Statement::Return(e) => Self::visit_expr(&e.1, ctx),
        }
    }

    fn visit_expr(e: &Expr, ctx: CtxType) -> Result<Self::OutputType, ErrorType> {
        match e {
            Expr::Literal(l) => Self::visit_literal(&l, ctx),
            Expr::BinOp(b) => Self::visit_bin_op(&b, ctx),
            Expr::UnOp(u) => Self::visit_un_op(&u, ctx),
            Expr::Identifier(i) => Self::visit_identifier(&i, ctx),
            Expr::FnCall(f) => Self::visit_fn_call(&f, ctx),
            Expr::LambdaDef(l) => Self::visit_lambda_def(&l, ctx),
            Expr::Block(b) => Self::visit_block(&b, ctx),
            Expr::Conditional(c) => Self::visit_conditional(&c, ctx),
            Expr::WhileLoop(w) => Self::visit_while_loop(&w, ctx),
            Expr::ForLoop(f) => Self::visit_for_loop(&f, ctx),
        }
    }
}
