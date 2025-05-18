use crate::parser::prelude::*;

pub trait Visitor {
    fn visit_identifier(&mut self, identifier: &Identifier) -> String;
    fn visit_module(&mut self, module: &Module) -> String;
    fn visit_def(&mut self, def: &Def) -> String;
    fn visit_fn_pattern_param(&mut self, fn_pattern_param: &FnPatternParam) -> String;
    fn visit_expr(&mut self, expr: &Expr) -> String;
    fn visit_literal(&mut self, literal: &Literal) -> String;
    fn visit_bin_operator(&mut self, bin_operator: &BinOperator) -> String;
    fn visit_unary_operator(&mut self, unary_operator: &UnaryOperator) -> String;
    fn visit_fn_expr(&mut self, fn_expr: &FnExpr) -> String;
    fn visit_stmt(&mut self, stmt: &Stmt) -> String;
}

impl Identifier {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> String {
        visitor.visit_identifier(self)
    }
}

impl Module {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> String {
        visitor.visit_module(self)
    }
}

impl Def {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> String {
        visitor.visit_def(self)
    }
}

impl FnPatternParam {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> String {
        visitor.visit_fn_pattern_param(self)
    }
}

impl Expr {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> String {
        visitor.visit_expr(self)
    }
}

impl Literal {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> String {
        visitor.visit_literal(self)
    }
}

impl BinOperator {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> String {
        visitor.visit_bin_operator(self)
    }
}

impl UnaryOperator {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> String {
        visitor.visit_unary_operator(self)
    }
}

impl FnExpr {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> String {
        visitor.visit_fn_expr(self)
    }
}

impl Stmt {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> String {
        visitor.visit_stmt(self)
    }
}