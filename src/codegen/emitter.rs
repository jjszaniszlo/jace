use std::fmt::format;

use crate::parser::prelude::*;

use super::visitor::Visitor;


// emits lua code from the AST

pub struct LuaEmitter {
    indent: usize,
}

impl LuaEmitter {
    pub fn new() -> Self {
        Self { indent: 0 }
    }

    fn indent(&self) -> String {
        " ".repeat(self.indent * 4)
    }

    fn increase_indent(&mut self) {
        self.indent += 1;
    }

    fn decrease_indent(&mut self) {
        if self.indent > 0 {
            self.indent -= 1;
        }
    }
}

impl Visitor for LuaEmitter {
    fn visit_identifier(&mut self, identifier: &Identifier) -> String {
        identifier.0.clone()
    }

    fn visit_module(&mut self, module: &Module) -> String {
        let mut result = module.0.iter()
            .map(|def| def.accept(self))
            .collect::<Vec<_>>()
            .join("\n");
        // TODO: Differentiate between main module and other modules
        result.push_str("\nmain()");
        result
    }

    fn visit_def(&mut self, def: &Def) -> String {
        let mut result = String::new();
        match def {
            Def::FnDef(identifier, _, _, _, fn_expr, _) => {
                result.push_str(&format!("{}{} = ", self.indent(), identifier.accept(self)));
                self.increase_indent();
                result.push_str(&fn_expr.accept(self));
                self.decrease_indent();
                result.push_str(&format!("{}end\n", self.indent()));
            },
            Def::TypeDef(identifier, _, type_params, _) => {
                result.push_str(&format!("{}{} = {{}}\n", self.indent(), identifier.accept(self)));
                result.push_str(&format!("{}{}.__type = \"{}\"\n", self.indent(), identifier.accept(self), identifier.accept(self)));

                for type_param in type_params {
                    match type_param {
                        TypeParam::Type(variant_identifier, _) => {
                            let constructor_name = variant_identifier.accept(self);

                            // Start the constructor function
                            result.push_str(&format!(
                                "{}{}.{} = function()\n",
                                self.indent(),
                                identifier.accept(self),
                                constructor_name
                            ));
                            self.increase_indent();

                            // Create the Lua table for the constructor
                            result.push_str(&format!(
                                "{}local self = setmetatable({{}}, {})\n",
                                self.indent(),
                                identifier.accept(self)
                            ));
                            result.push_str(&format!("{}self.__tag = \"{}\"\n", self.indent(), constructor_name));

                            // Return the constructed table
                            result.push_str(&format!("{}return self\n", self.indent()));
                            self.decrease_indent();
                            result.push_str(&format!("{}end\n", self.indent()));
                        }

                        TypeParam::TypeConstructorType(variant_identifier, params, _) => {
                            let constructor_name = variant_identifier.accept(self);
                            let param_names = params
                                .iter()
                                .enumerate()
                                .map(|(i, _)| format!("param{}", i + 1))
                                .collect::<Vec<_>>()
                                .join(", ");

                            result.push_str(&format!(
                                "{}{}.{} = function({})\n",
                                self.indent(),
                                identifier.accept(self),
                                constructor_name,
                                param_names
                            ));
                            self.increase_indent();

                            result.push_str(&format!(
                                "{}local self = setmetatable({{}}, {})\n",
                                self.indent(),
                                identifier.accept(self)
                            ));
                            result.push_str(&format!("{}self.__tag = \"{}\"\n", self.indent(), constructor_name));

                            // Assign fields for each parameter
                            for (i, _) in params.iter().enumerate() {
                                result.push_str(&format!(
                                    "{}self._{} = param{}\n",
                                    self.indent(),
                                    i + 1,
                                    i + 1
                                ));
                            }

                            result.push_str(&format!("{}return self\n", self.indent()));
                            self.decrease_indent();
                            result.push_str(&format!("{}end\n", self.indent()));
                        }

                        _ => unimplemented!(),
                    }
                }
            },
            Def::ProcDef(identifier, stmts, _) => {
                result.push_str(&format!("{}{} = function()\n", self.indent(), identifier.accept(self)));
                self.increase_indent();
                for stmt in stmts {
                    result.push_str(&format!("{}\n", stmt.accept(self)));
                }
                self.decrease_indent();
                result.push_str(&format!("{}end\n", self.indent()));
            },
        }
        result
    }

    fn visit_type_constraint(&mut self, type_constraint: &TypeConstraint) -> String {
        todo!()
    }

    fn visit_type_param(&mut self, type_param: &TypeParam) -> String {
        todo!()
    }

    fn visit_fn_pattern_param(&mut self, fn_pattern_param: &FnPatternParam) -> String {
        match fn_pattern_param {
            FnPatternParam::BindToLiteralParam(literal, _) => todo!(),
            FnPatternParam::BindToIdentParam(identifier, _) => identifier.accept(self),
            FnPatternParam::BindToSetDeconstructParam(identifiers, _) => todo!(),
            FnPatternParam::BindToSetSelectorParam(identifier, identifier1, _) => todo!(),
            FnPatternParam::BindToTypeConstructorParam(identifier, fn_pattern_params, _) => todo!(),
        }
    }

    fn visit_method_def(&mut self, method_def: &MethodDef) -> String {
        todo!()
    }

    fn visit_method_operator(&mut self, method_operator: &MethodOperator) -> String {
        todo!()
    }

    fn visit_expr(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::IdentExpr(identifier, _) => identifier.accept(self),
            Expr::LitExpr(literal, _) => literal.accept(self),
            Expr::SetExpr(items, range) => todo!(),
            Expr::ArrayExpr(exprs, _) => todo!(),
            Expr::TupleExpr(exprs, _) => todo!(),
            Expr::UnaryOp(unary_operator, p, _) => {
                let inner = p.accept(self);
                format!("{}{}", unary_operator.accept(self), inner)
            },
            Expr::BinOpExpr(bin_operator, p, p1, _) => {
                let left = p.accept(self);
                let right = p1.accept(self);
                format!("{} {} {}", left, bin_operator.accept(self), right)
            },
            Expr::LetInExpr(stmts, p, _) => todo!(),
            Expr::FnExpr(p, _) => todo!(),
            Expr::FnCallExpr(identifier, exprs, _) => todo!(),
            Expr::CaseExpr(identifier, items, _) => todo!(),
            Expr::IfThenElseIfExpr(items, p, _) => todo!(),
            Expr::MemberExpr(member_expr, _) => todo!(),
        }
    }

    fn visit_member_expr(&mut self, member_expr: &MemberExpr) -> String {
        todo!()
    }

    fn visit_member_expr_base(&mut self, member_expr_base: &MemberExprBase) -> String {
        todo!()
    }

    fn visit_literal(&mut self, literal: &Literal) -> String {
        match literal {
            Literal::Integer(i, _) => format!("{}", i),
            Literal::Float(f, _) => format!("{}", f),
            Literal::String(s, _) => format!("\"{}\"", s),
            Literal::Bool(b, _) => format!("{}", b),
        }
    }

    fn visit_bin_operator(&mut self, bin_operator: &BinOperator) -> String {
        match bin_operator {
            BinOperator::Plus => "+".into(),
            BinOperator::Minus => "-".into(),
            BinOperator::Divide => "/".into(),
            BinOperator::Multiply => "*".into(),
            BinOperator::Exp => "^".into(),
            BinOperator::And => "and".into(),
            BinOperator::Or => "or".into(),
            BinOperator::GreaterEquals => ">=".into(),
            BinOperator::LessEquals => "<=".into(),
            BinOperator::Greater => ">".into(),
            BinOperator::Less => "<".into(),
            BinOperator::EqualsEquals => "==".into(),
            BinOperator::NotEquals => "~=".into(),
            BinOperator::AppendSet => todo!(),
        }
    }

    fn visit_unary_operator(&mut self, unary_operator: &UnaryOperator) -> String {
        match unary_operator {
            UnaryOperator::Negative => "-".into(),
            UnaryOperator::Not => "not".into(),
        }
    }

    fn visit_fn_expr(&mut self, fn_expr: &FnExpr) -> String {
        match fn_expr {
            FnExpr::FnExpr(fn_pattern_params, expr, _) => {
                let params = fn_pattern_params
                    .iter()
                    .map(|p| p.accept(self))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("function({})\n{}return {}\n", params, self.indent(), expr.accept(self))
            }
            FnExpr::CaseFnExpr(fn_exprs, _) => todo!(),
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> String {
        match stmt {
            Stmt::FnCallStmt(identifier, exprs, _) => {
                let args = exprs
                    .iter()
                    .map(|e| e.accept(self))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}{}({})", self.indent(), identifier.accept(self), args)
            }
            _ => unimplemented!(),
        }
    }
}