use crate::parser::prelude::*;

use super::visitor::Visitor;


// emits lua code from the AST

pub struct LuaEmitter {
    indent: usize,
    type_constructor_map: std::collections::HashMap<String, String>,
}

impl LuaEmitter {
    pub fn new() -> Self {
        Self {
            indent: 0,
            type_constructor_map: std::collections::HashMap::new()
        }
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
                result.push_str(&fn_expr.accept(self));
            },
            Def::TypeDef(identifier, _, type_params, _) => {
                let ident = identifier.accept(self);
                result.push_str(&format!("{}{} = {{}}\n", self.indent(), identifier.accept(self)));
                result.push_str(&format!("{}{}.__type = \"{}\"\n", self.indent(), identifier.accept(self), identifier.accept(self)));

                for type_param in type_params {
                    match type_param {
                        TypeParam::Type(variant_identifier, _) => {
                            let constructor_name = variant_identifier.accept(self);
                            self.type_constructor_map.insert(constructor_name.clone(), ident.clone());

                            result.push_str(&format!(
                                "{}{}.{} = function()\n",
                                self.indent(),
                                identifier.accept(self),
                                constructor_name
                            ));
                            self.increase_indent();

                            result.push_str(&format!(
                                "{}local self = setmetatable({{}}, {})\n",
                                self.indent(),
                                identifier.accept(self)
                            ));
                            result.push_str(&format!("{}self.__tag = \"{}\"\n", self.indent(), constructor_name));

                            result.push_str(&format!("{}return self\n", self.indent()));
                            self.decrease_indent();
                            result.push_str(&format!("{}end\n", self.indent()));
                        }

                        TypeParam::TypeConstructorType(variant_identifier, params, _) => {
                            let constructor_name = variant_identifier.accept(self);
                            self.type_constructor_map.insert(constructor_name.clone(), ident.clone());

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

    fn visit_fn_pattern_param(&mut self, fn_pattern_param: &FnPatternParam) -> String {
        match fn_pattern_param {
            FnPatternParam::BindToLiteralParam(literal, _) => todo!(),
            FnPatternParam::BindToIdentParam(identifier, _) => identifier.accept(self),
            FnPatternParam::BindToSetDeconstructParam(identifiers, _) => todo!(),
            FnPatternParam::BindToSetSelectorParam(identifier, identifier1, _) => todo!(),
            FnPatternParam::BindToTypeConstructorParam(identifier, fn_pattern_params, _) => todo!(),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::IdentExpr(identifier, _) => {
                let ident = identifier.accept(self);
                let path = match self.type_constructor_map.get(&identifier.0) {
                    Some(path) => {
                        format!("{}.{}()", path, ident.clone())
                    },
                    None => ident.clone(),
                };
                path
            },
            Expr::LitExpr(literal, _) => literal.accept(self),
            Expr::SetExpr(items, range) => {
                let items = items
                    .iter()
                    .map(|(i, e)| {
                        let ident = i.accept(self);
                        let expr = e.accept(self);
                        format!("{} = {}", ident, expr)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{{}}}", items)
            },
            Expr::ArrayExpr(exprs, _) => {
                let items = exprs
                    .iter()
                    .map(|e| e.accept(self))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{{}}}", items) 
            },
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
            Expr::LetInExpr(stmts, expr, _) => {
                let stmts = stmts
                    .iter()
                    .map(|s| s.accept(self))
                    .collect::<Vec<_>>()
                    .join("\n");
                let mut result = String::new();
                // wrap the statements in a function and call it
                result.push_str(&format!("{}(function()\n", self.indent()));
                self.increase_indent();
                result.push_str(&format!("{}\n", stmts));
                result.push_str(&format!("{}return {}\n", self.indent(), expr.accept(self)));
                self.decrease_indent();
                result.push_str(&format!("{}end)()", self.indent()));
                result
            },
            Expr::FnExpr(fnexpr, _) => fnexpr.accept(self),
            Expr::FnCallExpr(identifier, exprs, _) => {
                let ident = identifier.accept(self);
                let path = match self.type_constructor_map.get(&identifier.0) {
                    Some(path) => {
                        format!("{}.{}", path, ident.clone())
                    },
                    None => ident.clone(),
                };
                let args = exprs
                    .iter()
                    .map(|e| e.accept(self))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", path, args)
            },
            Expr::CaseExpr(identifier, items, _) => todo!(),
            Expr::IfThenElseIfExpr(items, p, _) => {
                // wrap if then elseif else in a function and call it.
                let mut result = String::new();
                result.push_str(&format!("(function()\n"));
                self.increase_indent();
                for (i, item) in items.iter().enumerate() {
                    if i == 0 {
                        result.push_str(&format!("{}if ", self.indent()));
                    } else {
                        result.push_str(&format!("{}elseif ", self.indent()));
                    }
                    result.push_str(&item.0.accept(self));
                    result.push_str(" then\n");
                    self.increase_indent();
                    result.push_str(&format!("{}return {}\n", self.indent(), item.1.accept(self)));
                    self.decrease_indent();
                }
                result.push_str(&format!("{}else\n", self.indent()));
                self.increase_indent();
                result.push_str(&format!("{}return {}\n", self.indent(), p.accept(self)));
                self.decrease_indent();
                result.push_str(&format!("{}end\n", self.indent()));
                self.decrease_indent();
                result.push_str(&format!("{}end)()", self.indent()));
                result
            },
            Expr::MemberExpr(member_expr, _) => todo!(),
        }
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
                format!(
                    "function({})\n{}return {}\nend",
                    params,
                    self.indent(),
                    expr.accept(self)
                )
            }
            FnExpr::CaseFnExpr(branches, _) => {
                let mut result = String::new();

                let param_count = if let Some(FnExpr::FnExpr(params, _, _)) = branches.first() {
                    params.len()
                } else {
                    0
                };

                let outer_params = (1..=param_count)
                    .map(|i| format!("_{}", i))
                    .collect::<Vec<_>>()
                    .join(", ");

                result.push_str(&format!("function({})\n", outer_params));
                self.increase_indent();

                for (i, branch) in branches.iter().enumerate() {
                    if let FnExpr::FnExpr(params, body, _) = branch {
                        let branch_name = format!("_b{}", i + 1);
                        result.push_str(&format!("{}local function {}(_1, _2)\n", self.indent(), branch_name));

                        self.increase_indent();

                        for (j, param) in params.iter().enumerate() {
                            match param {
                                FnPatternParam::BindToTypeConstructorParam(_, inner_params, _) => {
                                    for (k, inner_param) in inner_params.iter().enumerate() {
                                        if let FnPatternParam::BindToIdentParam(ident, _) = inner_param {
                                            result.push_str(&format!(
                                                "{}local {} = _{}._{}\n",
                                                self.indent(),
                                                ident.accept(self),
                                                j + 1,
                                                k + 1
                                            ));
                                        }
                                    }
                                }
                                FnPatternParam::BindToIdentParam(ident, _) => {
                                    result.push_str(&format!(
                                        "{}local {} = _{}\n",
                                        self.indent(),
                                        ident.accept(self),
                                        j + 1
                                    ));
                                }
                                _ => {}
                            }
                        }

                        result.push_str(&format!(
                            "{}return {}\n",
                            self.indent(),
                            body.accept(self)
                        ));

                        self.decrease_indent();
                        result.push_str(&format!("{}end\n", self.indent()));
                    }
                }

                result.push_str(&format!("{}-- Case logic\n", self.indent()));
                for (i, branch) in branches.iter().enumerate() {
                    if let FnExpr::FnExpr(params, _, _) = branch {
                        let branch_name = format!("_b{}", i + 1);

                        if i > 0 {
                            result.push_str(&format!("{}elseif ", self.indent()));
                        } else {
                            result.push_str(&format!("{}if ", self.indent()));
                        }

                        let conditions = params
                            .iter()
                            .enumerate()
                            .map(|(j, param)| match param {
                                FnPatternParam::BindToTypeConstructorParam(ident, inner_params, _) => {
                                    let tag_check = format!("_{}.{} == \"{}\"", j + 1, "__tag", ident.0);
                                    let field_checks = inner_params
                                        .iter()
                                        .enumerate()
                                        .map(|(k, inner_param)| match inner_param {
                                            FnPatternParam::BindToLiteralParam(literal, _) => {
                                                format!("_{}._{} == {}", j + 1, k + 1, literal.accept(self))
                                            }
                                            _ => "true".to_string(),
                                        })
                                        .collect::<Vec<_>>()
                                        .join(" and ");
                                    if field_checks.is_empty() {
                                        tag_check
                                    } else {
                                        format!("{} and {}", tag_check, field_checks)
                                    }
                                }
                                FnPatternParam::BindToLiteralParam(literal, _) => {
                                    format!("_{} == {}", j + 1, literal.accept(self))
                                }
                                _ => "true".to_string(),
                            })
                            .collect::<Vec<_>>()
                            .join(" and ");
                        result.push_str(&conditions);
                        result.push_str(" then\n");

                        self.increase_indent();

                        result.push_str(&format!("{}return {}(_1, _2)\n", self.indent(), branch_name));

                        self.decrease_indent();
                    }
                }

                result.push_str(&format!("{}end\n", self.indent()));

                self.decrease_indent();
                result.push_str(&format!("{}end\n", self.indent()));

                result
            }
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
            Stmt::Empty(range) => todo!(),
            Stmt::AssignStmt(identifier, identifier1, expr, range) => {
                format!("{}{} = {}", self.indent(), identifier.accept(self), expr.accept(self))
            },
            Stmt::ProcCallStmt(identifier, range) => {
                format!("{}{}()", self.indent(), identifier.accept(self))
            },
            Stmt::CaseStmt(expr, items, range) => todo!(),
            Stmt::MultiAssignStmt(identifiers, identifiers1, expr, range) => todo!(),
            Stmt::SetDeconstructAssignStmt(identifiers, expr, range) => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::*;
    use crate::parser::prelude::*;
    use crate::lexer::token::{Token, TokenKind};
    use crate::parser::ptr::P;
    use crate::parser::tokenstream::TokenStream;

    fn create_ident(name: &str) -> Identifier {
        Identifier(name.to_string(), 0..name.len())
    }

    fn create_integer(value: i64) -> Literal {
        Literal::Integer(value.try_into().unwrap(), 0..value.to_string().len())
    }

    fn create_string(value: &str) -> Literal {
        Literal::String(value.to_string(), 0..value.len())
    }

    fn create_fn_expr() -> FnExpr {
        FnExpr::FnExpr(
            vec![FnPatternParam::BindToIdentParam(create_ident("x"), 0..1)],
            *Box::new(Expr::IdentExpr(create_ident("x"), 0..1)),
            0..1,
        )
    }

    #[test]
    fn test_visit_identifier() {
        let mut emitter = LuaEmitter::new();
        let identifier = create_ident("test");
        let result = emitter.visit_identifier(&identifier);
        assert_eq!(result, "test");
    }

    #[test]
    fn test_visit_literal_integer() {
        let mut emitter = LuaEmitter::new();
        let literal = create_integer(42);
        let result = emitter.visit_literal(&literal);
        assert_eq!(result, "42");
    }

    #[test]
    fn test_visit_literal_string() {
        let mut emitter = LuaEmitter::new();
        let literal = create_string("hello");
        let result = emitter.visit_literal(&literal);
        assert_eq!(result, "\"hello\"");
    }

    #[test]
    fn test_visit_fn_expr() {
        let mut emitter = LuaEmitter::new();
        let fn_expr = create_fn_expr();
        let result = emitter.visit_fn_expr(&fn_expr);
        assert_eq!(result, "function(x)\nreturn x\nend");
    }

    #[test]
    fn test_visit_def_fn() {
        let mut emitter = LuaEmitter::new();
        let fn_expr = create_fn_expr();
        let def = Def::FnDef(
            create_ident("test_fn"),
            vec![],
            TypeParam::Type(Identifier("Integer".into(), 0..1), 0..1),
            None,
            fn_expr, // Remove Box::new to match the expected type
            0..1,
        );
        let result = emitter.visit_def(&def);
        assert_eq!(result, "test_fn = function(x)\nreturn x\nend");
    }

    #[test]
    fn test_visit_def_type() {
        let mut emitter = LuaEmitter::new();
        let def = Def::TypeDef(
            create_ident("Option"),
            vec![],
            vec![
                TypeParam::Type(create_ident("Some"), 0..1),
                TypeParam::Type(create_ident("None"), 0..1),
            ],
            0..1,
        );
        let result = emitter.visit_def(&def);
        assert!(result.contains("Option = {}"));
        assert!(result.contains("Option.Some = function()"));
        assert!(result.contains("Option.None = function()"));
    }

    #[test]
    fn test_visit_stmt_fn_call() {
        let mut emitter = LuaEmitter::new();
        let stmt = Stmt::FnCallStmt(
            create_ident("print"),
            vec![Expr::LitExpr(create_integer(42), 0..2)],
            0..1,
        );
        let result = emitter.visit_stmt(&stmt);
        assert_eq!(result, "print(42)");
    }

    #[test]
    fn test_visit_stmt_assign() {
        let mut emitter = LuaEmitter::new();
        let stmt = Stmt::AssignStmt(
            create_ident("x"),
            None,
            Expr::LitExpr(create_integer(42), 0..2),
            0..1,
        );
        let result = emitter.visit_stmt(&stmt);
        assert_eq!(result, "x = 42");
    }

    #[test]
    fn test_visit_stmt_proc_call() {
        let mut emitter = LuaEmitter::new();
        let stmt = Stmt::ProcCallStmt(create_ident("do_something"), 0..1);
        let result = emitter.visit_stmt(&stmt);
        assert_eq!(result, "do_something()");
    }

    #[test]
    fn test_visit_expr_ident() {
        let mut emitter = LuaEmitter::new();
        let expr = Expr::IdentExpr(create_ident("x"), 0..1);
        let result = emitter.visit_expr(&expr);
        assert_eq!(result, "x");
    }

    #[test]
    fn test_visit_expr_lit() {
        let mut emitter = LuaEmitter::new();
        let expr = Expr::LitExpr(create_integer(42), 0..2);
        let result = emitter.visit_expr(&expr);
        assert_eq!(result, "42");
    }

    #[test]
    fn test_visit_expr_bin_op() {
        let mut emitter = LuaEmitter::new();
        let expr = Expr::BinOpExpr(
            BinOperator::Plus,
            P(Expr::LitExpr(create_integer(1), 0..1)),
            P(Expr::LitExpr(create_integer(2), 0..1)),
            0..3,
        );
        let result = emitter.visit_expr(&expr);
        assert_eq!(result, "1 + 2");
    }

    #[test]
    fn test_visit_expr_fn_call() {
        let mut emitter = LuaEmitter::new();
        let expr = Expr::FnCallExpr(
            create_ident("sum"),
            vec![
                Expr::LitExpr(create_integer(1), 0..1),
                Expr::LitExpr(create_integer(2), 0..1),
            ],
            0..3,
        );
        let result = emitter.visit_expr(&expr);
        assert_eq!(result, "sum(1, 2)");
    }
}