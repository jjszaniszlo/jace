use crate::parser::prelude::{BinOperator, Def, Expr, Literal, Module, Stmt};


pub trait CodeGen {
    fn code_gen(&self, indent: usize) -> String;
}

impl CodeGen for Module {
    fn code_gen(&self, indent: usize) -> String {
        let mut code = String::new();
        for def in &self.0 {
            code.push_str(&def.code_gen(0))
        }
        code
    }
}

impl CodeGen for Def {
    fn code_gen(&self, indent: usize) -> String {
        let mut code = String::new();
        match self {
            Def::FnDef(_, _, _, _, _, _) => todo!(),
            Def::TypeDef(_, _, _) => todo!(),
            Def::TypeUnion(_, _, _, _) => todo!(),
            Def::ClassDef(_, _, _, _) => todo!(),
            Def::InstanceDef(_, _, _, _) => todo!(),
            Def::ProcDef(i, stmts, _) => {
                code.push_str(&format!("{:i$}local {}\n", i.0, i = indent));
                code.push_str(&format!("{:i$}{} = function()\n", i.0, i = indent));
                for stmt in stmts {
                    code.push_str(&format!("{:i$}{}", stmt.code_gen(indent + 2), i = indent))
                }
                code.push_str(&format!("{:i$}end\n", i.0, i = indent));
            },
            Def::ModuleDef(_, _) => todo!(),
            Def::ConstDef(_, _, _) => todo!(),
        }
        code
    }
}

impl CodeGen for Stmt {
    fn code_gen(&self, indent: usize) -> String {
        let mut code = String::new();
        match self {
            Stmt::Empty(_) => todo!(),
            Stmt::AssignStmt(i, _, exp, _) => {
                code.push_str(&format!("{:i$}{} = ", i.0, i = indent));
                code.push_str(&exp.code_gen(0));
            },
            Stmt::FnCallStmt(_, _, _) => todo!(),
            Stmt::ProcCallStmt(_, _) => todo!(),
            Stmt::CaseStmt(_, _, _) => todo!(),
            Stmt::MultiAssignStmt(_, _, _, _) => todo!(),
            Stmt::SetDeconstructAssignStmt(_, _, _) => todo!(),
        }
        code
    }
}

impl CodeGen for Expr {
    fn code_gen(&self, indent: usize) -> String {
        let mut code = String::new();
        match self {
            Expr::IdentExpr(i, _) => code.push_str(&i.0),
            Expr::LitExpr(l, _) => code.push_str(&l.code_gen(0)),
            Expr::SetExpr(_, _) => todo!(),
            Expr::ArrayExpr(_, _) => todo!(),
            Expr::TupleExpr(_, _) => todo!(),
            Expr::BinOpExpr(op, lhs, rhs, _) => {
                code.push_str(&format!("{} {} {}", lhs.code_gen(0), op.code_gen(0), rhs.code_gen(0)))
            },
            Expr::UnaryOp(_, _, _) => todo!(),
            Expr::LetInExpr(_, _, _) => todo!(),
            Expr::FnExpr(_, _) => todo!(),
            Expr::FnCallExpr(_, _, _) => todo!(),
            Expr::CaseExpr(_, _, _) => todo!(),
            Expr::TypeConstructor(_, _, _) => todo!(),
            Expr::IfThenElseIfExpr(_, _, _) => todo!(),
            Expr::MemberExpr(_, _) => todo!(),
        }
        code
    }
}

impl CodeGen for Literal {
    fn code_gen(&self, indent: usize) -> String {
        match self {
            Literal::Integer(i) => i.to_string(),
            Literal::Float(f) => f.to_string(),
            Literal::String(s) => format!("\"{}\"", s),
            Literal::Bool(b) => if *b {"true".to_string()} else {"false".to_string()},
        }
    }
}

impl CodeGen for BinOperator {
    fn code_gen(&self, indent: usize) -> String {
        let st = match self {
            BinOperator::Plus => "+",
            BinOperator::Minus => "-",
            BinOperator::Divide => "/",
            BinOperator::Multiply => "*",
            BinOperator::Exp => "^",
            BinOperator::And => "and",
            BinOperator::Or => "or",
            BinOperator::GreaterEquals => ">=",
            BinOperator::LessEquals => "<=",
            BinOperator::Greater => ">",
            BinOperator::Less => "<",
            BinOperator::EqualsEquals => "==",
            BinOperator::NotEquals => "~=",
            BinOperator::AppendSet => "",
        };
        st.to_string()
    }
}
