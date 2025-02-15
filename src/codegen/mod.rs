use mlua::ffi::lua_WarnFunction;
use crate::parser::prelude::{Def, Expr, Identifier, Literal, Module, Stmt};

pub fn codegen(module: Module) -> String {
    let mut code = String::new();
    for def in module.0 {
        code.push_str(&codegen_def(def));
    }
    code
}

pub fn codegen_def(def: Def) -> String {
    let mut code = String::new();
    match def {
        Def::FnDef(_, _, _, _, _, _) => todo!(),
        Def::TypeDef(_, _, _) => todo!(),
        Def::TypeUnion(_, _, _, _) => todo!(),
        Def::ClassDef(_, _, _, _) => todo!(),
        Def::InstanceDef(_, _, _, _) => todo!(),
        Def::ProcDef(idents, stmts, _) => code.push_str(&codegen_proc_def(idents, stmts)),
        Def::ModuleDef(_, _) => todo!(),
        Def::ConstDef(_, _, _) => todo!(),
    }
    code
}

fn codegen_proc_def(ident: Identifier, stmts: Vec<Stmt>) -> String {
    let mut code = String::new();
    code.push_str(&format!("function {}()\n", ident.to_string()));

    for stmt in stmts {
        code.push_str("\t");
        code.push_str(&codegen_stmt(stmt));
    }

    code.push_str("end\n");
    code
}

fn codegen_stmt(stmt: Stmt) -> String {
    let mut code = String::new();
    match stmt {
        Stmt::Empty(_) => todo!(),
        Stmt::AssignStmt(i, ty, expr, _) => code.push_str(&codegen_assign_stmt(i, expr)),
        Stmt::FnCallStmt(_, _, _) => todo!(),
        Stmt::ProcCallStmt(_, _) => todo!(),
        Stmt::CaseStmt(_, _, _) => todo!(),
        Stmt::MultiAssignStmt(_, _, _, _) => todo!(),
        Stmt::SetDeconstructAssignStmt(_, _, _) => todo!(),
    }
    code
}

fn codegen_assign_stmt(ident: Identifier, expr: Expr) -> String {
    let mut code = String::new();
    code.push_str("local ");
    code.push_str(&String::from(ident));
    code.push_str(" = ");
    code.push_str(&codegen_expr(expr));
    code.push_str("\n");
    code
}

fn codegen_expr(expr: Expr) -> String {
    let mut code = String::new();
    match expr {
        Expr::IdentExpr(_, _) => todo!(),
        Expr::LitExpr(l, _) => code.push_str(&codegen_lit_expr(l)),
        Expr::SetExpr(_, _) => todo!(),
        Expr::ArrayExpr(_, _) => todo!(),
        Expr::TupleExpr(_, _) => todo!(),
        Expr::BinOpExpr(_, _, _, _) => todo!(),
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

fn codegen_lit_expr(lit: Literal) -> String {
    let mut code = String::new();
    match lit {
        Literal::Integer(i) => code.push_str(&i.to_string()),
        Literal::Float(f) => code.push_str(&f.to_string()),
        Literal::String(s) => code.push_str(&s),
        Literal::Bool(b) => match b {
            true => code.push_str("true"),
            false => code.push_str("false"),
        },
    }
    code
}