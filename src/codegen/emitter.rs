use crate::parser::prelude::{Def, Literal, Module};

// emit the AST to a string of Lua code.
pub trait Emitter {
    fn emit(&self) -> String;
}

impl Emitter for Literal {
    fn emit(&self) -> String {
        match self {
            Literal::Integer(value, _) => value.to_string(),
            Literal::Float(value, _) => value.to_string(),
            Literal::String(value, _) => format!("\"{}\"", value),
            Literal::Bool(value, _) => value.to_string(),
        }
    }
}

impl Emitter for Def {
    fn emit(&self) -> String {
        let mut output = String::new();
        match self {
            Def::FnDef(name, params, ret, _, fnexpr, _) => unimplemented!(),
            Def::TypeDef(name, params, tags, _ ) => unimplemented!(),
            Def::ProcDef(name, stmts, _) => unimplemented!(),
        }
        "".to_string()
    }
}

impl Emitter for Module { 
    fn emit(&self) -> String {
        let mut output = String::new();
        for item in &self.0 {
            output.push_str(&item.emit());
            output.push('\n');
        }
        output
    }
}