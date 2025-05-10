use crate::parser::prelude::Literal;

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