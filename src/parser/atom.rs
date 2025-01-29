use super::*;

impl From<Expr> for Atom {
    fn from(value: Expr) -> Self {
        Atom::Expr(value)
    }
}

impl From<usize> for Atom {
    fn from(value: usize) -> Self {
        Atom::Literal(Literal::Integer(value))
    }
}

impl From<f64> for Atom {
    fn from(value: f64) -> Self {
        Atom::Literal(Literal::Float(value))
    }
}

impl From<String> for Atom {
    fn from(value: String) -> Self {
        Atom::Literal(Literal::String(value))
    }
}

impl From<bool> for Atom {
    fn from(value: bool) -> Self {
        Atom::Literal(Literal::Bool(value))
    }
}
