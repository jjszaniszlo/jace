use super::*;


impl Expr {
    fn expr_to_atom(expr: Expr) -> Self {
        Expr::Atom(Box::new(Atom::from(expr)))
    }
}

impl From<usize> for Expr {
    fn from(value: usize) -> Self {
        return Expr::Atom(Box::new(Atom::from(value)));
    }
}

impl From<f64> for Expr {
    fn from(value: f64) -> Self {
        return Expr::Atom(Box::new(Atom::from(value)));
    }
}

impl From<String> for Expr {
    fn from(value: String) -> Self {
        return Expr::Atom(Box::new(Atom::from(value)));
    }
}

impl From<bool> for Expr {
    fn from(value: bool) -> Self {
        return Expr::Atom(Box::new(Atom::from(value)));
    }
}
