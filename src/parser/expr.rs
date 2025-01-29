use super::*;

impl Expr {
    fn expr_to_atom(expr: Expr) -> Self {
        Expr::Atom(Box::new(Atom::from(expr)))
    }
}

// evil macros
macro_rules! impl_from_for_expr_box_wrapper {
    ($($T:ident),+ $(,)?) => {
        $(
            impl From<$T> for Expr {
                fn from(value: $T) -> Self {
                    Expr::$T(Box::new(value))
                }
            }
        )+
    };
}

impl_from_for_expr_box_wrapper!(BinOpExpr, LetInExpr);

macro_rules! impl_from_for_expr_atom_box_wrapper {
    ($($T:ident),+ $(,)?) => {
        $(
            impl From<$T> for Expr {
                fn from(value: $T) -> Self {
                    Expr::Atom(Box::new(Atom::from(value)))
                }
            }
        )+
    };
}

impl_from_for_expr_atom_box_wrapper!(usize, f64, String, bool);
