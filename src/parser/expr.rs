use super::*;

impl From<&str> for Expr {
    fn from(value: &str) -> Self {
        Expr::Literal(Literal::String(value.to_string()))
    }
}

impl From<usize> for Expr {
    fn from(value: usize) -> Self {
        Expr::Literal(Literal::Integer(value))
    }
}

impl From<f64> for Expr {
    fn from(value: f64) -> Self {
        Expr::Literal(Literal::Float(value))
    }
}

impl From<bool> for Expr {
    fn from(value: bool) -> Self {
        Expr::Literal(Literal::Bool(value))
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

impl_from_for_expr_box_wrapper!(BinOpExpr, LetInExpr, FnExpr);
