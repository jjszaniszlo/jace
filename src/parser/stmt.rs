use super::*;

impl From<Asmt> for Stmt {
    fn from(value: Asmt) -> Self {
        Stmt::Asmt(value)
    }
}
