use super::ptr::*;

#[derive(Clone, Debug)]
pub struct Identifier(String);

#[derive(Clone, Debug)]
pub struct TypeName(Identifier);

#[derive(Clone, Debug)]
pub struct GenericTypeParam(Identifier);

#[derive(Clone, Debug)]
pub struct ClassName(Identifier);

//***************Module*********************
#[derive(Clone, Debug)]
pub struct Module {
    definitions: Vec<Def>,
    expression: Expr,
}

//***************Definitions*****************
#[derive(Clone, Debug)]
pub enum Def {
    FnDef(Identifier, Vec<TypeName>, TypeName, FnExpr),
    TypeDef(TypeName, Vec<(Identifier, TypeName)>),
    ClassDef(ClassName, GenericTypeParam, Vec<MethodDef>),
    InstanceDef(TypeName, ClassName, Vec<FnParam>, Vec<MethodImpl>),
    ModuleDef(String),
}

#[derive(Clone, Debug)]
pub enum FnParam {
    Literal(Literal),                   // only really for case
    Identifier(Identifier),
    SetDeconstruct(Vec<Identifier>),
    SetSelector(Identifier, Identifier),
}

#[derive(Clone, Debug)]
pub enum MethodDef {
    Operator(MethodOperator, Vec<GenericTypeParam>, TypeName),
    Named(Identifier, Vec<GenericTypeParam>, TypeName),
}

#[derive(Clone, Debug)]
pub enum MethodOperator {
    Plus,
    Minus,
    Divide,
    Multiply,
    Greater,
    Less,
    GreaterEquals,
    LessEquals,
    EqualsEquals,
    NotEquals,
}

#[derive(Clone, Debug)]
pub enum MethodImpl {
    Operator(MethodOperator, Expr),
    Named(Identifier, Expr),
}

//***************Expressions*****************
#[derive(Clone, Debug)]
pub enum Expr {
    Literal(Literal),
    BinOpExpr(BinOperator, P<Expr>, P<Expr>),
    LetInExpr(Vec<Stmt>, P<Expr>),
    FnExpr(P<FnExpr>),
}

#[derive(Clone, Debug)]
pub enum Literal {
    Integer(usize),
    Float(f64),
    String(String),
    Bool(bool),
    Set(Vec<(Identifier, Expr)>),
}

#[derive(Clone, Debug)]
pub enum BinOperator {
    Plus,
    Minus,
    Divide,
    Multiply,
    And,
    Or,
    GreaterEquals,
    LessEquals,
    Greater,
    Less,
    EqualsEquals,
    NotEquals,
}

#[derive(Clone, Debug)]
pub enum FnExpr {
    Single(Vec<FnParam>, Expr),
    Case(Vec<FnExpr>),
}

//***************Statements*****************
// There is only one place these go.  Within LetIn blocks.

#[derive(Clone, Debug)]
pub enum Stmt {
    Asmt(Identifier, Option<TypeName>, Expr),
    MultiAsmt(Vec<Identifier>, Vec<TypeName>, Vec<Expr>),
    SetDeconstructAsmt(Vec<Identifier>, Vec<Expr>),
}
