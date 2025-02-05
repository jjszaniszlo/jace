use super::ptr::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier(pub String);

impl<'a> From<&'a str> for Identifier {
    fn from(value: &'a str) -> Self {
        Identifier(value.to_string())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeName(pub Identifier);

impl<'a> From<&'a str> for TypeName {
    fn from(value: &'a str) -> Self {
        Self(Identifier(value.to_string()))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct GenericTypeParam(pub Identifier);

#[derive(Clone, Debug, PartialEq)]
pub struct ClassName(pub Identifier);

//***************Module*********************
#[derive(Clone, Debug, PartialEq)]
pub struct Module(pub Vec<Def>, pub Expr);

//***************Definitions*****************
#[derive(Clone, Debug, PartialEq)]
pub enum Def {
    FnDef(Identifier, Vec<TypeParam>, TypeParam, FnExpr),
    TypeDef(TypeName, Vec<(Identifier, TypeName)>),
    ClassDef(ClassName, Vec<GenericTypeParam>, Vec<MethodDef>),
    InstanceDef(ClassName, TypeName, Vec<FnParam>, Vec<MethodImpl>),
    ModuleDef(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeParam {
    Single(Identifier),
    ArrayType(Identifier),
}

#[derive(Clone, Debug, PartialEq)]
pub enum FnParam {
    Literal(Literal),                   // only really for case
    Identifier(Identifier),
    SetDeconstruct(Vec<Identifier>),
    SetSelector(Identifier, Identifier),
}

#[derive(Clone, Debug, PartialEq)]
pub enum MethodDef {
    Operator(MethodOperator, Vec<GenericTypeParam>, TypeName),
    Named(Identifier, Vec<GenericTypeParam>, TypeName),
}

#[derive(Clone, Debug, PartialEq)]
pub enum MethodOperator {
    Plus,
    Minus,
    Divide,
    Multiply,
    Exp,
    Greater,
    Less,
    GreaterEquals,
    LessEquals,
    EqualsEquals,
    NotEquals,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MethodImpl {
    Operator(MethodOperator, Expr),
    Named(Identifier, Expr),
}

//***************Expressions*****************
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    IdentExpr(Identifier),
    LitExpr(Literal),
    SetExpr(Vec<(Identifier, Expr)>),
    BinOpExpr(BinOperator, P<Expr>, P<Expr>),
    LetInExpr(Vec<Stmt>, P<Expr>),
    FnExpr(P<FnExpr>),
    FnCall(Identifier, Vec<Expr>),
    // predicates followed by their expressions, and lastly an else expression
    IfThenElseIfExpr(Vec<(Expr, Expr)>, P<Expr>),

    MemberExpr(MemberExpr),
}

#[derive(Clone, Debug, PartialEq)]
pub struct MemberExpr {
    pub identifier: Identifier,
    pub base: MemberExprBase,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MemberExprBase {
    Identifier(Identifier),
    MemberExpr(P<MemberExpr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Integer(usize),
    Float(f64),
    String(String),
    Bool(bool),
}

macro_rules! impl_from_x_for_literal {
    ($T:ty, $I:ident) => {
        impl From<$T> for Literal {        
            fn from(other: $T) -> Self {
                Self::$I(other)
            }
        }
    };
}

impl_from_x_for_literal!(usize, Integer);
impl_from_x_for_literal!(f64, Float);
impl_from_x_for_literal!(String, String);
impl_from_x_for_literal!(bool, Bool);

#[derive(Clone, Debug, PartialEq)]
pub enum BinOperator {
    Plus,
    Minus,
    Divide,
    Multiply,
    Exp,
    And,
    Or,
    GreaterEquals,
    LessEquals,
    Greater,
    Less,
    EqualsEquals,
    NotEquals,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FnExpr {
    Single(Vec<FnParam>, Expr),
    Case(Vec<FnExpr>),
}

//***************Statements*****************
// There is only one place these go.  Within LetIn blocks.

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Asmt(Identifier, Option<TypeName>, Expr),
    MultiAsmt(Vec<Identifier>, Vec<TypeName>, Vec<Expr>),
    SetDeconstructAsmt(Vec<Identifier>, Vec<Expr>),
}
