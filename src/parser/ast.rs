use super::ptr::*;
use crate::err::Span;

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier(pub String);

impl Into<Identifier> for &str {
    fn into(self) -> Identifier {
        Identifier(self.to_string())
    }
}

impl Into<Identifier> for String {
    fn into(self) -> Identifier {
        Identifier(self)
    }
}

//***************Module*********************
#[derive(Clone, Debug, PartialEq)]
pub struct Module(pub Vec<Def>, pub Span);

//***************Definitions*****************
#[derive(Clone, Debug, PartialEq)]
pub enum Def {
    // func_name, TypeParams, Return Type, Optional Class Type Constraints, Expression
    FnDef(Identifier, Vec<TypeParam>, TypeParam, Option<Vec<TypeConstraint>>, FnExpr, Span),
    // type_name, Vec of (field_name, type_name)
    TypeDef(Identifier, Vec<(Identifier, TypeParam)>, Span),

    // Algebraic types
    // name, any generic type params, the types that are joined together.
    TypeUnion(Identifier, Vec<Identifier>, Vec<TypeParam>, Span),

    // class_name, Vec of generic_type_param
    ClassDef(Identifier, Vec<Identifier>, Vec<MethodDef>, Span),
    // class_name, type_name
    InstanceDef(Identifier, Identifier, Vec<MethodImpl>, Span),
    // proc_name, statements
    ProcDef(Identifier, Vec<Stmt>, Span),
    // module_name
    ModuleDef(String, Span),

    // const_name, literal
    ConstDef(Identifier, Literal, Span),
}

#[derive(Clone, Debug, PartialEq)]
//type_constraint ::= '(' ident (ident | type_constraint)* ')'
pub struct TypeConstraint(pub Identifier, pub Vec<Identifier>, pub Span);

#[derive(Clone, Debug, PartialEq)]
pub enum TypeParam {
    Empty(Span),
    Type(Identifier, Span),
    Tuple(Vec<TypeParam>, Span),
    ArrayType(Identifier, Option<usize>, Span),
    FuncType(Vec<TypeParam>, P<TypeParam>, Span),
    PayloadType(Identifier, Vec<TypeParam>, Span),
}

#[derive(Clone, Debug, PartialEq)]
pub enum FnParam {
    LiteralParam(Literal, Span),                   // only really for case
    IdentParam(Identifier, Span),
    SetDeconstructParam(Vec<Identifier>, Span),
    SetSelectorParam(Identifier, Identifier, Span),
    TypeUnionParam(Identifier, Vec<Identifier>, Span),
}

#[derive(Clone, Debug, PartialEq)]
pub enum MethodDef {
    // Vec of generic_type_param, return type_name
    Operator(MethodOperator, Vec<TypeParam>, TypeParam, Span),
    // vec of generic_type_param, return type_name
    Named(Identifier, Vec<TypeParam>, TypeParam, Span),
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
    Operator(MethodOperator, FnExpr, Span),
    Named(Identifier, FnExpr, Span),
}

//***************Expressions*****************
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    IdentExpr(Identifier, Span),
    LitExpr(Literal, Span),

    SetExpr(Vec<(Identifier, Expr)>, Span),
    ArrayExpr(Vec<Expr>, Span),
    TupleExpr(Vec<Expr>, Span),

    BinOpExpr(BinOperator, P<Expr>, P<Expr>, Span),
    UnaryOp(UnaryOperator, P<Expr>, Span),
    LetInExpr(Vec<Stmt>, P<Expr>, Span),
    FnExpr(P<FnExpr>, Span),
    FnCallExpr(Identifier, Vec<Expr>, Span),
    CaseExpr(Identifier, Vec<(Vec<FnParam>, Expr)>, Span),

    TypeConstructor(Identifier, Vec<Expr>, Span),

    // predicates followed by their expressions, and lastly an else expression
    IfThenElseIfExpr(Vec<(Expr, Expr)>, P<Expr>, Span),
    MemberExpr(MemberExpr, Span),
}

#[derive(Clone, Debug, PartialEq)]
pub struct MemberExpr {
    pub identifier: Identifier,
    pub base: MemberExprBase,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MemberExprBase {
    Member(Identifier),
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
    AppendSet,
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOperator {
    Negative,
    Not,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FnExpr {
    FnExpr(Vec<FnParam>, Expr, Span),
    CaseFnExpr(Vec<FnExpr>, Span),
}

//***************Statements*****************
// There is only one place these go.  Within LetIn blocks.

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Empty(Span),
    // Option of type_name
    AssignStmt(Identifier, Option<Identifier>, Expr, Span),
    FnCallStmt(Identifier, Vec<Expr>, Span),
    ProcCallStmt(Identifier, Span),

    CaseStmt(Identifier, Vec<(Vec<FnParam>, Stmt)>, Span),

    // Vec of type_name
    MultiAssignStmt(Vec<Identifier>, Option<Vec<Identifier>>, Expr, Span),
    SetDeconstructAssignStmt(Vec<Identifier>, Expr, Span),
}
