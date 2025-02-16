use std::ops::Range;
use super::ptr::*;

pub trait AstSpan {
    fn span(&self) -> Range<usize>;
}

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
pub struct Module(pub Vec<Def>, pub Range<usize>);
impl AstSpan for Module {
    fn span(&self) -> Range<usize> {
        self.1.clone()
    }
}

//***************Definitions*****************
#[derive(Clone, Debug, PartialEq)]
pub enum Def {
    // func_name, TypeParams, Return Type, Optional Class Type Constraints, Expression
    FnDef(Identifier, Vec<TypeParam>, TypeParam, Option<Vec<TypeConstraint>>, FnExpr, Range<usize>),
    // type_name, Vec of (field_name, type_name)
    TypeDef(Identifier, Vec<(Identifier, TypeParam)>, Range<usize>),

    // Algebraic types
    // name, any generic type params, the types that are joined together.
    TypeUnion(Identifier, Vec<Identifier>, Vec<TypeParam>, Range<usize>),

    // class_name, Vec of generic_type_param
    ClassDef(Identifier, Vec<Identifier>, Vec<MethodDef>, Range<usize>),
    // class_name, type_name
    InstanceDef(Identifier, Identifier, Vec<MethodImpl>, Range<usize>),
    // proc_name, statements
    ProcDef(Identifier, Vec<Stmt>, Range<usize>),
    // module_name
    ModuleDef(String, Range<usize>),

    // const_name, literal
    ConstDef(Identifier, Literal, Range<usize>),
}

impl AstSpan for Def {
    fn span(&self) -> Range<usize> {
        let s = match self {
            Def::FnDef(_, _, _, _, _, s) => s,
            Def::TypeDef(_, _, s) => s,
            Def::TypeUnion(_, _, _, s) => s,
            Def::ClassDef(_, _, _, s) => s,
            Def::InstanceDef(_, _, _, s) => s,
            Def::ProcDef(_, _, s) => s,
            Def::ModuleDef(_, s) => s,
            Def::ConstDef(_, _, s) => s,
        };
        s.clone()
    }
}

#[derive(Clone, Debug, PartialEq)]
//type_constraint ::= '(' ident (ident | type_constraint)* ')'
pub struct TypeConstraint(pub Identifier, pub Vec<Identifier>, pub Range<usize>);

impl AstSpan for TypeConstraint {
    fn span(&self) -> Range<usize> {
        self.2.clone()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeParam {
    Empty(Range<usize>),
    Type(Identifier, Range<usize>),
    Tuple(Vec<TypeParam>, Range<usize>),
    ArrayType(Identifier, Option<usize>, Range<usize>),
    FuncType(Vec<TypeParam>, P<TypeParam>, Range<usize>),
    PayloadType(Identifier, Vec<TypeParam>, Range<usize>),
}

impl AstSpan for TypeParam {
    fn span(&self) -> Range<usize> {
        let s = match self {
            TypeParam::Empty(s) => s,
            TypeParam::Type(_, s) => s,
            TypeParam::Tuple(_, s) => s,
            TypeParam::ArrayType(_, _, s) => s,
            TypeParam::FuncType(_, _, s) => s,
            TypeParam::PayloadType(_, _, s) => s,
        };
        s.clone()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum FnParam {
    LiteralParam(Literal, Range<usize>),                   // only really for case
    IdentParam(Identifier, Range<usize>),
    SetDeconstructParam(Vec<Identifier>, Range<usize>),
    SetSelectorParam(Identifier, Identifier, Range<usize>),
    TypeUnionParam(Identifier, Vec<Identifier>, Range<usize>),
}

impl AstSpan for FnParam {
    fn span(&self) -> Range<usize> {
        let s = match self {
            FnParam::LiteralParam(_, s) => s,
            FnParam::IdentParam(_, s) => s,
            FnParam::SetDeconstructParam(_, s) => s,
            FnParam::SetSelectorParam(_, _, s) => s,
            FnParam::TypeUnionParam(_, _, s) => s,
        };
        s.clone()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum MethodDef {
    // Vec of generic_type_param, return type_name
    Operator(MethodOperator, Vec<TypeParam>, TypeParam, Range<usize>),
    // vec of generic_type_param, return type_name
    Named(Identifier, Vec<TypeParam>, TypeParam, Range<usize>),
}

impl AstSpan for MethodDef {
    fn span(&self) -> Range<usize> {
        let s = match self {
            MethodDef::Operator(_, _, _, s) => s,
            MethodDef::Named(_, _, _, s) => s,
        };
        s.clone()
    }
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
    Operator(MethodOperator, FnExpr, Range<usize>),
    Named(Identifier, FnExpr, Range<usize>),
}

impl AstSpan for MethodImpl {
    fn span(&self) -> Range<usize> {
        let s = match self {
            MethodImpl::Operator(_, _, s) => s,
            MethodImpl::Named(_, _, s) => s,
        };
        s.clone()
    }
}

//***************Expressions*****************
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    IdentExpr(Identifier, Range<usize>),
    LitExpr(Literal, Range<usize>),

    SetExpr(Vec<(Identifier, Expr)>, Range<usize>),
    ArrayExpr(Vec<Expr>, Range<usize>),
    TupleExpr(Vec<Expr>, Range<usize>),

    BinOpExpr(BinOperator, P<Expr>, P<Expr>, Range<usize>),
    UnaryOp(UnaryOperator, P<Expr>, Range<usize>),
    LetInExpr(Vec<Stmt>, P<Expr>, Range<usize>),
    FnExpr(P<FnExpr>, Range<usize>),
    FnCallExpr(Identifier, Vec<Expr>, Range<usize>),
    CaseExpr(Identifier, Vec<(Vec<FnParam>, Expr)>, Range<usize>),

    TypeConstructor(Identifier, Vec<Expr>, Range<usize>),

    // predicates followed by their expressions, and lastly an else expression
    IfThenElseIfExpr(Vec<(Expr, Expr)>, P<Expr>, Range<usize>),
    MemberExpr(MemberExpr, Range<usize>),
}

impl AstSpan for Expr {
    fn span(&self) -> Range<usize> {
        let s = match self {
            Expr::IdentExpr(_, s) => s,
            Expr::LitExpr(_, s) => s,
            Expr::SetExpr(_, s) => s,
            Expr::ArrayExpr(_, s) => s,
            Expr::TupleExpr(_, s) => s,
            Expr::BinOpExpr(_, _, _, s) => s,
            Expr::UnaryOp(_, _, s) => s,
            Expr::LetInExpr(_, _, s) => s,
            Expr::FnExpr(_, s) => s,
            Expr::FnCallExpr(_, _, s) => s,
            Expr::CaseExpr(_, _, s) => s,
            Expr::TypeConstructor(_, _, s) => s,
            Expr::IfThenElseIfExpr(_, _, s) => s,
            Expr::MemberExpr(_, s) => s,
        };
        s.clone()
    }
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
    FnExpr(Vec<FnParam>, Expr, Range<usize>),
    CaseFnExpr(Vec<FnExpr>, Range<usize>),
}

impl AstSpan for FnExpr {
    fn span(&self) -> Range<usize> {
        let s = match self {
            FnExpr::FnExpr(_, _, s) => s,
            FnExpr::CaseFnExpr(_, s) => s,
        };
        s.clone()
    }
}

//***************Statements*****************
// There is only one place these go.  Within LetIn blocks.

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Empty(Range<usize>),
    // Option of type_name
    AssignStmt(Identifier, Option<Identifier>, Expr, Range<usize>),
    FnCallStmt(Identifier, Vec<Expr>, Range<usize>),
    ProcCallStmt(Identifier, Range<usize>),

    CaseStmt(Identifier, Vec<(Vec<FnParam>, Stmt)>, Range<usize>),

    // Vec of type_name
    MultiAssignStmt(Vec<Identifier>, Option<Vec<Identifier>>, Expr, Range<usize>),
    SetDeconstructAssignStmt(Vec<Identifier>, Expr, Range<usize>),
}

impl AstSpan for Stmt {
    fn span(&self) -> Range<usize> {
        let s = match self {
            Stmt::Empty(s) => s,
            Stmt::AssignStmt(_, _, _, s) => s,
            Stmt::FnCallStmt(_, _, s) => s,
            Stmt::ProcCallStmt(_, s) => s,
            Stmt::CaseStmt(_, _, s) => s,
            Stmt::MultiAssignStmt(_, _, _, s) => s,
            Stmt::SetDeconstructAssignStmt(_, _, s) => s,
        };
        s.clone()
    }
}
