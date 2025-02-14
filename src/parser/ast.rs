use super::ptr::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier(pub String);

impl Into<Identifier> for & str {
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
pub struct Module(pub Vec<Def>);

//***************Definitions*****************
#[derive(Clone, Debug, PartialEq)]
pub enum Def {
    FnDef(Identifier, Vec<TypeParam>, TypeParam, Option<Vec<TypeConstraint>>, FnExpr),
    // type_name, Vec of (field_name, type_name)
    TypeDef(Identifier, Vec<(Identifier, TypeParam)>),

    TypeUnion(Identifier, Vec<Identifier>, Vec<TypeParam>),

    // class_name, Vec of generic_type_param
    ClassDef(Identifier, Vec<Identifier>, Vec<MethodDef>),
    // class_name, type_name
    InstanceDef(Identifier, Identifier, Vec<MethodImpl>),
    // proc_name, statements
    ProcDef(Identifier, Vec<Stmt>),
    // module_name
    ModuleDef(String),

    // const_name, literal
    ConstDef(Identifier, Literal),
}

#[derive(Clone, Debug, PartialEq)]
//type_constraint ::= '(' ident (ident | type_constraint)* ')'
pub struct TypeConstraint(pub Identifier, pub Vec<Identifier>);

#[derive(Clone, Debug, PartialEq)]
pub enum TypeParam {
    Empty,
    Type(Identifier),
    ArrayType(Identifier, Option<usize>),
    FuncType(Vec<TypeParam>, P<TypeParam>),
    PayloadType(Identifier, Vec<TypeParam>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum FnParam {
    LiteralParam(Literal),                   // only really for case
    IdentParam(Identifier),
    SetDeconstructParam(Vec<Identifier>),
    SetSelectorParam(Identifier, Identifier),
    TypeUnionParam(Identifier, Vec<Identifier>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum MethodDef {
    // Vec of generic_type_param, return type_name
    Operator(MethodOperator, Vec<TypeParam>, TypeParam),
    // vec of generic_type_param, return type_name
    Named(Identifier, Vec<TypeParam>, TypeParam),
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
    Operator(MethodOperator, FnExpr),
    Named(Identifier, FnExpr),
}

//***************Expressions*****************
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    IdentExpr(Identifier),
    LitExpr(Literal),
    SetExpr(Vec<(Identifier, Expr)>),
    ArrayExpr(Vec<Expr>),
    BinOpExpr(BinOperator, P<Expr>, P<Expr>),
    UnaryOp(UnaryOperator, P<Expr>),
    LetInExpr(Vec<Stmt>, P<Expr>),
    FnExpr(P<FnExpr>),
    FnCallExpr(Identifier, Vec<Expr>),
    CaseExpr(Identifier, Vec<(Vec<FnParam>, Vec<Expr>)>),

    TypeConstructor(Identifier, Vec<Expr>),

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
    FnExpr(Vec<FnParam>, Vec<Expr>),
    CaseFnExpr(Vec<FnExpr>),
}

//***************Statements*****************
// There is only one place these go.  Within LetIn blocks.

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Empty,
    // Option of type_name
    AssignStmt(Identifier, Option<Identifier>, Expr),
    FnCallStmt(Identifier, Vec<Expr>),
    ProcCallStmt(Identifier),

    CaseStmt(Identifier, Vec<(Vec<FnParam>, Stmt)>),

    // Vec of type_name
    MultiAssignStmt(Vec<Identifier>, Option<Vec<Identifier>>, Vec<Expr>),
    SetDeconstructAssignStmt(Vec<Identifier>, Vec<Expr>),
}
