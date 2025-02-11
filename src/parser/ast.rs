use super::ptr::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier(pub String);

//***************Module*********************
#[derive(Clone, Debug, PartialEq)]
pub struct Module(pub Vec<Def>, pub Option<Expr>);

//***************Definitions*****************
#[derive(Clone, Debug, PartialEq)]
pub enum Def {
    FnDef(Identifier, Vec<TypeParam>, TypeParam, Option<Vec<(Identifier, Identifier)>>, FnExpr),
    // type_name, Vec of (field_name, type_name)
    TypeDef(Identifier, Vec<(Identifier, Identifier)>),

    TypeAlias(Identifier, Vec<TypeParam>),

    // class_name, Vec of generic_type_param
    ClassDef(Identifier, Vec<Identifier>, Vec<MethodDef>),
    // class_name, type_name
    InstanceDef(Identifier, Identifier, Vec<FnParam>, Vec<MethodImpl>),
    // proc_name, statements
    ProcDef(Identifier, Vec<Stmt>),
    // module_name
    ModuleDef(String),

    // const_name, literal
    ConstDef(Identifier, Literal),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeParam {
    Type(Identifier),
    ArrayType(Identifier, Option<usize>),
    FuncType(Vec<TypeParam>, P<TypeParam>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum FnParam {
    LiteralParam(Literal),                   // only really for case
    IdentParam(Identifier),
    SetDeconstructParam(Vec<Identifier>),
    SetSelectorParam(Identifier, Identifier),
}

#[derive(Clone, Debug, PartialEq)]
pub enum MethodDef {
    // Vec of generic_type_param, return type_name
    Operator(MethodOperator, Vec<Identifier>, Identifier),
    // vec of generic_type_param, return type_name
    Named(Identifier, Vec<Identifier>, Identifier),
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
    ArrayExpr(Vec<Expr>),
    BinOpExpr(BinOperator, P<Expr>, P<Expr>),
    UnaryOp(UnaryOperator, P<Expr>),
    LetInExpr(Vec<Stmt>, P<Expr>),
    FnExpr(P<FnExpr>),
    FnCallExpr(Identifier, Vec<Expr>),
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
    // Option of type_name
    AssignStmt(Identifier, Option<Identifier>, Expr),
    FnCallStmt(Identifier, Vec<Expr>),
    ProcCallStmt(Identifier),

    // Vec of type_name
    MultiAssignStmt(Vec<Identifier>, Option<Vec<Identifier>>, Vec<Expr>),
    SetDeconstructAssignStmt(Vec<Identifier>, Vec<Expr>),
}
