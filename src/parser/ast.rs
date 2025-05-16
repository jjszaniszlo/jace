use super::ptr::*;
use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier(pub String, pub Range<usize>);


//***************Module*********************
#[derive(Clone, Debug, PartialEq)]
pub struct Module(pub Vec<Def>, pub Range<usize>);

//***************Definitions*****************
#[derive(Clone, Debug, PartialEq)]
pub enum Def {
    // func_name, TypeParams, Return Type, Optional Class Type Constraints, Expression
    FnDef(Identifier, Vec<TypeParam>, TypeParam, Option<Vec<TypeConstraint>>, FnExpr, Range<usize>),

    // type_name, parametric types, sum of types.
    TypeDef(Identifier, Vec<Identifier>, Vec<TypeParam>, Range<usize>),

    // // class_name, Vec of generic_type_param
    // ClassDef(Identifier, Vec<Identifier>, Vec<MethodDef>, Range<usize>),
    // // class_name, type_name
    // InstanceDef(Identifier, Identifier, Vec<MethodImpl>, Range<usize>),
    // proc_name, statements
    ProcDef(Identifier, Vec<Stmt>, Range<usize>),
    // module_name
    // ModuleDef(String, Range<usize>),

    // // const_name, literal
    // ConstDef(Identifier, Literal, Range<usize>),
}

#[derive(Clone, Debug, PartialEq)]
//type_constraint ::= '(' ident (ident | type_constraint)* ')'
pub struct TypeConstraint(pub Identifier, pub Vec<Identifier>, pub Range<usize>);

#[derive(Clone, Debug, PartialEq)]
pub enum TypeParam {
    // "(" ")"
    Empty(Range<usize>),

    // <ident>
    Type(Identifier, Range<usize>),

    // "(" <type_param> ("," <type_param>)+ ")"
    TupleType(Vec<TypeParam>, Range<usize>),
    // "[" <type_param> <integer>? "]"
    ArrayType(P<TypeParam>, Option<usize>, Range<usize>),
    // <type_param> ("," <type_param>)* "=>" <type_param>
    FuncType(Vec<TypeParam>, P<TypeParam>, Range<usize>),

    // <ident> <type_param>*
    TypeConstructorType(Identifier, Vec<TypeParam>, Range<usize>),

    // <ident> (<ident> ":" <type_param>)+
    RecordType(Identifier, Vec<(Identifier, TypeParam)>, Range<usize>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum FnPatternParam {
    // <literal>
    BindToLiteralParam(Literal, Range<usize>),

    // <ident>
    BindToIdentParam(Identifier, Range<usize>),

    // "{" <ident> ("," <ident>)* "}"
    BindToSetDeconstructParam(Vec<Identifier>, Range<usize>),

    // "{" <ident> : <ident> "}"
    BindToSetSelectorParam(Identifier, Identifier, Range<usize>),

    // (<ident> <ident>*) | (<ident> <literal>*)
    BindToTypeConstructorParam(Identifier, Vec<FnPatternParam>, Range<usize>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum MethodDef {
    // Vec of generic_type_param, return type_name
    // <wrappedop> "::" <type> ("," <type>)* "=>" <type>
    Operator(MethodOperator, Vec<TypeParam>, TypeParam, Range<usize>),
    // vec of generic_type_param, return type_name
    // <ident> "::" <type> ("," <type>)* "=>" <type>
    Named(Identifier, Vec<TypeParam>, TypeParam, Range<usize>),
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

// #[derive(Clone, Debug, PartialEq)]
// pub enum MethodImpl {
//     // <wrappedop> "::" <fnexpr>
//     Operator(MethodOperator, FnExpr, Range<usize>),
//     // <ident> "::" <fnexpr>
//     Named(Identifier, FnExpr, Range<usize>),
// }

//***************Expressions*****************
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    // <ident>
    IdentExpr(Identifier, Range<usize>),

    // <integer> | <float> | <string> | <boolean>
    LitExpr(Literal, Range<usize>),

    // "{" (<ident> "=" <expr>)+ "}"
    SetExpr(Vec<(Identifier, Expr)>, Range<usize>),

    // "{" (<expr> ("," <expr>)* )? "}"
    ArrayExpr(Vec<Expr>, Range<usize>),

    // "(" <expr> ("," <expr>)+ ")"
    TupleExpr(Vec<Expr>, Range<usize>),

    // <unop> <expr>
    UnaryOp(UnaryOperator, P<Expr>, Range<usize>),

    // <expr> <binop> <expr>
    BinOpExpr(BinOperator, P<Expr>, P<Expr>, Range<usize>),

    // "let" <stmt>* "in" <expr>
    LetInExpr(Vec<Stmt>, P<Expr>, Range<usize>),

    // // <ident> ("," <ident>)+ "=>" <expr>
    FnExpr(P<FnExpr>, Range<usize>),

    // <ident> <expr>+
    FnCallExpr(Identifier, Vec<Expr>, Range<usize>),

    // "case" <expr> <branch> <branch>+
    CaseExpr(Identifier, Vec<(Vec<FnPatternParam>, Expr)>, Range<usize>),

    // "if" <expr> ("then" <expr> "elseif" <expr>)* "then" <expr> "else" <expr>
    IfThenElseIfExpr(Vec<(Expr, Expr)>, P<Expr>, Range<usize>),

    // <ident> ("." <ident>)+
    MemberExpr(MemberExpr, Range<usize>),

    // <ident> <expr>+
    // TypeConstructor(Identifier, Vec<Expr>, Range<usize>),               // the type checker
    // converts FnCallExpr to
    // this type
}


#[derive(Clone, Debug, PartialEq)]
pub struct MemberExpr {
    pub identifier: Identifier,
    pub base: MemberExprBase,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MemberExprBase {
    Member(Identifier, Range<usize>),
    MemberExpr(P<MemberExpr>, Range<usize>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Integer(usize, Range<usize>),
    Float(f64, Range<usize>),
    String(String, Range<usize>),
    Bool(bool, Range<usize>),
}


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
    FnExpr(Vec<FnPatternParam>, Expr, Range<usize>),
    CaseFnExpr(Vec<FnExpr>, Range<usize>),
}


//***************Statements*****************
// There is only one place these go.  Within LetIn blocks.

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Empty(Range<usize>),

    // <ident> ":=" <expr>
    AssignStmt(Identifier, Option<Identifier>, Expr, Range<usize>),

    // <ident> <expr> <expr>* "!"
    FnCallStmt(Identifier, Vec<Expr>, Range<usize>),
    // <ident> "!"
    ProcCallStmt(Identifier, Range<usize>),

    // case <expr> <stmt_branch> <stmt_branch>+
    // Even thouugh it uses statements, the parser only parses function calls
    // since it doesn't really make sense to have any other kind of statment
    // be valid since case statements would introduce their own inner scope.
    CaseStmt(Expr, Vec<(Vec<FnPatternParam>, Stmt)>, Range<usize>),

    // <ident> ("," <ident>)+ ":=" <expr> ("," <expr>)+
    MultiAssignStmt(Vec<Identifier>, Option<Vec<Identifier>>, Expr, Range<usize>),
    // "{" <ident>  ("," <ident>)* "}" ":=" <expr> ("," <expr>)*
    SetDeconstructAssignStmt(Vec<Identifier>, Expr, Range<usize>),
}
