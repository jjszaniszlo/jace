mod atom;
mod expr;
mod binop;

type Identifier = String;
type TypeName = Identifier;

//***************Expressions*****************
#[derive(Debug)]
enum Expr {
    Atom(Box<Atom>),
    BinOp(Box<BinOp>),
    FnExpr(Box<FnExpr>),
}

// either a literal or '(' <expression> ')'
#[derive(Debug)]
enum Atom {
    Expr(Expr),
    Literal(Literal),
}

#[derive(Debug)]
struct BinOp {
    lhs: Atom,
    rhs: Atom,
}

#[derive(Debug)]
struct FnExpr {
    params: Vec<Identifier>,
    let_in_vars: Vec<Asmt>,
    body: FnBodyExpr,
}

#[derive(Debug)]
enum FnBodyExpr {
    Expr(Expr),
    CaseExpr(CaseExpr),
}

#[derive(Debug)]
struct CaseExpr {
    fn_exprs: Vec<FnBodyExpr>,
}

#[derive(Debug)]
enum Literal {
    Integer(usize),
    Float(f64),
    String(String),
    Bool(bool),
}

//***************Statements*****************
#[derive(Debug)]
enum Stmt {
    Asmt(Asmt),
}

#[derive(Debug)]
struct Asmt {
    identifier: Identifier,
    type_name: Option<TypeName>,
    expression: Expr,
}
//***************Definitions*****************
#[derive(Debug)]
enum Def {
    FnDef(FnDef),
    TypeDef(TypeDef),
}

// <func_name> :: <type_name>, <type_name>?.. => <type_name>
// <func_name> :: a, b?... => <func_expr>
#[derive(Debug)]
struct FnDef {
    identifier: Identifier,
    param_types: Vec<TypeName>,
    return_type: TypeName,
    fn_expr: FnExpr,
}

#[derive(Debug)]
struct TypeDef {
    identifier: Identifier,
    fields: Vec<VarDef>,
}

// just <identifier> : <type_name>
// Where both identifier and type_name are Identifiers at this point.
#[derive(Debug)]
struct VarDef {
    identifier: Identifier,
    type_name: TypeName,
}

pub fn test_ast() {
    let ast = Asmt {
        identifier: String::from("x"),
        type_name: None,
        expression: Expr::from(10),
    };
    println!("{:#?}", ast);
}
