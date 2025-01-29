
type Identifier = String;
type TypeName = Identifier;

//***************Expressions*****************
enum Expr {
    Atom(Box<Atom>),
    BinOp(Box<BinOp>),
    FnExpr(Box<FnExpr>),
}

// either a literal or '(' <expression> ')'
enum Atom {
    Expr(Expr),
    Literal(Literal),
}

struct BinOp {
    lhs: Atom,
    rhs: Atom,
}

struct FnExpr {
    params: Vec<Identifier>,
    let_in_vars: Vec<Asmt>,
    body: FnBodyExpr,
}

enum FnBodyExpr {
    Expr(Expr),
    CaseExpr(CaseExpr),
}

struct CaseExpr {
    fn_exprs: Vec<FnBodyExpr>,
}

enum Literal {
    Integer(usize),
    Float(f64),
    String(String),
    Bool(bool),
}

//***************Statements*****************
enum Stmt {
    Asmt(Asmt),
}

struct Asmt {
    identifier: Identifier,
    expression: Expr,
}
//***************Definitions*****************
enum Def {
    FnDef(FnDef),
    TypeDef(TypeDef),
}

// <func_name> :: <type_name>, <type_name>?.. => <type_name>
// <func_name> :: a, b?... => <func_expr>
struct FnDef {
    identifier: Identifier,
    param_types: Vec<TypeName>,
    return_type: TypeName,
    fn_expr: FnExpr,
}

struct TypeDef {
    identifier: Identifier,
    fields: VarDef,
}

// just <identifier> : <type_name>
// Where both identifier and type_name are Identifiers at this point.
struct VarDef {
    identifier: Identifier,
    TypeName: TypeName,
}
