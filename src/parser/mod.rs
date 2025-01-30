mod expr;
mod binop;
mod stmt;
mod def;

type Identifier = String;
type TypeName = Identifier;
type GenericType = Identifier;

//***************Module*********************
#[derive(Clone, Debug)]
struct Module {
    definitions: Vec<Def>,
    expression: Expr,
}

//***************Expressions*****************
#[derive(Clone, Debug)]
enum Expr {
    BinOpExpr(Box<BinOpExpr>),
    LetInExpr(Box<LetInExpr>),
    FnExpr(Box<FnExpr>),
    Literal(Literal),
}

#[derive(Clone, Debug)]
struct BinOpExpr {
    operator: BinOperator,
    lhs: Expr,
    rhs: Expr,
}

#[derive(Clone, Debug)]
enum BinOperator {
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
struct FnExpr {
    params: Vec<Identifier>,
    body: FnBodyExpr,
}

#[derive(Clone, Debug)]
enum FnBodyExpr {
    Expr(Expr),
    CaseExpr(CaseExpr),
}

#[derive(Clone, Debug)]
struct CaseExpr {
    fn_exprs: Vec<FnBodyExpr>,
}

#[derive(Clone, Debug)]
struct LetInExpr {
    stmts : LetInBlock,
    expr : Expr,
}

#[derive(Clone, Debug)]
struct Set {
    set_members : Vec<SetMember>,
}

#[derive(Clone, Debug)]
struct SetMember {
    field : Identifier,
    value : Expr,
}

#[derive(Clone, Debug)]
enum Literal {
    Integer(usize),
    Float(f64),
    String(String),
    Bool(bool),
    Set(Set),
}

type LetInBlock = Vec<Stmt>;

//***************Statements*****************
#[derive(Clone, Debug)]
enum Stmt {
    Asmt(Asmt),
}

#[derive(Clone, Debug)]
struct Asmt {
    identifier: Identifier,
    type_name: Option<TypeName>,
    expression: Expr,
}
//***************Definitions*****************
#[derive(Clone, Debug)]
enum Def {
    FnDef(FnDef),
    TypeDef(TypeDef),
    ClassDef(ClassDef),
    InstanceDef(InstanceDef),
    ModuleDef(ModuleDef),
}

// <func_name> :: <type_name>, <type_name>?.. => <type_name>
// <func_name> :: a, b?... => <func_expr>
#[derive(Clone, Debug)]
struct FnDef {
    identifier: Identifier,
    param_types: Vec<TypeName>,
    return_type: TypeName,
    fn_expr: FnExpr,
}

#[derive(Clone, Debug)]
struct TypeDef {
    identifier: Identifier,
    fields: Vec<FieldDef>,
}

#[derive(Clone, Debug)]
struct ClassDef {
    identifier: Identifier,
    methods : Vec<MethodDef>
}

#[derive(Clone, Debug)]
enum MethodDef {
    Operator(OperatorMethodDef),
    Named(NamedMethodDef),
}

#[derive(Clone, Debug)]
struct OperatorMethodDef {
    operator: MethodOperator,
    type_params : Vec<GenericType>,
    return_type : TypeName
}

#[derive(Clone, Debug)]
enum MethodOperator {
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
struct NamedMethodDef {
    method_name : Identifier,
    type_params : Vec<GenericType>,
    return_type : TypeName,
}

#[derive(Clone, Debug)]
struct InstanceDef {
    class_name : Identifier,
    type_name : Identifier,
    params : Vec<Identifier>,
    method_impls : Vec<MethodImplDef>,
}

#[derive(Clone, Debug)]
enum MethodImplDef {
    Operator(MethodImplOp),
    Named(MethodImplNamed),
}

#[derive(Clone, Debug)]
struct MethodImplOp {
    operator : MethodOperator,
    fn_expr : FnBodyExpr,
}

#[derive(Clone, Debug)]
struct MethodImplNamed {
    method_name : Identifier,
    fn_expr : FnBodyExpr,
}

#[derive(Clone, Debug)]
struct ModuleDef {
    module: String,
}

// just <identifier> : <type_name>
// Where both identifier and type_name are Identifiers at this point.
#[derive(Clone, Debug)]
struct FieldDef {
    identifier: Identifier,
    type_name: TypeName,
}

pub fn test_ast() {
    let ast = Expr::from(LetInExpr {
        stmts: vec![
            Stmt::from(Asmt {
                expression: Expr::from(10),
                identifier: String::from("x"),
                type_name: Some(String::from("Integer"))
            }),
            Stmt::from(Asmt {
                expression: Expr::from(2),
                identifier: String::from("y"),
                type_name: Some(String::from("Integer"))
            }),
        ],
        expr: Expr::from(BinOpExpr {
            operator: BinOperator::Plus,
            lhs: Expr::from(BinOpExpr {
                operator: BinOperator::Multiply,
                lhs: Expr::from("x"),
                rhs: Expr::from("x"),
            }),
            rhs: Expr::from("y")
        }),
    });

    println!("{:#?}", ast);
}

pub fn test_ast2() {
    let ast = Module {
        definitions: vec![
            Def::from(TypeDef {
                identifier: String::from("Person"),
                fields: vec![
                    FieldDef {
                        identifier: String::from("name"),
                        type_name: String::from("String"),
                    },
                    FieldDef {
                        identifier: String::from("age"),
                        type_name: String::from("Integer"),
                    },
                ],
            }),
        ],
        expression: Expr::from(10),
    };

    println!("{:#?}", ast);
}
