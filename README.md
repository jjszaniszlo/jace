

```Haskell
type Bool ::
    | True
    | False

-- 'a represents a generic type to be used within the type expression
type Option 'a ::
    | Some : 'a -- Some constructs Option with type param 'a
    | None      -- None constructs Option with no type param

type Either 'a ::
    | Left : 'a
    | Right : 'a

-- type alias.  This says a point is any two of the same two types as a product.
-- constructed as Point 'a
type Point_ 'a :: 'a * 'a

type Shape 'a ::
    | Point : Point_ 'a                -- same as: | Point : (Point_ 'a)
    | Circle : Point_ 'a * 'a          -- same as: | Circle : (Point_ 'a) * 'a 
    | Rect : Point_ 'a * 'a * 'a       -- same as: | Rect : (Point_ 'a) * ('a) * ('a)

-- class
-- "class" identifier param "::" methods*
class Monad m ::
    -- method
    -- identifier "::" type_expr
    bind :: m 'a => ('a => m 'a) => m 'a
    return :: 'a => m 'a

instance Monad Option ::
    bind := o => f =>
    case o in
        | Some v => f v
        | None => None
    return := a => Some a

-- another way which you could define a monad.  The first way is better
-- because this wouldn't be able to curry.
class Monad2 m ::
    bind :: (m 'a) * ('a => m 'a) => m 'a
    return :: 'a => m 'a

instance Monad Option ::
    bind :=
    case
        | Some v, f => f v
        | None, _ => None
    return := a => Some a

MATH_PI := 3.14
        
-- all comments above the methods are optional type annotations.
-- mostly useful for forward declarations in niche scenarios where you have functions
-- call each other.

-- map :: ('a => 'b) * ['a] => ['b]
let map :=
case
    | _, {} => {}
    | f, {x:xs} => f x : map f xs
    
-- map :: ('a => 'b) => ['a] => ['b]
let map := f =>
case
    | {} => {}
    | {x:xs} => f x : map f xs

-- sum :: 'a * 'a => 'a
let sum := a, b => a + b

-- sum :: 'a => 'a => 'a
let sum := a => b => a + b

-- something :: 'a => 'a
let something := a =>
    let
        a2 := a * a
        a3 := a * a * a
    in
    print a2 a3;
    a2 + a3

-- do_something2 :: 'a => 'a
let do_something2 := a =>
    let
        times_two := a => a * 2
        times_three := a => a * 3
    in
    print (times_two a) (times_three a);
    let
        compose := f, g => f g 
    in (compose times_two times_three) a

-- this is the entry point '()' is the unit type.
-- def main :: () => ()
let main := _ =>
    let
        a := 69;
        b := 10;
        (c, d) := (34, 45);
    in
    print "hello world!" a b;

```

```
Program         ::= { Declaration | Statement }

Declaration     ::= TypeDecl | ClassDecl | InstanceDecl | ConstantDecl

TypeDecl        ::= "type" TypeName [TypeParams] "::" TypeBody
TypeBody        ::= SumType | TypeExpr

SumType         ::= Constructor { "|" Constructor }
Constructor     ::= ConstrName [ ":" TypeExpr ]

TypeExpr        ::= TypeTerm { "*" TypeTerm }
TypeTerm        ::= TypeName | "'" Identifier | "(" TypeExpr ")"

TypeParams      ::= "'" Identifier { "'" Identifier }

ClassDecl       ::= "class" Identifier Identifier "::" { MethodSig }

MethodSig       ::= Identifier "::" TypeExpr

InstanceDecl    ::= "instance" Identifier TypeName "::" { MethodImpl }

MethodImpl      ::= Identifier ":=" Expr

ConstantDecl    ::= Identifier ":=" Literal

Statement       ::= LetBinding | Expr

LetBinding      ::= "let" Identifier ":=" Expr

Expr            ::= Lambda | CaseExpr | LetExpr | InfixExpr

Lambda          ::= "let" Identifier ":=" ParamList "=>" Expr

ParamList       ::= Identifier { "," Identifier } 
                  | Identifier
                  | "(" ParamList ")"

CaseExpr        ::= "case" [Expr] "in" { "|" Pattern "=>" Expr }

LetExpr         ::= "let" { LetBinding } "in" Expr

InfixExpr       ::= Application { InfixOp Application }
Application     ::= Atom { Atom }

Atom            ::= Identifier
                  | Literal
                  | "(" Expr ")"
                  | ListExpr
                  | TupleExpr

TupleExpr       ::= "(" Expr "," Expr { "," Expr } ")"
ListExpr        ::= "{" [Expr { ":" Expr }] "}"

Pattern         ::= Identifier
                  | TupleExpr
                  | ListExpr

InfixOp         ::= "+" | "-" | "*" | "/" | "=>" | "*"

Literal         ::= Number | String

Identifier      ::= ? letter followed by letters, digits, or underscores ?
ConstrName      ::= ? uppercase letter followed by letters/digits/underscores ?
TypeName        ::= Identifier

Comment         ::= "--" { ? any character except newline ? } "\n"

Number          ::= Digit { Digit } ["." Digit { Digit }]
String          ::= '"' { ? any character except '"' ? } '"'
Digit           ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

```
