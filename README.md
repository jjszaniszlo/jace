

```Haskell
type Bool :: True | False

-- 'a represents a generic type to be used within the type expression
type Option 'a :: Some 'a | None
type Either 'a :: Left 'a | Right 'a

type Point 'a :: 'a * 'a

type Shape 'a ::
    | Point Point 'a
    | Circle Point 'a * 'a
    | Rect Point 'a * 'a * 'a 

class Monad m ::
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
<program> ::= { <statement> }

<statement> ::= <type_declaration>
             | <class_declaration>
             | <instance_declaration>
             | <function_definition>
             | <constant_definition>
             | <comment>

<comment> ::= "--" <text>

<type_declaration> ::= "type" <type_name> [<type_param>] "::" <type_variant_list>

<type_variant_list> ::= <type_variant> { "|" <type_variant> }

<type_variant> ::= <constructor> [<type_expr_list>]

<constructor> ::= <identifier>

<type_expr_list> ::= <type_expr> { "*" <type_expr> }

<type_expr> ::= <type_name>
             | <type_param>
             | <constructor> <type_expr>
             | "(" <type_expr> ")"

<type_name> ::= <identifier>
<type_param> ::= "'" <identifier>

<class_declaration> ::= "class" <identifier> <identifier> "::" { <method_signature> }

<method_signature> ::= <identifier> "::" <type_expr> "=>" <type_expr>

<instance_declaration> ::= "instance" <identifier> <type_expr> "::" { <method_binding> }

<method_binding> ::= <identifier> ":=" <expression>

<function_definition> ::= <identifier> ":=" <expression>

<constant_definition> ::= <identifier> ":=" <literal>


<expression> ::= <lambda_expression>
              | <case_expression>
              | <let_expression>
              | <application>
              | <literal>
              | <tuple>
              | <list>
              | <identifier>

<lambda_expression> ::= <parameter_list> "=>" <expression>

<parameter_list> ::= <identifier> { "," <identifier> }

<case_expression> ::= "case" [<identifier>] "in" { "\\" <pattern> "=>" <expression> }

<let_expression> ::= "let" { <binding> } "in" <expression>

<binding> ::= <identifier> ":=" <expression>
           | <tuple_pattern> ":=" <tuple_expression>

<tuple_pattern> ::= "(" <identifier> { "," <identifier> } ")"

<application> ::= <expression> <expression>

<list> ::= "{" [ <expression> { ":" <expression> } ] "}"

<pattern> ::= <constructor> <identifier>
           | <constructor> "," <identifier>
           | <constructor>
           | <list_pattern>

<tuple_expression> ::= "(" <expression> { "," <expression> } ")"

<literal> ::= <number>
           | <string>

<identifier> ::= <letter> { <letter> | <digit> | "_" }
<text> ::= { any character except newline }
<number> ::= { <digit> } ["." <digit> { <digit> }]
<string> ::= "\"" { any character except "\"" } "\""

<letter> ::= "a" | ... | "z" | "A" | ... | "Z"
<digit> ::= "0" | ... | "9"

```
