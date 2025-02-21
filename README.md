# Jace Language

A Haskell inspired compiler that targets Lua.

Predefined:
- \<string\>

# Grammar

## Some notes about the EBNF.
The EBNF grammar doesn't take into account operator precedence because the parser uses pratt parsing for operators, so its not inherently needed to be a part of the grammar.

```
<ident> ::= ([a-z] | [A-Z] | "_") ([a-z] | [A-Z] | [0-9] | "_")*
<integer> ::= [1-9] [0-9]*
<float> ::= ("0" "." [0-9]+) | ([1-9] [0-9]* "." [0-9]+)
<number> ::= <integer> | <float>
<bool> ::= "true" | "false"

<bin_op> ::= "==" | "!=" | ">" | ">=" | "<" | "<=" | "+" | "-" | "*" | "/" | "&&" | "||" 
<bin_expr> ::= <primary> (<bin_op> <primary>)*
<primary> ::= "(" <expr> ")" | <number> | <ident> | <bool> | "-" <primary> | <set_access>

<fn_call> ::= ( <ident> | <module_access> ) <primary>+
<fn_param> ::= <ident>
             | <set_selector>
             | <set_destructure>
             | <type_constructor>

<case_expr> ::= <case> <expr>? (<fn_param> ("," <fn_param>")* => <expr>)+

<set_destructure> ::= "{" <ident> ("," <ident>)* "}"
<set_selector> ::= "{" <ident> ":" <ident> "}
<set_access> ::= <ident> "." <ident> ("." <ident>)*
<module_access> ::= <ident> "." <ident> ("." <ident>)*

<type_constructor> ::= <ident> <expr> (<expr>)*

<expr> ::= <bin_expr>
         | "if" <expr> "then" <expr> ("elseif" <pred_expr> "then" <expr>)* "else" <expr>
         | "let" <stmt>* "in" <expr>
         | <fn_call>
         | <fn_param> ("," <fn_param>")* "=>" ( <expr> | <case_expr> )
         | "{" (<ident> "=" <expr>)+ ("," <ident> "=" <expr>)* "}"
         | <type_constructor>

<stmt> ::= <ident> ":=" <expr>
         | <ident> ("," <ident>)* ":=" <set_destructure>
         | <set_destructure> "=" <expr> ("," <expr>)*
         | <ident> (", <ident>)* "=" <fn_call>
         | <ident> "=" <expr>
         | <set_access> "=" <expr>

<type> ::= <sum_type>
<sum_type> ::= <product_type> ("|" <product_type>)*
<product_type> :: <primary_type> (<primary_type>)*
<primary_type> ::= <ident> | "(" <type> ")"

<algebraic_type_def> ::= "type" <ident> <ident>* "::" <type>

<def> ::= "type" <ident> "::" <ident> ":" <ident> (<ident> ":" <ident>)*
        | <algebraic_type_def>
        | "class" <ident> <ident>+ "::" ( <ident> "::" ( <ident> ( "," <ident> )* "=>" <ident> )+ )+
        | "class" <ident> <ident>+ "::" ( "(" <bin_op> ")" "::" ( <ident> "," <ident> "=>" <ident> )+ )+
        | "instance" <ident> <ident> <ident>* "::" <ident> "=>" ( <expr> | <case_expr> )
        | "module" "::" <ident>

<reserved_idents> ::= "class" | "type" | "instance" | "module" | "true"
                    | "false" | "if" | "then" | "else" | "elseif" | "let" | "in"
                    | "case"
```

## Structure

A jace module always will follow the following structure

1) Zero or more modules
2) zero or more definitions

Additionally, the entry point module must contain a main procedure.

```Haskell
-- main procedure
def main :: ()
    print "Hello World!"
```

## Small overview

Here is an abstracted overview of base language features.

```Haskell
```

## Types

```Haskell
-- the base "primitive" types are
Float, Integer, Bool, String

-- set or record types are be defined as
type Person ::
    Person
        name : String
        age : Integer
    
-- tagged union types may be defined as
type Bool :: True | False

-- tagged union types may have polymoprhic parameters
type Either a b :: Left a | Right b

type Vector a :: Vector3 a a a | Vector2 a a 

-- records may be a part of the type algebra as well.
type Mammal ::
    Dog
        name : String
        bark_text : String
    |
    Cat
        name : String
        meow_text : String 
        

-- types may be aliased as so
type Vector3 :: [Float 3]   -- array of 3 floats.
```

## Constants, Variables and Assignment

Variables, defined in Jace as a symbol containing a value, may not be top level.  However, constants may be defined top level and are defined using '::' instead of ':='.  Variables may be defined with in 'let-in' blocks and within procedures.

```Haskell

-- this is ok
const MATH_PI :: 3.14

def myComputation :: Integer, Integer => Integer
    a, b =>
        let
            -- this is a local variable to the function's scope
            my_result := a + b
        in my_result

-- within procedure
def main :: ()
    foo := 10 

    -- multiple assignment
    a, b := 21, 22
    
    print a b       -- prints: 21    22
    
    -- set destructuring
    my_set := {foo = "Nice", bar = "...Nice"}
    {first, second} := my_set
    
    print first second  -- prints: first    second

```

## Algebraic Types

Jace has support for algebraic types. 

```Haskell
-- this is a basic sum type
type Bool :: True | False

-- a product type is defined as so
type Vector :: Vector3 Float Float Float
--     [1]   |   [2]  |       [3]       |
-- 1) Type 
-- 2) Variant Constructor
-- 3) Value constructor

```

## Operator Overloading and custom operators

```Haskell

class Equal a ::
    (==) :: a, a => Bool 

type Vector a :: Vector3 a a a | Vector2 a a

instance Equal Vector ::
    a (==) b :: case
        (Vector3 x1 y1 z1), (Vector3 x2 y2 z2) => x1==x2 && y1==y2 && z1==z2;
        (Vector2 x1 y1),    (Vector3 x2 y2)    => x1==x2 && y1==y2;

type Person ::
    Person
        name : String
        age : Integer

instance Equal Person ::
    (==) :: p1, p2 => p1.name == p2.name && p1.age == p2.age

-- custom operator
class Monad m ::
    (>==) :: (m a), (a => (m b)) => (m b)
    return :: a => (m a)

type Option a :: Some(a) | None

instance Monad Option ::
    a (>==) b :: case
        None, _ => None;
        (Some x), f => f x;
    return :: a => Some a

```
