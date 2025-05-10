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
    name : String
    age : Int
    
-- union types may be defined as
type Bool :: True | False

-- union types may have polymoprhic parameters
type Either a b :: (Left a) | (Right b)

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
