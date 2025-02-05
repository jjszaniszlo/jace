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

<pred_expr> ::= <bin_expr> | <bool> | <fn_call>

<fn_call> ::= ( <ident> | <set_access> ) ( <primary> | <bool> )+
<fn_param> ::= <ident>
             | <set_selector>
             | <set_destructure>

<case_expr> ::= <case> (<fn_param> ("," <fn_param>")* => <expr>)+

<set_destructure> ::= "{" <ident> ("," <ident>)* "}"
<set_selector> ::= "{" <ident> ":" <ident> "}
<set_access> ::= <ident> "." <ident> ("." <ident>)*

<expr> ::= <bin_expr>
         | "if" <pred_expr> "then" <expr> ("elseif" <pred_expr> "then" <expr>)* "else" <expr>
         | "let" <stmt>* "in" <expr>
         | <fn_call>
         | <fn_param> ("," <fn_param>")* "=>" ( <expr> | <case_expr> )
         | "{" (<ident> "=" <expr>)+ ("," <ident> "=" <expr>)* "}"

<stmt> ::= <ident> ":=" <expr>
         | <ident> ("," <ident>)* := <set_destructure>
         | <set_destructure> = <expr> ("," <expr>)*
         | <ident> "=" <expr>
         | <set_access> "=" <expr>

<def> ::= "type" <ident> "::" <ident> ":" <ident> (<ident> ":" <ident>)*
        | "class" <ident> <ident>+ "::" ( <ident> "::" ( <ident> ( "," <ident> )* "=>" <ident> )+ )+
        | "class" <ident> <ident>+ "::" ( "(" <bin_op> ")" "::" ( <ident> "," <ident> "=>" <ident> )+ )+
        | "instance" <ident> <ident> <ident>* "::" <ident> "=>" ( <expr> | <case_expr> )
        | "module" "::" <ident>

<reserved_idents> ::= "class" | "type" | "instance" | "module" | "true"
                    | "false" | "if" | "then" | "else" | "elseif" | "let" | "in"
                    | "case"
```

## Structure

A jace file always will follow the following structure

1) Zero or more modules
2) All Definitions
3) A single expression


```Haskell

mod mymodule

-- all definitions which can be remembered by the :: syntax

type Foo ::
    ...

class Bar _ ::
    ...

instance Foo Bar _ ::
    ...

fooFunc :: Foo => Foo
fooFunc :: {} => {}

-- then a single expression as an entry point to the program or module.
-- for example this:
-- fooFunc {}

-- or this
let
    x : Foo = {} 
in fooFunc x
```

A snippet of what all that looks like.

```Haskell
type Person ::
  name : String
  age : Integer

class Equal a ::
    (==) :: a, a => Bool

instance Person Equal x y ::
    (==) => x.name == y.name && x.age == y.age

-- bill will infer the type of Person, since it has two members which match the names and types of the type "Person"
let
    bill := {name = "bill", age = 34}
in { person_of_interest = bill }
-- the final expression is a set with a member, which is a set of type "Person"
```

Importing this module is as simple as adding it to the definitions.

Importing a module means importing all of its definitions, under the namespace of the module name.

```Haskell
mod Person

let 
    joe := {name = "joe", age = 23}
in joe == Person.person_of_interest
-- this will finally return false since the two sets aren't comparable
```
