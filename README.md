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

A jace module always will follow the following structure

1) Zero or more modules
2) zero or more definitions
3) final expression or main proc definition **if its the** entry point file.

```Haskell
-- import a module
import mymodule

-- all definitions use ::, which is the constant assignment operator.


type Foo ::
    ...

class Bar _ ::
    ...

instance Bar Foo _ ::
    ...

fooFunc :: Foo => Foo
fooFunc :: ... => ...

-- only in entry point module
main :: ()
main :: do
    ...
    end

-- any other module than entrypoint
let ... in expr

-- or

expr
```

## Types

These are the base types in Jace.

```Haskell
-- the base primitive types are
Number, Float, Integer, Bool, String
-- all sets are implicitly unioned with the "Set" type.
Set

-- Note that Number is simply a Float unioned with the Integer type.
-- Type unioning means that it can be constructed from both types, but not coerced into those types.
type Number :: Float | Integer

```

Non-primitive types are defined in two different ways. Set prototype definition and set alias definition.

```Haskell
-- set prototype definition
type Vector3 ::
    x : Number
    y : Number
    z : Number

-- set alias definition     (The two definitions ARE NOT EQUIVALENT)
type Vec3 :: [Number 3]

```

Types can be assigned to expressions explicitly or literally.  Either way, fields must be defined with their names.

```Haskell
type Vector3 ::
    x : Number
    y : Number
    z : Number

-- defines a constant value.
CONSTANT_VEC_3 :: {x = 1, y = 2, z = 3}

let
    up := {x = 0, y = 0, z = 1}
    down : Vector3 = {x = 0, y = 0, z = -1}
in up

```

For the other vec definition however, since it alias an array, it can be defined as the following

```Haskell
type Vec3 :: [Number 3]

CONSTANT_VEC_3 :: {1, 2, 3}

let
    up := {0, 0, 1}
    down : Vector3 = {0, 0, 1}
in up

```

## Function Types

Functions are first class citizens in Jace, so they are a valid type.  These types of functions are referred to as closures.

```Haskell
 
-- for example
-- This function takes a closure which takes in an integer and returns an integer.
-- it also takes in an Integer and returns an Integer.
-- the final expression in the function applies function f to integer i.
applyTo :: (Integer => Integer), Integer => Integer
applyTo :: func, int => func int

applyTo (a => a * 2) 4  -- returns 8.

```

## Case expression in functions.

Functions may accept a case expression.  A case expression matches the input values of the function and returns the expression which matches.
Pattern expressions may be used in case expressions.

```Haskell

-- all cases must be covered in some way.  in this case, the first param is only used if its 0, 1 or 2.
doOperation :: Integer, Integer => Integer
doOperation :: case
    0, n => n^2
    1, n => n^3
    2, n => n^4
    _, n => n

-- the case expression works really nicely for recursive functions.
factorial :: Integer => Integer
factorial :: case
    0 => 1
    n => n * factorial (n - 1)

```

## Pattern Expressions

Pattern expressions may be only used in the context of a case expression within a function.

```Haskell
type Vector3 ::
    x : Number
    y : Number
    z : Number

-- makes the vector's y value 0 essentially.
-- this matches all possible vaules of vector3 but discards the y value.
-- the set destructure pattern takes on the type of the associated param type.
groundVector :: Vector3 => Vector3
groundVector :: case
    {x, _, z} => {x = x, y = 0, z = z}

-- in this specific case, working with set arrays is much nicer.
 
type Vec3 :: [Number 3]

groundVec :: Vec3 => Vec3
groundVec :: case
    {x, _, z} => {x, 0, z}

-- another useful expression is the set selector.
-- the syntax is {first_element:rest_of_array}
-- it doesn't have to be used in case expressions, but they typically will, especially for recursive functions.
first :: [Number] => Number
first :: {f:_} => f

-- Speaking of recursive functions, we can now implement the map function.

map :: (Number => String), [Number] => [String]
map :: case
    _, {} => {}
    f, {x:xs} => {f x} | (map f xs)

-- this doesnt seem very useful though... it can only map a set array of numbers to a set array of strings...
-- luckily, generics can save the day! On to the next section.

```

## Generic Fn Params

Any lowercase type name, where types can be parsed is implicity a generic type.

```Haskell

-- this means that map can really be implemented *better* as this:
map :: (a => b), [a] => [b]
map :: case
    _, {} => {}
    f, {x:xs} => {f x} | (map f xs)

```

## Classes

Classes are really where things start to become more useful and clear.

```Haskell

-- first lets define a type.
type Foo ::
    bar : String,
    baz : Integer,

-- next, define a class with a method. The method in this case overloads the == operator.
-- this uses the generic param a to dictate that the input params are the same type.
class Equal a ::
    (==) :: a, a => Bool

-- implement the desired class for a type.
instance Equal Foo x y ::
    (==) => x.bar == y.bar && x.baz == y.baz

let
    fooIsh := {bar = "fizzbuzz", baz = 21}
    barIsh := {bar = "fizzbuzz", baz = 21}
in fooIsh == barIsh     -- returns true.

```

```Haskell
-- classes become even more useful when you use them as type params.
type Vector3 ::
    x : Number
    y : Number
    z : Number

type Vec2 :: [Number 2]

class Dot a b ::
    dot :: a, a => Number

-- set destructuring can be used here as well
instance Dot Vector3 {x1, y1, z1} {x2, y2, z2} ::
    dot => x1*x2+y1*y2+z1*z2

-- since array sets can be destructured the same way as sets, this is also valid and identical for that matter.
instance Dot Vec2 {x1, y1} {x2, y2} ::
    dot => x1*x2+y1*y2+z1*z2

let 
    -- notice that the same method applies to both set types!
    result1 := dot {x = 1, y = 2, z = 3} {x = 2, y = 3, z = 1}
    result2 := dot {1, 2} {3, 4}
    in {result1, result2}

```

```Haskell
type Vector3 ::
    x : Number
    y : Number
    z : Number

class Display a ::
    print :: a => Void

instance Display Vector3 {x, y, z} ::
    print => printfmt "({}, {}, {})" x y z

```

## Procedures

Procedures are essentially functions which may take in parameters but do not return a value.  They are impure.

They cannot be assigned to anything, nor can they be returned from functions.


```Haskell

-- a proc is really the only place where functions that don't have return values can be called.
-- a proc is a collection of statements and proc calls.


-- the () signifies the type of proc.
run :: ()
run :: do
    print "Hello World!"
    end

```

