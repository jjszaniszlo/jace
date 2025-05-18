# Jace Language

A Haskell inspired compiler that targets Lua.

# Why this language design

I've programmed a bit here and there in Haskell and OCaml and found them interesting enough to want to learn how they work.  A good way for me to learn is by doing and so attempting to build a language similar to them is a good way for me to learn.


## Structure

A jace module always will follow the following structure

- zero or more definitions
- main entry procedure 


```Haskell
-- main procedure
def main :: ()
    print "Hello World!"
```

Please look to the examples in the `examples/` directory for more language features and examples.
Within this directory there is also a set of "guides" for various language features, these are the numbered files 1-4.jc.

# Snippets

## Procedure Definition
A procedure is a function with no arguments or return type. They are treated like functions internally but just have a name to differentiate.
```Haskell
def main :: ()
    print "Hello World!"
```
## Function Definition
A function is defined with it's signature followed by its body.
```Haskell
def foo :: Integer => ()
do
    a => print "My number is:" a
```

## Function/Procedure calling
A function is called by typing its name followed by arguments.  A function call expression is accepted anywhere an expression is accepted.

A procedure is called by typing its name, followed by a bang `!`.

```Haskell

def square :: Integer => Integer
do
    a => a ^ 2

def my_cool_proc :: ()
    print "cool proc"

def main :: ()
    my_cool_proc!
    print "2 squared is" (square 2)

```

## Type definition

A type is defined by a name then a series of constructors.

```Haskell

-- a useful type.
type Option a :: Some a | None

```

## Type Construction

A type can be constructed in a similar way to how functions are called.

```Haskell
type Option a :: Some a | None

def main :: ()
    opt_21 := Option 21
    print opt_21 -- will print `table` followed by a hash.  Further down on how to print nicely.

```

## Function with pattern matching

A function can use the `case` keyword to match against different types and literal values.

```Haskell
type Option a :: Some a | None

def maybe_double :: Option Integer => Option Integer
case
|   Some a => a * 2
|   None => None

```

A silly example that shows using a literal.
```Haskell
type Option a :: Some a | None

def maybe_double :: Option Integer => Option Integer
case
|   Some 0 => 0
|   Some a => a * 2
|   None => None

```

## More on pattern matching

With more complex function signatures, pattern matching is still possible as shown here.

```Haskell

type Option a :: Some a | None
type Either a b :: Left a | Right b

-- if both are the same side, add them, otherwise don't.
def add_eithers :: Either a b, Either a b => Option (Either a b)
case
|   Left a, Left b => Some (Left (a + b))
|   Right a, Right b => Some (Right (a + b))
|   _, _ => None

```

## Let-In

Let in can be used to introduce localized bindings for a function.

```Haskell

def area_circle :: Float => Float
do
    r => let pi := 3.14159 in pi*r^2
```

# Known Limitations

```Haskell
-- if you try to avoid using parenthesis around function calls,
-- the parser won't be able to properly disambiguate them.
def main :: ()
    print "a few numbers" 1 2 3
    print 2 5 6
    -- the first print will accept the second print statement
    -- as an argument.  So the output will be:
    -- print "a few numbers" 1 2 3 <builtin-function>
```

```Haskell
type Option a :: Some a | None

def print_option :: Option a => ()
case
|   Some a => print "some:" a
|   None => print "none"

-- there is no way to properly destruture a type without using a function
-- with pattern matching
def main :: ()
    opt21 := Option 21
    print opt21   -- this prints 'table' followed by a hash.
    print_option opt21 -- this prints '"some:"  21'
```

# Compilation Instructions
To build simply use cargo:
```
cargo build
```

# Running the Compiler

To run a program you may use cargo or the built binary.
```
cargo run run path_to_file.jc
-- OR
path/to/jace run path_to_file.jc
```

To build a program to see its `lua`, you can use both cargo and the built binary again.
```
cargo run build path_to_file.jc
-- OR
path/to/jace build path_to_file.jc
```

# Running Tests/Coverage

To run tests.
```
cargo test
```

To see code coverage.  You must have `llvm-cov` installed by the cargo package manager.

```
cargo llvm-cov
-- OR for HTML
cargo llvm-cov --html
```

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