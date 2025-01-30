# Jace Language

A Haskell inspired compiler that targets Lua.

Predefined:
- \<string\>

# Grammar

Still incomplete in BNF form.  Need to figure out how to represent binary expressions properly without too much recursion.

```
<identifier> ::= ([a-z] | [A-Z] | "_") ([a-z] | [A-Z] | [0-9] | "_")*
<integer> ::= [1-9] [0-9]*
<float> ::= ("0" "." [0-9]+) | ([1-9] [0-9]* "." [0-9]+)
<number> := <integer> | <float>

<literal> ::= <integer> | <float> | <string> | <set_literal>

<expression> ::= <func_call>
             | <expression> "+" <factor> | <expression> "-" <factor>
             | <factor>

<atom> := <number> | "(" <expression> ")"
<factor> := <atom> | <factor> "*" <atom>

<func_name> ::= <identifier>

<func_input_types> ::= <type_name> ("," <type_name>)*
<func_type_sig> ::= <func_name> "::" <func_input_types> "=>" <type_name>

<def_func> ::= <func_name> "::" (<func_expression> | <func_case_expression>)
<func_case> ::= <func_expression>
<func_case_expression> := "case\n" <func_case> ("\n" <func_case")*
<func_expression> ::= (<func_params> "=>" <expression>)
<func_params> ::= <identifier> ("," <identifier>)*

<func_argument> ::= <identifier> | <literal>
<func_arguments> ::= <func_argument> (" " <func_argument>)*
<func_call> ::= <func_name> " " <func_arguments>

<def_variable> ::= <identifier> ":" <type_name>? "=" (<identifer> | <literal>)

<type_name> := <identifier>

<set_prototype> := ("\t" <set_prototype_entry>)+
<set_prototype_entry> := <identifier> ":" <type_name>
<def_type> := "type" <type_name> "::" "\n" <set_prototype>
<type_union> := "type" <type_name> "::" <type> "|" <type> ("|" <type>)*

<set_literal> := "{" (<set_entry> ("," <set_entry>)*)? "}"
<set_entry> := <identifier> "=" (<identifier> | <literal>)

<class_name> := <identifier>
<class_var> := <identifier>
<class_vars> := <class_var> ("," <class_var>)*
<class_fn_id> := <identifier> | ("(" <operator> "))
<class_fn_sig> := <class_fn_id> "::" <class_vars> "=>" <type>
<class_fn_sigs> := <class_fn_sig> ("\n" <class_fn_sig>)*
<def_class> := "class" <class_name> <class_var> "::\n" <class_fn_signature>

<instance_arg> := <identifier>
<instance_args> := <instance_arg> ("," <instance_arg>)*
<instance_fn_def> := <class_fn_id> "|" <expression>
<instance_fn_defs> := <instance_fn_def> ("\n" <instance_fn_def>)*
<def_instance> := "instance" <type_name> <class_name> <instance_args> "::\n" <instance_fn_defs>

```

## Structure

A jace file always will follow the following structure

1) All Definitions
2) A single expression


```Haskell
MyModule :: mod mymodule

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
