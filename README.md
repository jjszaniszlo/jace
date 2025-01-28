# Jace Language

A Haskell inspired compiler that targets Lua.

Predefined string
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
A snippet of what all that looks like.

```Haskell
type Person ::
  name : String
  age : Integer

john := {name = "John", age = 21}

updateAge :: Person, Integer => Person
updateAge :: case
  {name, age}, n => {name=name, age=age+n}
  {name, age}, 1 => {name=name, age=age+1}

-- a class essentially determins a function prototype for the operator.
class Equal a :: 
  (==) :: a, a => Bool
  sameAge :: a, a => Bool
  sameName :: a, a => Bool

-- an instance implements a class's prototype
-- slightly different than function syntax, where x and y are implicitly params of the implemented functions.
instance Person Equal x y ::
  (==) => x.name == y.name && x.age == y.age
  sameAge => x.age == y.age
  sameName => x.name == y.name

-- sets of the Person type can now be compared.
john2 := {name = "John", age = 21}
harry := {name = "Harry", age = 21}

john == john2 -- outputs: true
john == harry -- outputs: false

-- the classes act as a means for ad-hoc polymorphism, for both operators and functions.
type Foo ::
  bar : String

instance Foo Equal x y ::
  (==) => strEqual x.bar y.bar

-- type unions
type Number :: Integer | Float | Double

-- function currying
sum :: Number, Number => Number
sum :: a, b =>  c + d

-- addOne is a curried function of sum
addOne :: sum 1

addOne 2 -- outputs: 3

-- higher order functions
numSet := {1, 2, 3, 4}

addTwoToSet :: [Number] => [Number]
addTwoToSet :: case 
  {} => {}
  s => map (a => a + 2) s
```

Here's what the lua output could look like.

```Lua
local Person = {
    _type = "_Person"
}
Person.__index = Person
Person.init = function(name, age)
    return setmetatable({
        name = name,
        age = age
    }, Person)
end

john = Person.init("John", 21)

local updateAge = {
    _caseSignature1 = function(person, n)
        return {
            name = person.name,
            age = age + n,
        }
    end,
    _caseSignature2 = function(person)
        return {
            name = person.name,
            age = age + 1
        }
    end,
}
updateAge = setmetatable(updateAge, {
    __call = function(...) 
        if pcall(function()
            local args = {...}
            if updateAge[__GetFunctionSignature(args)] ~= nil then
                return updateAge[__GetFunctionSignature(args)](...)
            end
        end) == false then
            error("Unknown case in case expression!")
        end
    end,
})

Person.__eq = function(a_1, a_2)
    return a_1.name == a_2.name && a_1.age == a_2.age
end


```
