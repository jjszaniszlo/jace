

```Haskell

type Bool :: True | False

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
        \ Some v => f v
        \ None => None
    return := a => Some a

-- another way which you could define a monad.  The first way is better
-- because this wouldn't be able to curry.
class Monad2 m ::
    bind :: (m 'a) * ('a => m 'a) => m 'a
    return :: 'a => m 'a

instance Monad Option ::
    bind :=
    case
        \ Some v, f => f v
        \ None, _ => None
    return := a => Some a

MATH_PI := 3.14
        
-- all comments above the methods are optional type annotations.
-- mostly useful for forward declarations in niche scenarios where you have functions
-- call each other.

-- map :: ('a => 'b) * ['a] => ['b]
map :=
case
    \ _, {} => {}
    \ f, {x:xs} => f x : map f xs
    
-- map :: ('a => 'b) => ['a] => ['b]
map := f =>
case
    \ {} => {}
    \ {x:xs} => f x : map f xs

-- sum :: 'a * 'a => 'a
sum := a, b => a + b

-- sum :: 'a => 'a => 'a
sum := a => b => a + b

-- something :: 'a => 'a
something := a =>
    let
        a2 := a * a
        a3 := a * a * a
    in
    print a2 a3;
    a2 + a3

-- do_something2 :: 'a => 'a
do_something2 := a =>
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
main := _ =>
    let
        a := 69;
        b := 10;
        (c, d) := (34, 45);
    in
    print "hello world!" a b;

```
