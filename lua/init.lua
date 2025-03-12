-- This is the lua side of jace.

-- a functional library and runtime so I don't have to make one.
require "fun" ()

print(reduce(operator.add, 0, map(function(x) return x^2 end, range(100))))
