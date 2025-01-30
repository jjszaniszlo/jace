


--- factorial :: Integer => Integer
--- factorial :: case
---   0 => 1
---   n => n * factorial (n - 1)

local factorial
factorial = function(n)
  if n == 0 then
    return 1
  else
    return n * factorial(n - 1)
  end
end


--- sum :: Integer, Integer => Integer
--- sum :: a, b => a + b

--- implicitly a closure
local sum
sum = function(a)
  return function(b)
    return a + b
  end
end


---
--- sum2 :: (Integer, Integer) => Integer
--- sum2 :: a, b => a + b

--- implicitly a closure
local sum2
sum2 = function(a, b)
  return a + b
end
