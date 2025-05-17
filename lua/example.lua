Option = {}
Option.__type = "Option"
Option.Some = function(param1)
    local self = setmetatable({}, Option)
    self.__tag = "Some"
    self._1 = param1
    return self
end
Option.None = function()
    local self = setmetatable({}, Option)
    self.__tag = "None"
    return self
end

Either = {}
Either.__type = "Either"
Either.Left = function(param1)
    local self = setmetatable({}, Either)
    self.__tag = "Left"
    self._1 = param1
    return self
end
Either.Right = function(param1)
    local self = setmetatable({}, Either)
    self.__tag = "Right"
    self._1 = param1
    return self
end

Tuple = {}
Tuple.__type = "Tuple"
Tuple.Tuple = function(param1, param2, param3, param4)
    local self = setmetatable({}, Tuple)
    self.__tag = "Tuple"
    self._1 = param1
    self._2 = param2
    self._3 = param3
    self._4 = param4
    return self
end

sum = function(a, b)
    return a + b
end

main = function()
    print("Hello, World!")
end

main()