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

Tuple = {}
Tuple.__type = "Tuple"
Tuple.Tuple = function(param1, param2)
    local self = setmetatable({}, Tuple)
    self.__tag = "Tuple"
    self._1 = param1
    self._2 = param2
    return self
end

sum = function(a, b)
return a + b
end
add = function(_1, _2)
    local function _b1(_1, _2)
        return Option.Some(1)
    end
    local function _b2(_1, _2)
        local a = _1._1
        local b = _2._1
        return Option.Some(sum(a, b))
    end
    local function _b3(_1, _2)
        local _ = _1
        local _ = _2
        return Option.None()
    end
    -- Case logic
    if _1.__tag == "Some" and _1._1 == 0 and _2.__tag == "Some" and _2._1 == 1 then
        return _b1(_1, _2)
    elseif _1.__tag == "Some" and true and _2.__tag == "Some" and true then
        return _b2(_1, _2)
    elseif true and true then
        return _b3(_1, _2)
    end
end

add2 = function(_1, _2)
    local function _b1(_1, _2)
        return Tuple.Tuple(0, 0)
    end
    local function _b2(_1, _2)
        local a = _1._1
        local b = _1._2
        local c = _2._1
        local d = _2._2
        return Tuple.Tuple(a + c, b + d)
    end
    -- Case logic
    if _1.__tag == "Tuple" and _1._1 == 0 and _1._2 == 1 and _2.__tag == "Tuple" and _2._1 == 1 and _2._2 == 0 then
        return _b1(_1, _2)
    elseif _1.__tag == "Tuple" and true and true and _2.__tag == "Tuple" and true and true then
        return _b2(_1, _2)
    end
end

print_option = function(_1)
    local function _b1(_1, _2)
        local a = _1._1
        return print("some:", a)
    end
    local function _b2(_1, _2)
        local None = _1
        return print("none")
    end
    -- Case logic
    if _1.__tag == "Some" and true then
        return _b1(_1, _2)
    elseif true then
        return _b2(_1, _2)
    end
end

print_tuple = function(_1)
    local function _b1(_1, _2)
        local a = _1._1
        local b = _1._2
        return print("tuple:", a, b)
    end
    local function _b2(_1, _2)
        local _ = _1
        return print("You shouldn't have done this.")
    end
    -- Case logic
    if _1.__tag == "Tuple" and true and true then
        return _b1(_1, _2)
    elseif true then
        return _b2(_1, _2)
    end
end

main = function()
    print("Hello, World!")
    opt = Option.Some(42)
    opt2 = Option.Some(43)
    opt3 = add(opt, opt2)
    print_option(opt3)
    none_opt = Option.None()
    print_option(none_opt)
    tuple1 = Tuple.Tuple(1, 2)
    tuple2 = Tuple.Tuple(3, 4)
    tuple3 = add2(tuple1, tuple2)
    print_tuple(tuple3)
end

main()