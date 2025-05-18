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

add = function(_1, _2)
    local function _b1(_1, _2)
        local a = _1._1
        local b = _2._1
        return Option.Some(Either.Left(a + b))
    end
    local function _b2(_1, _2)
        local a = _1._1
        local b = _2._1
        return Option.Some(Either.Right(a + b))
    end
    local function _b3(_1, _2)
        local _ = _1
        local _ = _2
        return Option.None()
    end
    -- Case logic
    if _1.__tag == "Left" and true and _2.__tag == "Left" and true then
        return _b1(_1, _2)
    elseif _1.__tag == "Right" and true and _2.__tag == "Right" and true then
        return _b2(_1, _2)
    elseif true and true then
        return _b3(_1, _2)
    end
end

print_either = function(_1)
    local function _b1(_1, _2)
        local a = _1._1
        return print("left:", a)
    end
    local function _b2(_1, _2)
        local a = _1._1
        return print("right:", a)
    end
    -- Case logic
    if _1.__tag == "Left" and true then
        return _b1(_1, _2)
    elseif _1.__tag == "Right" and true then
        return _b2(_1, _2)
    end
end

print_option_either = function(_1)
    local function _b1(_1, _2)
        local a = _1._1
        return print_either(a)
    end
    local function _b2(_1, _2)
        local _ = _1
        return print("none")
    end
    -- Case logic
    if _1.__tag == "Some" and true then
        return _b1(_1, _2)
    elseif true then
        return _b2(_1, _2)
    end
end

main = function()
    foo_left = Either.Left(1)
    foo_right = Either.Right(2)
    foo_left2 = Either.Left(3)
    foo_right2 = Either.Right(4)
    left_added = add(foo_left, foo_left2)
    right_added = add(foo_right, foo_right2)
    left_add_right = add(foo_left, foo_right)
    right_add_left = add(foo_right, foo_left)
    print_option_either(left_added)
    print_option_either(right_added)
    print_option_either(left_add_right)
    print_option_either(right_add_left)
end

main()