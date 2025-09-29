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

factorial = function(_1)
    local function _b1(_1, _2)
        return 1
    end
    local function _b2(_1, _2)
        local n = _1
        return n * factorial(n - 1)
    end
    if _1 == 0 then
        return _b1(_1, _2)
    elseif true then
        return _b2(_1, _2)
    end
end

fibonacci = function(_1)
    local function _b1(_1, _2)
        return 0
    end
    local function _b2(_1, _2)
        return 1
    end
    local function _b3(_1, _2)
        local n = _1
        return fibonacci(n - 1) + fibonacci(n - 2)
    end
    if _1 == 0 then
        return _b1(_1, _2)
    elseif _1 == 1 then
        return _b2(_1, _2)
    elseif true then
        return _b3(_1, _2)
    end
end

main()