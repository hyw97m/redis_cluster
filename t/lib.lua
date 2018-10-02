
local redis = require "resty.redis"
local rediscluster = require "resty.redis_cluster"
local json_encode   = require("cjson").encode

local concat = table.concat


local _M = {}


function _M.ncli()
    local red, err = redis:new()
    if not red then
        return nil, err
    end

    local ok, err = red:connect("127.0.0.1", 6379)
    if not ok then
        return nil, err
    end

    return red
end


function _M.ncluster()
    local config = {
        name = "name",
        servers = {
            { "127.0.0.1", 7001 },
            { "127.0.0.1", 7002 },
            { "127.0.0.1", 7003 },
            { "127.0.0.1", 7004 },
            { "127.0.0.1", 7005 },
            { "127.0.0.1", 7006 },
        },
        -- password        = "abc",
        idle_timeout    = 1000,
        pool_size       = 200,
    }

    return rediscluster:new(config)
end


function _M.chktab(tab1, tab2, flag)
    local str1 = json_encode(tab1)
    local str2 = json_encode(tab2)
    if flag then
        return concat({ str1, str2, tostring(str1 == str2) }, " ")
    else
        return str1 == str2
    end
end

return _M
