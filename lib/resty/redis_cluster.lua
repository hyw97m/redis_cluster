

local bit   = require "bit";
local json_encode   = require("cjson").encode


local bxor      = bit.bxor
local band      = bit.band
local lshift    = bit.lshift
local rshift    = bit.rshift

local str_len   = string.len
local str_sub   = string.sub

local concat    = table.concat
local tbl_insert= table.insert

local ngx_log   = ngx.log
local ERR       = ngx.ERR
local WARN       = ngx.WARN


local crc16tab  = {
    0x0000, 0x1021, 0x2042, 0x3063, 0x4084, 0x50a5, 0x60c6, 0x70e7,
    0x8108, 0x9129, 0xa14a, 0xb16b, 0xc18c, 0xd1ad, 0xe1ce, 0xf1ef,
    0x1231, 0x0210, 0x3273, 0x2252, 0x52b5, 0x4294, 0x72f7, 0x62d6,
    0x9339, 0x8318, 0xb37b, 0xa35a, 0xd3bd, 0xc39c, 0xf3ff, 0xe3de,
    0x2462, 0x3443, 0x0420, 0x1401, 0x64e6, 0x74c7, 0x44a4, 0x5485,
    0xa56a, 0xb54b, 0x8528, 0x9509, 0xe5ee, 0xf5cf, 0xc5ac, 0xd58d,
    0x3653, 0x2672, 0x1611, 0x0630, 0x76d7, 0x66f6, 0x5695, 0x46b4,
    0xb75b, 0xa77a, 0x9719, 0x8738, 0xf7df, 0xe7fe, 0xd79d, 0xc7bc,
    0x48c4, 0x58e5, 0x6886, 0x78a7, 0x0840, 0x1861, 0x2802, 0x3823,
    0xc9cc, 0xd9ed, 0xe98e, 0xf9af, 0x8948, 0x9969, 0xa90a, 0xb92b,
    0x5af5, 0x4ad4, 0x7ab7, 0x6a96, 0x1a71, 0x0a50, 0x3a33, 0x2a12,
    0xdbfd, 0xcbdc, 0xfbbf, 0xeb9e, 0x9b79, 0x8b58, 0xbb3b, 0xab1a,
    0x6ca6, 0x7c87, 0x4ce4, 0x5cc5, 0x2c22, 0x3c03, 0x0c60, 0x1c41,
    0xedae, 0xfd8f, 0xcdec, 0xddcd, 0xad2a, 0xbd0b, 0x8d68, 0x9d49,
    0x7e97, 0x6eb6, 0x5ed5, 0x4ef4, 0x3e13, 0x2e32, 0x1e51, 0x0e70,
    0xff9f, 0xefbe, 0xdfdd, 0xcffc, 0xbf1b, 0xaf3a, 0x9f59, 0x8f78,
    0x9188, 0x81a9, 0xb1ca, 0xa1eb, 0xd10c, 0xc12d, 0xf14e, 0xe16f,
    0x1080, 0x00a1, 0x30c2, 0x20e3, 0x5004, 0x4025, 0x7046, 0x6067,
    0x83b9, 0x9398, 0xa3fb, 0xb3da, 0xc33d, 0xd31c, 0xe37f, 0xf35e,
    0x02b1, 0x1290, 0x22f3, 0x32d2, 0x4235, 0x5214, 0x6277, 0x7256,
    0xb5ea, 0xa5cb, 0x95a8, 0x8589, 0xf56e, 0xe54f, 0xd52c, 0xc50d,
    0x34e2, 0x24c3, 0x14a0, 0x0481, 0x7466, 0x6447, 0x5424, 0x4405,
    0xa7db, 0xb7fa, 0x8799, 0x97b8, 0xe75f, 0xf77e, 0xc71d, 0xd73c,
    0x26d3, 0x36f2, 0x0691, 0x16b0, 0x6657, 0x7676, 0x4615, 0x5634,
    0xd94c, 0xc96d, 0xf90e, 0xe92f, 0x99c8, 0x89e9, 0xb98a, 0xa9ab,
    0x5844, 0x4865, 0x7806, 0x6827, 0x18c0, 0x08e1, 0x3882, 0x28a3,
    0xcb7d, 0xdb5c, 0xeb3f, 0xfb1e, 0x8bf9, 0x9bd8, 0xabbb, 0xbb9a,
    0x4a75, 0x5a54, 0x6a37, 0x7a16, 0x0af1, 0x1ad0, 0x2ab3, 0x3a92,
    0xfd2e, 0xed0f, 0xdd6c, 0xcd4d, 0xbdaa, 0xad8b, 0x9de8, 0x8dc9,
    0x7c26, 0x6c07, 0x5c64, 0x4c45, 0x3ca2, 0x2c83, 0x1ce0, 0x0cc1,
    0xef1f, 0xff3e, 0xcf5d, 0xdf7c, 0xaf9b, 0xbfba, 0x8fd9, 0x9ff8,
    0x6e17, 0x7e36, 0x4e55, 0x5e74, 0x2e93, 0x3eb2, 0x0ed1, 0x1ef0
}


local function crc16(str)
    local crc = 0
    for i=1, #str do
        crc = bxor(lshift(crc, 8), crc16tab[band(bxor(rshift(crc, 8), str:byte(i)), 0x00ff) + 1]);
    end

    return crc
end


local function keyhashslot(key)
    local s, e
    local keylen = str_len(key)
    for i = 1, keylen do
        s = i
        if key:byte(s) == 123 then break end
    end

    if s == keylen then
        return band(crc16(key), 0x3fff) + 1
    end

    for i = s + 1, keylen do
        e = i
        if key:byte(e) == 125 then break end
    end

    if e == keylen or e == s + 1 then
        return band(crc16(key), 0x3fff) + 1
    else
        return band(crc16(str_sub(key, s + 1, e - 1)), 0x3fff) + 1
    end
end


local ok, new_tab = pcall(require, "table.new")
if not ok or type(new_tab) ~= "function" then
    new_tab = function (narr, nrec) return {} end
end


local redis = require "resty.redis"
redis.add_commands("cluster")


local commands = {
    "append",               --[["auth",]]           --[["bgrewriteaof",]]
    --[["bgsave",]]         "bitcount",             --[["bitop",]]
    "blpop",                "brpop",
    --[["brpoplpush",]]     --[["client",]]         --[["config",]]
    --[["dbsize",]]
    --[["debug",]]          "decr",                 "decrby",
    --[["del",]]            --[["discard",]]        --[["dump",]]
    --[["echo",]]
    --[["eval",]]           --[["exec",]]           "exists",
    "expire",               "expireat",             --[["flushall",]]
    --[["flushdb",]]        "get",                  "getbit",
    "getrange",             "getset",               "hdel",
    "hexists",              "hget",                 "hgetall",
    "hincrby",              "hincrbyfloat",         "hkeys",
    "hlen",
    "hmget",                "hmset",                --[["hscan",]]
    "hset",
    "hsetnx",               "hvals",                "incr",
    "incrby",               "incrbyfloat",          --[["info",]]
    --[["keys",]]
    --[["lastsave",]]       "lindex",               "linsert",
    "llen",                 "lpop",                 "lpush",
    "lpushx",               "lrange",               "lrem",
    "lset",                 "ltrim",                --[["mget",]]
    --[["migrate",]]
    --[["monitor",]]        --[["move",]]           --[["mset",]]
    --[["msetnx",]]         --[["multi",]]          --[["object",]]
    "persist",              "pexpire",              "pexpireat",
    --[["ping",]]           "psetex",               --[["psubscribe",]]
    "pttl",
    --[["publish",]]        --[["punsubscribe",]]   --[["pubsub",]]
    --[["quit",]]
    --[["randomkey",]]      --[["rename",]]         --[["renamenx",]]
    --[["restore",]]
    "rpop",                 --[["rpoplpush",]]      "rpush",
    "rpushx",               "sadd",                 --[["save",]]
    "scan",                 "scard",                --[["script",]]
    --[["sdiff",]]          --[["sdiffstore",]]
    --[["select",]]         "set",                  "setbit",
    "setex",                "setnx",                "setrange",
    --[["shutdown",]]       --[["sinter",]]         --[["sinterstore",]]
    --[["sismember",]]      --[["slaveof",]]        --[["slowlog",]]
    "smembers",             --[["smove",]]          "sort",
    "spop",                 --[["srandmember",]]    "srem",
    "sscan",
    --[["strlen",]]         --[["subscribe",]]      --[["sunion",]]
    --[["sunionstore",]]    --[["sync",]]           --[["time",]]
    "ttl",
    --[["type",]]           --[["unsubscribe",]]    --[["unwatch",]]
    --[["watch",]]          "zadd",                 "zcard",
    "zcount",               "zincrby",              --[["zinterstore",]]
    "zrange",               "zrangebyscore",        "zrank",
    "zrem",                 "zremrangebyrank",      "zremrangebyscore",
    "zrevrange",            "zrevrangebyscore",     "zrevrank",
    --[["zscan",]]
    "zscore",               --[["zunionstore",]]    --[["evalsha"]]
}


local mcommands = {
    mset = { cmd = "set", typ = "update" },
    mget = { cmd = "get", typ = "list" },
    del  = { cmd = "del", typ = "incr" },
    msetnx  = { cmd = "setnx", typ = "incr" },
}

local cache_slots = new_tab(0, 0x4000)


local function _fetch_slots(self, server)
    local slots = {}
    for _, svr in ipairs(server) do
        local red_cli = redis:new()
        local ok, err = red_cli:connect(svr[1], svr[2])
        if not ok then
            ngx_log(WARN, tostring(err))
        else
            local info, err = red_cli:cluster("slots")
            if info and type(info) == "table" and #info > 0 then
                for i=1, #info do
                    local item = info[i]
                    for slot = item[1], item[2] do
                        slots[slot + 1] = {
                            host = item[3][1],
                            port = item[3][2],
                        }
                    end
                end

                return slots
            else
                ngx_log(WARN, tostring(err))
            end
        end

        red_cli:set_keepalive(self.config.idle_timeout, self.config.pool_size)
    end

    return nil, "failed to init redis cluster"
end


local _M = {}


function _M.new(self, conf)
    local config = {
        name    = conf.name or "dev",
        server  = conf.server,
        idle_timeout= conf.idle_timeout or 1000,
        pool_size   = conf.pool_size or 100,
    }

    local mt = setmetatable({ config = config, link = {} }, { __index = _M })
    if not cache_slots[config.name] then
        local res, err = _fetch_slots(mt, config.server)
        if not res then
            return nil, err
        end

        cache_slots[config.name] = res
    end

    return mt
end


local function _do_retry(self, host, port, cmd, key, ...)
    local red_cli = redis:new()
    local ok, err = red_cli:connect(host, port)
    if not ok then
        return nil, err
    end

    local res, err
    if ... then
        res, err = red_cli[cmd](red_cli, key, ...)
    else
        res, err = red_cli[cmd](red_cli, key)
    end

    -- ngx_log(ERR, concat({host, port, cmd, key, tostring(res), tostring(err)}, "|"))

    red_cli:set_keepalive(self.config.idle_timeout, self.config.pool_size)

    return res, err
end


local function _do_cmd(self, cmd, key, ...)
    if self._reqs then
        if not self[cmd] then
            return nil, "nofound cmd"
        end

        self._group[#self._group + 1] = { size = 1 }
        tbl_insert(self._reqs, { cmd = cmd, key = key, args = { ... } })
        return true
    end

    local slots = cache_slots[self.config.name]
    local slot = keyhashslot(key)
    local host, port = slots[slot].host, slots[slot].port

    local red_cli = redis:new()
    local ok, err = red_cli:connect(host, port)
    if not ok then
        return nil, err
    end

    local res, err = red_cli[cmd](red_cli, key, ...)
    if not res then
        if err and str_sub(err, 1, 5) == "MOVED" then
            local res, err = _fetch_slots(self, self.config.server)
            if res then
                cache_slots[self.config.name] = res
            end

        end

        return nil, err
    end

    red_cli:set_keepalive(self.config.idle_timeout, self.config.pool_size)

    return res, err
end


for i = 1, #commands do
    local cmd = commands[i]

    _M[cmd] =
        function (self, ...)
            local res, err
            for i = 1, 2 do
                res, err =_do_cmd(self, cmd, ...)
                if res then
                    break
                end
            end

            return res, err
        end
end


function _M.init_pipeline(self)
    self._reqs = {}
    self._group = {}
end


local function merge(group, res)
    local ret = {}
    local pp = 0
    for _, item in ipairs(group) do
        local data
        if item.typ == "incr" then
            data = 0
            for cp = pp + 1, pp + item.size do
                data = data + ((res[cp] == false) and 0 or res[cp])
            end

        elseif item.typ == "update" or item.typ == "list" then
            data = {}
            for cp = pp + 1, pp + item.size do
                data[#data + 1] = res[cp]
            end

        else
            data = res[pp + 1]
        end

        pp = pp + item.size
        ret[#ret + 1] = data
    end

    return ret
end


function _M.commit_pipeline(self)
    local reqs = self._reqs
    local group = self._group
    if not reqs then
        return nil, "no pipeline"
    end

    local refetch

    self._reqs = nil
    self._group = nil

    local slots = cache_slots[self.config.name]

    local map = {}
    for idx, req in ipairs(reqs) do
        local slot = keyhashslot(req.key)
        local host, port = slots[slot].host, slots[slot].port

        local svrkey = concat({ host, port }, ":")
        if not map[svrkey] then
            map[svrkey] = { server = { host = host, port = port }, reqs = {}, idxs = {} }
        end

        local mreqs = map[svrkey].reqs
        local midxs = map[svrkey].idxs
        mreqs[#mreqs + 1] = req
        midxs[#midxs + 1] = idx
    end

    local ret = new_tab(0, #reqs)

    for svrkey, item in pairs(map) do
        local red_cli = redis:new()
        local ok, err = red_cli:connect(item.server.host, item.server.port)
        if not ok then
            return nil, err
        end

        red_cli:init_pipeline()
        local reqs = item.reqs
        for _, req in pairs(reqs) do
            if req.args and #req.args > 0 then
                red_cli[req.cmd](red_cli, req.key, unpack(req.args))
            else
                red_cli[req.cmd](red_cli, req.key)
            end
        end

        local res, err = red_cli:commit_pipeline()
        if not res then
            return nil, err
        end

        red_cli:set_keepalive(self.config.idle_timeout, self.config.pool_size)

        for i, idx in ipairs(item.idxs) do
            if type(res[i]) == "table" and res[i][1] == false and str_sub(res[i][2], 1, 5) == "MOVED" then
                local m, err = ngx.re.match(res[i][2], "MOVED [0-9]+ ([0-9.]+):([0-9]+)")
                if type(m) == "table" then
                    local res, err
                    if reqs[i].args and #reqs[i].args > 0 then
                        res, err = _do_retry(self, m[1], m[2], reqs[i].cmd, reqs[i].key, unpack(reqs[i].args))
                    else
                        res, err = _do_retry(self, m[1], m[2], reqs[i].cmd, reqs[i].key)
                    end

                    ret[idx] = res or false
                else
                    ret[idx] = false
                end

                refetch = true
            else
                ret[idx] = res[i]
            end
        end

        if refetch then
            local res, err = _fetch_slots(self, self.config.server)
            if res then
                cache_slots[self.config.name] = res
            end

            refetch = nil
        end

    end

    if #group > 0 then
        ret = merge(group, ret)
    end

    return ret
end


function _M.cancel_pipeline(self)
    self._reqs = nil
    self._group = nil
end


local function _do_mcmd(self, cmd, typ, ...)
    local pipeline = self._reqs and true or nil
    if not pipeline then
        _M.init_pipeline(self)
    end

    local args = { ... }
    if typ == "update" then
        self._group[#self._group + 1] = { size = #args / 2, typ = typ }
        for i = 1, #args, 2 do
            tbl_insert(self._reqs, { cmd = cmd, key = args[i], args = { args[i + 1] } })
        end
    else
        self._group[#self._group + 1] = { size = #args, typ = typ }
        for _, key in ipairs(args) do
            tbl_insert(self._reqs, { cmd = cmd, key = key })
        end
    end

    if not pipeline then
        local res, err = _M.commit_pipeline(self)
        if not res then
            return res, err
        end

        return res[1]
    end
end


for mcmd, item in pairs(mcommands) do
    local cmd = item.cmd
    local typ = item.typ

    _M[mcmd] =
        function (self, ...)
            return _do_mcmd(self, cmd, typ, ...)
        end
end


return _M
