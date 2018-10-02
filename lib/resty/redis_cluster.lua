

local bit   = require "bit";
local json_encode   = require("cjson").encode

local bxor      = bit.bxor
local band      = bit.band
local lshift    = bit.lshift
local rshift    = bit.rshift

local len       = string.len
local sub       = string.sub
local gsub      = string.gsub
local format    = string.format

local concat    = table.concat
local insert    = table.insert

local seed      = math.randomseed
local random    = math.random

local timer_at  = ngx.timer.at
local exiting   = ngx.worker.exiting

local ngx_log   = ngx.log
local ERR       = ngx.ERR
local WARN      = ngx.WARN

local now       = ngx.now


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
    local keylen = len(key)
    for i = 1, keylen do
        if not s and key:byte(i) == 123 then
            s = i
        elseif key:byte(i) == 125 then
            e = i
            break
        end
    end

    if s and e and e > s + 1 then
        return band(crc16(sub(key, s + 1, e - 1)), 0x3fff) + 1
    end

    return band(crc16(key), 0x3fff) + 1
end


local ok, new_tab = pcall(require, "table.new")
if not ok or type(new_tab) ~= "function" then
    new_tab = function (narr, nrec) return {} end
end


local redis = require "resty.redis"
redis.add_commands("cluster")


local commands = {
    [1] = {
        "append",               --[["auth",]]           --[["bgrewriteaof",]]
        --[["bgsave",]]         "bitcount",             --[["bitop",]]
        "blpop",                "brpop",
        --[["brpoplpush",]]     --[["client",]]         --[["config",]]
        --[["dbsize",]]
        --[["debug",]]          "decr",                 "decrby",
        --[["del",]]            --[["discard",]]        --[["dump",]]
        --[["echo",]]
        --[["exec",]]           "exists",
        "expire",               "expireat",             --[["flushall",]]
        --[["flushdb",]]        "get",                  "getbit",
        "getrange",             "getset",               "hdel",
        "hexists",              "hget",                 "hgetall",
        "hincrby",              "hincrbyfloat",         "hkeys",
        "hlen",
        "hmget",                "hmset",                "hscan",
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
        "sismember",            --[["slaveof",]]        --[["slowlog",]]
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
        "zscore",               --[["zunionstore",]]
    },
    [2] = { },
    [3] = {
        "eval",     "evalsha"
    },
    [4] = {
        "script",
    },
}


local mcommands = {
    mset = { cmd = "set" },
    mget = { cmd = "get" },
    del  = { cmd = "del" },
    msetnx  = { cmd = "setnx" },
}


local cache_nodes = {}


local function _connect(host, port, password, timeout)
    local red_cli = redis:new()

    -- ngx_log(ERR, concat({host, port, tostring(password), tostring(timeout)}, ", "))
    red_cli:set_timeout(timeout or 3000)

    local st = now()
    local ok, err = red_cli:connect(host, port)
    if not ok then
        ngx_log(ERR, format("failed to connect, err: %s [%s:%s], t: %s", tostring(err), host, port, now() - st))
        return nil, err
    end

    if password then
        ok, err = red_cli:auth(password)
        if not ok then
            ngx_log(ERR, format("failed to auth, err: %s [%s:%s]", tostring(err), host, port))
        end
    end

    return red_cli, err
end


local function _random(min, max)
    seed(gsub(now(),"[.]",""):reverse():sub(1, 7))
    return random(min, max)
end


local function _fetch_servers(self)
    local i = 0
    local srvsize = #self.config.servers
    local offset = _random(1, srvsize * 3)

    return function ()
        i = i + 1
        if i > srvsize then
            return
        end

        local idx = (i + offset) % srvsize + 1
        return self.config.servers[idx]
    end
end


local function _fetch_slots(self, async)
    local cluster_name = self.config.name
    if not cache_nodes[cluster_name] then
        cache_nodes[cluster_name] = {}
    end

    local st = now()
    if cache_nodes[cluster_name].lock and cache_nodes[cluster_name].lock > st then
        if async then
            return
        end

        if cache_nodes[cluster_name].down then
            return nil, "CLUSTERDOWN the cluster is down"
        end

        while true do
            ngx.sleep(0.1)
            if cache_nodes[cluster_name].lock then
                if now() - st > 1 then
                    return false, "the node is being refreshed."
                end
            else
                return true
            end
        end
    end

    cache_nodes[cluster_name].lock = ngx.time() + 5

    local nodes, nidx = {}, 0
    local slots = {}
    for server in _fetch_servers(self) do
        local host, port = server[1], server[2]
        local red_cli, err = _connect(host, port, self.config.password, self.config.timeout)
        if not red_cli then
            ngx_log(ERR, format("failed to connect, err: %s [%s:%s]", tostring(err), host, port))

        else
            local info, err = red_cli:cluster("slots")
            red_cli:set_keepalive(self.config.idle_timeout, self.config.pool_size)

            if info and type(info) == "table" and #info > 0 then
                for i = 1, #info do
                    nidx = nidx + 1
                    local item = info[i]
                    local node = {
                        host = item[3][1],
                        port = item[3][2],
                        s_slot = item[1],
                        e_slot = item[2],
                    }

                    for slot = node.s_slot, node.e_slot do
                        slots[slot + 1] = nidx
                    end

                    nodes[nidx] = node
                end

                cache_nodes[cluster_name] = {
                    nodes   = nodes,
                    node_num= #nodes,
                    slots   = slots,
                }

                cache_nodes[cluster_name].lock = nil
                cache_nodes[cluster_name].down = nil

                -- ngx_log(ERR, format("fetch success slots, time: ", now() - st))

                return true
            else
                ngx_log(ERR, format("failed to fetch slots, err: %s [%s:%s]", tostring(err), host, port))

            end

        end
    end

    cache_nodes[cluster_name].lock = nil
    cache_nodes[cluster_name].down = true

    return nil, "CLUSTERDOWN the cluster is down"
end


local function _async_fetch_slots(self)
    local handler = function(premature)
        if premature or exiting() then
            return
        end

        _fetch_slots(self, true)
    end

    local ok, err = timer_at(0, handler)
    if not ok then
        ngx_log(ERR, "failed to create timer: ", err)
        return
    end
end


local _M = {}


function _M.new(self, conf)
    local config = {
        name    = conf.name or "dev",
        servers = conf.servers,
        password= conf.password,
        timeout = conf.timeout or 3000,
        idle_timeout= conf.idle_timeout or 1000,
        pool_size   = conf.pool_size or 100,
    }

    local mt = setmetatable({ config = config }, { __index = _M })

    local ns = cache_nodes[config.name]
    if not ns or not ns.nodes or ns.down then
        local ok, err = _fetch_slots(mt)
        if not ok then
            return nil, err
        end
    end

    return mt
end


function _M.keyhashslot(key)
    return keyhashslot(key) - 1
end


function _M.fetch_nodes(self)
    return cache_nodes[self.config.name]["nodes"]
end


local function _do_retry(self, host, port, cmd, key, ...)
    local red_cli, err = _connect(host, port, self.config.password, self.config.timeout)
    if not red_cli then
        return nil, err
    end

    local res, err
    if ... then
        res, err = red_cli[cmd](red_cli, key, ...)
    else
        res, err = red_cli[cmd](red_cli, key)
    end

    red_cli:set_keepalive(self.config.idle_timeout, self.config.pool_size)

    return res, err
end

-- return res, err, errcode[1.2] or retry[2]
local function _do_cmd(self, keypos, cmd, ...)
    local args = { ... }

    if self._reqs then
        if not self[cmd] then
            return nil, "nofound cmd"
        end

        self._group_n = self._group_n + 1
        self._group[self._group_n] = { size = 1 }

        self._reqs_n = self._reqs_n + 1
        self._reqs[self._reqs_n] = { cmd = cmd, keypos = keypos, args = args }

        return true
    end

    local slots = cache_nodes[self.config.name]["slots"]
    local nidxs = {}
    if keypos == 3 then
        local keynum = tonumber(args[2])
        if keynum and keynum > 0 then
            local slot = keyhashslot(args[3])
            nidxs[1] = slots[slot]
        else
            local node_num = cache_nodes[self.config.name].node_num
            nidxs[1] = _random(1, node_num * 3) % node_num + 1
        end
    elseif keypos == 4 then
        for i = 1, cache_nodes[self.config.name].node_num do
            nidxs[i] = i
        end
    else
        local slot = keyhashslot(args[1])
        nidxs[1] = slots[slot]
    end

    local res, errcode = {}, nil
    for i, nidx in ipairs(nidxs) do
        local node = cache_nodes[self.config.name]["nodes"][nidx]

        local red_cli, err = _connect(node.host, node.port, self.config.password, self.config.timeout)
        if not red_cli then
            return nil, err, 1
        end

        local rs, err = red_cli[cmd](red_cli, ...)
        if not rs then
            if err and sub(err, 1, 5) == "MOVED" then
                errcode = 2
            end
        end

        res[i] = {result = rs, err = err}

        red_cli:set_keepalive(self.config.idle_timeout, self.config.pool_size)
    end

    if keypos == 4 then
        local ret
        for i, nidx in ipairs(nidxs) do
            if res[i].result then
                ret = res[i].result
            end
        end

        return ret, res, errcode
    end

    return res[1].result, res[1].err, errcode
end


for keypos, cmds in pairs(commands) do
    for i = 1, #cmds do
        local cmd = cmds[i]

        _M[cmd] =
        function (self, ...)
            local res, err, errcode =_do_cmd(self, keypos, cmd, ...)
            if not res then
                if errcode and errcode > 0 then
                    _async_fetch_slots(self)
                end
            end

            return res, err
        end
    end
end


function _M.init_pipeline(self)
    self._reqs,  self._reqs_n  = {}, 0
    self._group, self._group_n = {}, 0
end


local function _merge(group, res)
    local ret, n = {}, 0
    local pp = 0
    for _, item in ipairs(group) do
        local data, m
        if item.mcmd == "del" then
            data = 0
            for cp = pp + 1, pp + item.size do
                data = data + ((res[cp] == false) and 0 or res[cp])
            end

        elseif item.mcmd == "mset" or item.mcmd == "msetnx" or item.mcmd == "mget" then
            data, m = {}, 0
            for cp = pp + 1, pp + item.size do
                m = m + 1
                data[m] = res[cp]
            end

        else
            data = res[pp + 1]
        end

        pp = pp + item.size
        n = n + 1
        ret[n] = data
    end

    return ret
end


-- return res, err, errcode[1.2] or retry[2]
local function _commit(self, nidx, reqs)
    local node = cache_nodes[self.config.name]["nodes"][nidx]

    local errcode

    local red_cli, err = _connect(node.host, node.port, self.config.password, self.config.timeout)
    if not red_cli then
        return nil, err, 1
    end

    red_cli:init_pipeline()
    for _, req in pairs(reqs) do
        red_cli[req.cmd](red_cli, unpack(req.args))
    end

    local result, err = red_cli:commit_pipeline()
    red_cli:set_keepalive(self.config.idle_timeout, self.config.pool_size)
    if not result then
        return nil, err
    end

    local ret = {}

    for i, rs in ipairs(result) do
        if type(rs) == "table" and rs[1] == false and sub(rs[2], 1, 5) == "MOVED" then
            local m, err = ngx.re.match(rs[2], "MOVED [0-9]+ ([0-9.]+):([0-9]+)")
            if type(m) == "table" then
                local res, err = _do_retry(self, m[1], m[2], reqs[i].cmd, unpack(reqs[i].args))

                ret[i] = res or false
            else
                ret[i] = false
            end

            errcode = 1
        else
            ret[i] = rs
        end
    end

    return ret, nil, errcode
end


function _M.commit_pipeline(self)
    local reqs = self._reqs
    local group = self._group
    if not reqs then
        return nil, "no pipeline"
    end

    self._reqs,  self._reqs_n  = nil, nil
    self._group, self._group_n = nil, nil

    local slots = cache_nodes[self.config.name]["slots"]

    local map = {}
    for idx, req in ipairs(reqs) do
        local nidx
        if req.keypos == 3 then
            local keynum = tonumber(req.args[2])
            if keynum and keynum > 0 then
                local slot = keyhashslot(req.args[3])
                nidx = slots[slot]
            else
                local node_num = cache_nodes[self.config.name].node_num
                nidx = _random(1, node_num * 3) % node_num + 1
            end
        else
            local slot = keyhashslot(req.args[1])
            nidx = slots[slot]
        end

        if not map[nidx] then
            map[nidx] = { nidx = nidx, reqs = {}, idxs = {} }
        end

        local mreqs, midxs = map[nidx].reqs, map[nidx].idxs
        mreqs[#mreqs + 1], midxs[#midxs + 1] = req, idx
    end

    local ret = new_tab(0, #reqs)

    local refetch
    for nidx, item in pairs(map) do
        local res, err, errcode = _commit(self, nidx, item.reqs)

        if errcode then
            refetch = true
        end

        if not res then
            _async_fetch_slots(self)
            return nil, err
        end

        for i, idx in ipairs(item.idxs) do
            ret[idx] = res[i]
        end
    end

    if #group > 0 then
        ret = _merge(group, ret)
    end

    return ret
end


function _M.cancel_pipeline(self)
    self._reqs,  self._reqs_n  = nil, nil
    self._group, self._group_n = nil, nil
end


local function _do_mcmd(self, keypos, mcmd, cmd, ...)
    local pipeline = self._reqs and true or nil
    if not pipeline then
        _M.init_pipeline(self)
    end

    local args = { ... }

    local decompose_size = 1
    if mcmd == "mset" or mcmd == "msetnx" then
        decompose_size = 2
    end

    self._group_n = self._group_n + 1
    self._group[self._group_n] = {
        size = #args / decompose_size,
        -- decompose_size = decompose_size,
        mcmd = mcmd,
        -- cmd  = cmd,
    }

    for i = 1, #args, decompose_size do
        self._reqs_n = self._reqs_n + 1
        self._reqs[self._reqs_n] = {
            cmd  = cmd,
            keypos  = 1,
            args = decompose_size == 1 and { args[i] } or { args[i], args[i + 1] }
        }
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
    local keypos = 1

    _M[mcmd] =
        function (self, ...)
            return _do_mcmd(self, keypos, mcmd, cmd, ...)
        end
end


return _M

