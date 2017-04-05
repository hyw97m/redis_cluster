#   Name

redis cluster

#   部署

具体不做详细说明


#   实例


```
server {
    location /t {
        content_by_lua_block {
            local rediscluster = require "resty.redis_cluster"
            local config = {
                name = "name",
                server = {
                    { "127.0.0.1", 7001 },
                    { "127.0.0.1", 7002 },
                    { "127.0.0.1", 7003 },
                    { "127.0.0.1", 7004 },
                    { "127.0.0.1", 7005 },
                    { "127.0.0.1", 7006 },
                },
                password        = "abc",
                idle_timeout    = 1000,
                pool_size       = 200,
            }

            local redcli, err = rediscluster:new(config)
            if not ok then
                ngx.say("failed to cluster: ", err)
                return
            end

            local key = "abc"
            local ok, err = redcli:set(key, 123)
            if not ok then
                ngx.say("failed to set key: ", err)
                return
            end

            local res, err = redcli:get(key)
            if not res then
                ngx.say("failed to get key: ", err)
                return
            end
            ngx.say(res)

            local ok, err = redcli:del(key)
            if not ok then
                ngx.say("failed to del key: ", err)
                return
            end

            local res, err = redcli:get(key)
            if not res then
                ngx.say("failed to get key: ", err)
                return
            end
            ngx.say(res)
        }
    }
}

```


##  config

*   `name`

    集群名, 用于区别多个集群的使用

*   `server`

    服务列表, 如: {{"127.0.0.1", 7001}, {"127.0.0.1", 7002}}

*   `password`

    auth密码, 设置该值会连接redis时做一次auth验证


