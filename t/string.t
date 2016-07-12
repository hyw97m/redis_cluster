# vim:set ft=perl ts=4 sw=4 et fdm=marker:

use Test::Nginx::Socket::Lua;
use Cwd qw(cwd);

worker_connections(1024);
repeat_each(1);

plan tests => repeat_each() * (blocks() * 3 + 0);

my $pwd = cwd();

$ENV{TEST_NGINX_ROOT_PATH} = $pwd;

our $HttpConfig = qq{
    lua_package_path "$pwd/lib/?.lua;;";
};

$ENV{TEST_NGINX_RESOLVER} = '8.8.8.8';

#log_level("warn");
no_long_string();
no_shuffle();

run_tests();


__DATA__

=== TEST 1: set, get, del
--- http_config eval: $::HttpConfig
--- config
    location /t {
        content_by_lua_block {
            local lib = require "t.lib"
            local redcli = lib.ncluster()
            local key = "abc"
            local res, err = redcli:set(key, 123)
            ngx.say(res)
            local res, err = redcli:get(key)
            ngx.say(res)
            local res, err = redcli:del(key)
            ngx.say(res)
            local res, err = redcli:get(key)
            ngx.say(res)

            local key = "a{bcd}ef"
            local res, err = redcli:set(key, 456)
            ngx.say(res)
            local res, err = redcli:get(key)
            ngx.say(res)
            local res, err = redcli:del(key)
            ngx.say(res)
            local res, err = redcli:get(key)
            ngx.say(res)

        }
    }
--- request
GET /t
--- response_body
OK
123
1
null
OK
456
1
null
--- no_error_log
[error]

=== TEST 2: , setext, ttl, expire
--- http_config eval: $::HttpConfig
--- config
    location /t {
        content_by_lua_block {
            local lib = require "t.lib"
            local redcli = lib.ncluster()
            local key = "test"
            local res, err = redcli:setex(key, 1, "abc")
            ngx.say(res)
            local res, err = redcli:get(key)
            ngx.say(res)
            local res, err = redcli:ttl(key)
            ngx.say(res)
            local res, err = redcli:expire(key, 2)
            ngx.say(res)
            local res, err = redcli:ttl(key)
            ngx.say(res)
            ngx.sleep(2)
            local res, err = redcli:get(key)
            ngx.say(res)

        }
    }
--- request
GET /t
--- response_body
OK
abc
1
1
2
null
--- no_error_log
[error]

=== TEST 3: incr, incrby, decr, decrby
--- http_config eval: $::HttpConfig
--- config
    location /t {
        content_by_lua_block {
            local lib = require "t.lib"
            local redcli = lib.ncluster()
            local key = "num"
            redcli:del(key)
            local res, err = redcli:incr(key)
            ngx.say(res)
            local res, err = redcli:incrby(key, 10)
            ngx.say(res)
            local res, err = redcli:decr(key)
            ngx.say(res)
            local res, err = redcli:decrby(key, 5)
            ngx.say(res)
            local res, err = redcli:get(key)
            ngx.say(res)

        }
    }
--- request
GET /t
--- response_body
1
11
10
5
5
--- no_error_log
[error]


=== TEST 4: mset, mget
--- http_config eval: $::HttpConfig
--- config
    location /t {
        content_by_lua_block {
            local lib = require "t.lib"
            local key1 = "abc1"
            local key2 = "abc2"
            local key3 = "abc3"
            local key4 = "abc4"

            local val1 = "123"
            local val2 = "456"
            local val3 = "789"


            local redcli = lib.ncli()
            redcli:del(key1, key2, key3)
            local res1_1, err = redcli:mset(key1, val1, key2, val2, key3, val3)
            local res1_2, err = redcli:mget(key1)
            local res1_3, err = redcli:mget(key1, key2, key3)
            local res1_4, err = redcli:del(key1, key2, key3, key4)
            local res1_5, err = redcli:mget(key1, key2, key3)

            local redcli = lib.ncluster()
            redcli:del(key1, key2, key3, key4)
            local res2_1, err = redcli:mset(key1, val1, key2, val2, key3, val3)
            local res2_2, err = redcli:mget(key1)
            local res2_3, err = redcli:mget(key1, key2, key3)
            local res2_4, err = redcli:del(key1, key2, key3, key4)
            local res2_5, err = redcli:mget(key1, key2, key3)

            ngx.say(lib.chktab(res1_1, res2_1, true))
            ngx.say(lib.chktab(res1_2, res2_2, true))
            ngx.say(lib.chktab(res1_3, res2_3, true))
            ngx.say(lib.chktab(res1_4, res2_4, true))
            ngx.say(lib.chktab(res1_5, res2_5, true))

        }
    }
--- request
GET /t
--- response_body
"OK" ["OK","OK","OK"] false
["123"] ["123"] true
["123","456","789"] ["123","456","789"] true
3 3 true
[null,null,null] [null,null,null] true
--- no_error_log
[error]


