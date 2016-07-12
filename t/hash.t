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

=== TEST 1: hset, hget, hmget, hdel, hgetall
--- http_config eval: $::HttpConfig
--- config
    location /t {
        content_by_lua_block {
            local lib = require "t.lib"
            local json_encode   = require("cjson").encode

            local redcli = lib.ncli()
            local redcli = lib.ncluster()
            redcli:hset("hkey", "field1", 123)
            redcli:hset("hkey", "field2", "abc")
            local res, err = redcli:hget("hkey", "field1")
            ngx.say(res)
            local res, err = redcli:hmget("hkey", "field1", "field2")
            ngx.say(json_encode(res))
            local res, err = redcli:hdel("hkey", "field2")
            ngx.say(json_encode(res))
            local res, err = redcli:hmget("hkey", "field1", "field2")
            ngx.say(json_encode(res))
            local res, err = redcli:hgetall("hkey")
            ngx.say(json_encode(res))
        }
    }
--- request
GET /t
--- response_body
123
["123","abc"]
1
["123",null]
["field1","123"]
--- no_error_log
[error]

