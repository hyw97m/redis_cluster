# vim:set ft=perl ts=4 sw=4 et fdm=marker:

use Test::Nginx::Socket::Lua;
use Cwd qw(cwd);

worker_connections(1024);
repeat_each(1);

plan tests => repeat_each() * (blocks() * 3 + 0);

my $pwd = cwd();

$ENV{TEST_NGINX_ROOT_PATH} = $pwd;

our $HttpConfig = qq{
    lua_package_path "$pwd/?.lua;../$pwd/?.lua;;";
};

$ENV{TEST_NGINX_RESOLVER} = '8.8.8.8';

#log_level("warn");
no_long_string();
no_shuffle();

run_tests();


__DATA__

=== TEST 1: mset, mget, del
--- http_config eval: $::HttpConfig
--- config
    location /t {
        content_by_lua_block {
            local lib = require "t.lib"

            local redcli = lib.ncli()
            redcli:init_pipeline()
            redcli:set("abc", 123)
            redcli:get("abc")
            redcli:set("d{e}f", 456)
            redcli:get("d{e}f")
            redcli:del("abc","d{e}f")
            redcli:mget("abc", "d{e}f")
            local res1_1, err = redcli:commit_pipeline()

            local redcli = lib.ncluster()
            redcli:init_pipeline()
            redcli:set("abc", 123)
            redcli:get("abc")
            redcli:set("d{e}f", 456)
            redcli:get("d{e}f")
            redcli:del("abc","d{e}f")
            redcli:mget("abc", "d{e}f")
            local res2_1, err = redcli:commit_pipeline()

            ngx.say(lib.chktab(res1_1, res2_1, true))
        }
    }
--- request
GET /t
--- response_body
["OK","123","OK","456",2,[null,null]] ["OK","123","OK","456",2,[null,null]] true
--- no_error_log
[error]

