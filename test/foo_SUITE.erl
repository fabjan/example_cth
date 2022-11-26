-module(foo_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    foo_works/1
]).

suite() ->
    [{ct_hooks, [example_cth]}].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_Test, Config) ->
    Config.

end_per_testcase(_Test, Config) ->
    Config.

all() ->
    [foo_works].

foo_works(_Config) ->
    ok.

foo_fails(_Config) ->
    ?assertEqual(5, 2 + 2).
