-module(bar_SUITE).

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
    bar_works/1,
    bar_fails/1
]).

suite() ->
    [{ct_hooks, [{example_cth, [{filename, "example_cth.log"}]}, example_cth2]}].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_Test, Config) ->
    Config.

end_per_testcase(_Test, Config) ->
    Config.

all() ->
    [bar_works, bar_fails].

bar_works(_Config) ->
    ok.

bar_fails(_Config) ->
    {skip, "no thanks"}.
