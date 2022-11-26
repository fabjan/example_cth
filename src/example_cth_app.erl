%%%-------------------------------------------------------------------
%% @doc example_cth public API
%% @end
%%%-------------------------------------------------------------------

-module(example_cth_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    example_cth_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
