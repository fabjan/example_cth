%%% Common Test Example Common Test Hook module.
-module(example_cth2).

%% Mandatory Callbacks
-export([init/2]).

%% Optional Callbacks
-export([id/1]).

-export([pre_init_per_suite/3]).
-export([post_end_per_suite/4]).

-export([pre_init_per_testcase/4]).
-export([post_end_per_testcase/5]).

-export([terminate/1]).

-record(state, { filename, total, suite_total, ts, tcs, data }).

%% Return a unique id for this CTH.
%% Using the filename means the hook can be used with different
%% log files to separate timing data within the same test run.
%% See Installing a CTH for more information.
id(Opts) ->
    proplists:get_value(filename, Opts, "example_cth2.log").

%% Always called before any other callback function. Use this to initiate
%% any common state.
init(Id, _) ->
    {ok, #state{ filename = Id, total = 0, data = [] }}.

%% Called before init_per_suite is called.
pre_init_per_suite(_,Config,State) ->
    {Config, State#state{ suite_total = 0, tcs = [] }}.

%% Called after end_per_suite.
post_end_per_suite(Suite,_,Return,State) ->
    Data = {suites, Suite, State#state.suite_total, lists:reverse(State#state.tcs)},
    {Return, State#state{ data = [Data | State#state.data] ,
                          total = State#state.total + State#state.suite_total } }.

%% Called before each init_per_testcase.
pre_init_per_testcase(_,_,Config,State) ->
    Now = erlang:monotonic_time(microsecond),
    {Config, State#state{ ts = Now, total = State#state.suite_total + 1 } }.

%% Called after each end_per_testcase.
post_end_per_testcase(Suite,TC,_,Return,State) ->
    Now = erlang:monotonic_time(microsecond),
    TCInfo = {testcase, Suite, TC, Return, Now - State#state.ts},
    {Return, State#state{ ts = undefined, tcs = [TCInfo | State#state.tcs] } }.

%% Called when the scope of the CTH is done
terminate(State) ->
    {ok, Dev} = file:open(State#state.filename, [write, append]),
    io:format(Dev, "~p.~n",
              [{test_run, State#state.total, State#state.data}]),
    file:close(Dev),
    ok.
