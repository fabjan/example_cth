 %%% This is a minimal example of a Common Test Hook module.
 %%%
 %%% To use it, on the command line:
 %%%     ct_run -suite example_SUITE -pa . -ct_hooks example_cth
 %%%
 %%% N.B. `-pa .`, the hook beam file must be in the code path when installing,
 %%% see https://www.erlang.org/doc/apps/common_test/ct_hooks_chapter.html#installing-a-cth for more information.
 -module(example_cth2).

 %% Mandatory Callbacks
 -export([init/2]).

 %% Optional Callbacks
 -export([id/1]).

 -export([pre_init_per_suite/3]).
 -export([post_end_per_suite/4]).

 -export([pre_init_per_testcase/4]).
 -export([post_end_per_testcase/5]).

 -export([on_tc_skip/4]).

 -export([terminate/1]).

 %% This state is threaded through all the callbacks.
 -record(state, { filename, total, suite_total, ts, tcs, data, skipped }).

 %% Return a unique id for this CTH.
 %% Using the filename means the hook can be used with different
 %% log files to separate timing data within the same test run.
 %% See Installing a CTH for more information.
 id(Opts) ->
     %% the path is relative to the test run directory
     proplists:get_value(filename, Opts, "example_cth2.log").

 %% Always called before any other callback function. Use this to initiate
 %% any common state.
 init(Id, _Opts) ->
     {ok, #state{ filename = Id, total = 0, data = [], skipped = 0 }}.

 %% Called before init_per_suite is called.
 pre_init_per_suite(_Suite,Config,State) ->
     {Config, State#state{ suite_total = 0, tcs = [] }}.

 %% Called after end_per_suite.
 post_end_per_suite(Suite,_Config,Return,State) ->
     Data = {suites, Suite, State#state.suite_total, lists:reverse(State#state.tcs)},
     {Return, State#state{ data = [Data | State#state.data] ,
                           total = State#state.total + State#state.suite_total } }.

 %% Called before each init_per_testcase.
 pre_init_per_testcase(_Suite,_TC,Config,State) ->
     Now = erlang:monotonic_time(microsecond),
     {Config, State#state{ ts = Now, suite_total = State#state.suite_total + 1 } }.

 %% Called after each end_per_testcase.
 post_end_per_testcase(Suite,TC,_Config,Return,State) ->
     Now = erlang:monotonic_time(microsecond),
     TCInfo = {testcase, Suite, TC, Return, Now - State#state.ts},
     {Return, State#state{ ts = undefined, tcs = [TCInfo | State#state.tcs] } }.

 %% Called when a test case is skipped by either user action
 %% or due to an init function failing.  
 on_tc_skip(_Suite, _TC, _Reason, State) ->
     State#state{ skipped = State#state.skipped + 1 }.

 %% Called when the scope of the CTH is done.
 terminate(State) ->
     %% use append to avoid data loss if the path is reused
    io:format(Dev, "~p.~n",
              [{test_run, State#state.total, State#state.data}]),
    file:close(Dev),
     ok.
