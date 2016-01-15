-module(generic_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([console_log/1]).
-export([return/1]).
-export([reset_vm/1]).
-export([context/1]).

%% Callbacks

all() ->
    [
        console_log
        %% reset_vm,
        %% context,
        %% return
    ].

init_per_suite(Config) ->
    application:ensure_all_started(erlang_v8_lib),
    Config.

end_per_suite(_Config) ->
    ok.

%% Tests

console_log(_Config) ->
    ok = erlang_v8_lib:run(<<"console.log('test');">>),
    ok.

context(_Config) ->
    {ok, <<"abc">>} = erlang_v8_lib:run_with_context(
                        <<"process.return(Event.getType());">>,
                        [{type, <<"abc">>}]),
    ok.

return(_Config) ->
    {ok, 1} = erlang_v8_lib:run(<<"process.return(1);">>),
    ok.

reset_vm(_Config) ->
    {ok, 1} = erlang_v8_lib:run(<<"__internal.lol = 1;
                                  process.return(__internal.lol);">>),
    {ok, null} = erlang_v8_lib:run(<<"process.return(__internal.lol);">>),
    ok.
