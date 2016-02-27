-module(generic_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([console_log/1]).
-export([instructions/1]).
-export([return/1]).
-export([manual_release/1]).
-export([automatic_release/1]).
-export([context/1]).

%% Callbacks

all() ->
    [
        %% console_log,
        %% instructions,
        manual_release,
        automatic_release
        %% context,
        %% return
    ].

init_per_suite(Config) ->
    application:ensure_all_started(erlang_v8_lib),
    Config.

init_per_testcase(_Case, Config) ->
    {ok, Pid} = erlang_v8_lib_sup:start_link(),
    [{pid, Pid}|Config].

end_per_testcase(_Case, Config) ->
    Pid = proplists:get_value(pid, Config),
    exit(Pid, normal),
    ok.

%% Tests

console_log(_Config) ->
    ok = erlang_v8_lib:run(<<"console.log('test');">>),
    ok.

instructions(_Config) ->
    erlang_v8_lib_sup:start_link(),
    {ok, 1} = erlang_v8_lib:run([
        {eval, <<"function lol() { process.return(1); }">>},
        {call, <<"lol">>, []}
    ]),
    ok.

context(_Config) ->
    erlang_v8_lib_sup:start_link(),
    {ok, <<"abc">>} = erlang_v8_lib:run([
        {context, #{ type => <<"abc">> }},
        {eval, <<"process.return(Context.get().type);">>}
    ]),
    ok.

return(_Config) ->
    erlang_v8_lib_sup:start_link(),
    {ok, 1} = erlang_v8_lib:run(<<"process.return(1);">>),
    ok.

automatic_release(_Config) ->
    Parent = self(),

    Pid = spawn(fun() ->
        {ok, Worker} = erlang_v8_lib_pool:claim(),
        Parent ! Worker,
        receive never -> ok end
    end),

    Worker = receive Response -> Response end,

    exit(Pid, kill),
    timer:sleep(1),

    {error, invalid_worker} = erlang_v8_lib_pool:release(Worker),

    ok.

manual_release(_Config) ->
    Parent = self(),

    _Pid = spawn(fun() ->
        {ok, Worker} = erlang_v8_lib_pool:claim(),
        Parent ! Worker,
        receive never -> ok end
    end),

    Worker = receive Response -> Response end,
    timer:sleep(1),
    ok = erlang_v8_lib_pool:release(Worker),

    ok.

