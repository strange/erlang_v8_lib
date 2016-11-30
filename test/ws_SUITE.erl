-module(ws_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([simple/1]).

%% Callbacks

all() ->
    [
        simple
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

simple(_Config) ->
    {ok, #{ <<"reqData">> := <<"data">> }} = erlang_v8_lib:run(<<"
    ws.open('ws://sockb.in')
        .then((conn) => {
            conn.send('data');
            return conn.receive();
        })
        .then((data) => process.return(JSON.parse(data)))
        .catch((error) => process.return(error));
    ">>),

    ok.
