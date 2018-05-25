-module(ws_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([simple/1]).
-export([subprotocol/1]).
-export([close/1]).

%% Callbacks

all() ->
    [
        simple,
        subprotocol,
        close
    ].

init_per_suite(Config) ->
    application:ensure_all_started(erlang_v8_lib),
    application:ensure_all_started(hemlock),
    Config.

end_per_suite(Config) ->
    application:stop(hemlock),
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
    {ok, #{ <<"data">> := <<"data">> }} = erlang_v8_lib:run(<<"
    ws.open('ws://127.0.0.1:5000/ws')
        .then((conn) => {
            conn.send('data');
            return conn.receive();
        })
        .then((data) => process.return(JSON.parse(data)))
        .catch((error) => process.return(error));
    ">>),

    ok.

subprotocol(_Config) ->
    {ok, #{ <<"data">> := <<"data">> }} = erlang_v8_lib:run(<<"
    ws.open('ws://127.0.0.1:5000/ws', { subprotocols: ['lol', 1] })
        .then((conn) => {
            conn.send('data');
            return conn.receive();
        })
        .then((data) => process.return(JSON.parse(data)))
        .catch((error) => process.return(error));
    ">>),

    ok.


close(_Config) ->
    {ok, <<"No connection.">>} = erlang_v8_lib:run(<<"
    ws.open('ws://127.0.0.1:5000/ws')
        .then((conn) => {
            conn.close();
            return conn.send('test');
        })
        .catch((error) => process.return(error));
    ">>),

    ok.
