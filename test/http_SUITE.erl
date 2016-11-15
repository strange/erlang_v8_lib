-module(http_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([simple/1]).

-export([get/1]).
-export([post/1]).
-export([put/1]).
-export([delete/1]).
-export([head/1]).

-export([https/1]).
-export([arguments/1]).
-export([headers/1]).

%% Callbacks

all() ->
    [
        simple,
        arguments,
        get,
        headers,
        post,
        put,
        delete,
        head
        %% https
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
    {ok, #{ <<"args">> := #{ <<"x">> := <<"1">> } }} = erlang_v8_lib:run(<<"
    http.get('http://127.0.01:5000/get?x=1')
        .then((resp) => resp.json())
        .then((json) => process.return(json))
        .catch((error) => process.return(error));
    ">>),

    {ok, <<"invalid_url">>} = erlang_v8_lib:run(<<"
    http.get('abcdefedcba')
        .then((resp) => resp.json())
        .then((json) => process.return(json))
        .catch((error) => process.return(error));
    ">>),
    ok.

get(_Config) ->
    {ok, #{ <<"args">> := #{ <<"test">> := <<"fest">> } }} = erlang_v8_lib:run(<<"
    http.get('http://127.0.01:5000/get?test=fest').then(function(resp) {
        return resp.json();
    }).then(function(json) {
        process.return(json);
    }).catch(function(err) {
        process.return(err);
    });
    ">>),

    {ok, Data1} = erlang_v8_lib:run(<<"
    http.get('http://127.0.01:5000/get?test=fest').then((resp) => {
        return resp.json();
    }).then(function(json) {
        process.return(json);
    });
    ">>),
    #{ <<"args">> := #{ <<"test">> := <<"fest">> } } = Data1,

    %% {ok, Data2} = erlang_v8_lib:run(<<"
    %% http.get('http://127.0.01:5000/status/404').then(function(resp) {
    %%     process.return(resp);
    %% });
    %% ">>),
    %% #{ <<"code">> := 404 } = Data2,

    %% {ok, Data3} = erlang_v8_lib:run(<<"
    %% http.get('http://127.0.01:5000/status/200').then(function(data) {
    %%     process.return(data);
    %% });
    %% ">>),
    %% true = is_number(maps:get(<<"time">>, Data3)),

    %% {ok, Data4} = erlang_v8_lib:run(<<"
    %% http.get('http://127.0.01:5000/get', {
    %%     headers: { 'Content-Type': 'application/json' },
    %%     body: { test: 'fest' }
    %% }).then(function(resp) {
    %%     return resp.json();
    %% }).then(function(json) {
    %%     process.return(json);
    %% });
    %% ">>),
    %% #{ <<"args">> := #{ <<"test">> := <<"fest">> } } =
    %%     jsx:decode(Data4, [return_maps]),
    %%
    %% {ok, Data5} = erlang_v8_lib:run(<<"
    %% http.get('http://127.0.01:5000/get?test=fest', {
    %%     headers: { 'Connection': 'close' }
    %% }).then(function(resp) {
    %%     return resp.json();
    %% }).then(function(json) {
    %%     process.return(json);
    %% });
    %% ">>),
    %% #{ <<"args">> := #{ <<"test">> := <<"fest">> } } =
    %%     jsx:decode(Data5, [return_maps]),
    ok.

post(_Config) ->
    {ok, Data0} = erlang_v8_lib:run(<<"
    http.post('http://127.0.01:5000/post', { body: 'hello' }).then((resp) => {
        return resp.json();
    }).then(function(json) {
        process.return(json);
    });
    ">>),
    #{ <<"data">> := <<"hello">> } = Data0,

    ok.

put(_Config) ->
    {ok, Data0} = erlang_v8_lib:run(<<"
    http.put('http://127.0.01:5000/put', { body: 'hello' }).then((resp) => {
        return resp.json();
    }).then(function(json) {
        process.return(json);
    });
    ">>),
    #{ <<"data">> := <<"hello">> } = Data0,
    ok.

delete(_Config) ->
    {ok, Data0} = erlang_v8_lib:run(<<"
    http.delete('http://127.0.01:5000/delete', { body: 'hello' }).then((resp) => {
        return resp.json();
    }).then(function(json) {
        process.return(json);
    });
    ">>),
    #{ <<"data">> := <<"hello">> } = Data0,
    ok.

head(_Config) ->
    {ok, Data0} = erlang_v8_lib:run(<<"
    http.head('http://127.0.0.1:5000').then(function(resp) {
        process.return(resp);
    });
    ">>),
    #{<<"code">> := 200 } = Data0,

    ok.

headers(_Config) ->
    {ok, #{ <<"headers">> := #{ <<"Header">> := <<"ok">> }}} = erlang_v8_lib:run(<<"
    http.get('http://127.0.01:5000/headers', { headers: { 'header': 'ok' } })
        .then((resp) => resp.json())
        .then((json) => process.return(json))
        .catch((error) => process.return(error));
    ">>),

    {ok, #{ <<"headers">> := #{ <<"Header">> := <<"1">> }}} = erlang_v8_lib:run(<<"
    http.get('http://127.0.01:5000/headers', { headers: { 'header': 1 } })
        .then((resp) => resp.json())
        .then((json) => process.return(json))
        .catch((error) => process.return(error));
    ">>),

    {ok, #{ <<"headers">> := #{ <<"1">> := <<"header">> }}} = erlang_v8_lib:run(<<"
    http.get('http://127.0.01:5000/headers', { headers: { 1: 'header' } })
        .then((resp) => resp.json())
        .then((json) => process.return(json))
        .catch((error) => process.return(error));
    ">>),

    ok.

arguments(_Config) ->
    {ok, <<"invalid_url">>} = erlang_v8_lib:run(<<"
    http.get(1).catch((error) => process.return(error));
    ">>),
    ok.

https(_Config) ->
    {ok, Data0} = erlang_v8_lib:run(<<"
    http.get('https://127.0.01:5000/get', {
        body: { test: 'fest' }
    }).then(function(data) {
        process.return(data);
    }).catch(function(err) {
        process.return(err);
    });
    ">>),
    #{ <<"body">> := Body0 } = Data0,
    #{ <<"args">> := #{ <<"test">> := <<"fest">> } } =
        jsx:decode(Body0, [return_maps]),

    ok.
