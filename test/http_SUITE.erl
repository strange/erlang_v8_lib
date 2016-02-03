-module(http_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([get/1]).
-export([post/1]).
-export([put/1]).
-export([delete/1]).
-export([head/1]).

%% Callbacks

all() ->
    [
        get,
        post,
        put,
        delete,
        head
    ].

init_per_suite(Config) ->
    application:ensure_all_started(erlang_v8_lib),
    Config.

end_per_suite(_Config) ->
    ok.

%% Tests

get(_Config) ->
    {ok, Data0} = erlang_v8_lib:run(<<"
    http.get('http://httpbin.org/get', {test: 'fest'}).then(function(data) {
        process.return(data);
    });
    ">>),
    #{ <<"body">> := Body0 } = Data0,
    #{ <<"args">> := #{ <<"test">> := <<"fest">> } } =
        jsx:decode(Body0, [return_maps]),

    {ok, Data1} = erlang_v8_lib:run(<<"
    http.get('http://httpbin.org/get?test=fest').then(function(data) {
        process.return(data);
    });
    ">>),
    #{ <<"body">> := Body1 } = Data1,
    #{ <<"args">> := #{ <<"test">> := <<"fest">> } } =
        jsx:decode(Body1, [return_maps]),

    {ok, Data2} = erlang_v8_lib:run(<<"
    http.get('http://httpbin.org/status/404').then(function(data) {
        process.return(data);
    });
    ">>),
    #{ <<"code">> := 404 } = Data2,

    {ok, Data3} = erlang_v8_lib:run(<<"
    http.get('http://httpbin.org/status/200').then(function(data) {
        process.return(data);
    });
    ">>),
    true = is_number(maps:get(<<"time">>, Data3)),

    {ok, Data4} = erlang_v8_lib:run(<<"
    http.get('http://httpbin.org/get',
             [{'Content-Type': 'application/json'}],
             {test: 'fest'}).then(function(data) {
        process.return(data);
    });
    ">>),
    #{ <<"body">> := Body4 } = Data4,
    #{ <<"args">> := #{ <<"test">> := <<"fest">> } } =
        jsx:decode(Body4, [return_maps]),

    {ok, Data5} = erlang_v8_lib:run(<<"
    http.get('http://httpbin.org/get?test=fest',
             [{'Content-Type': 'application/json'}]).then(function(data) {
        process.return(data);
    });
    ">>),
    #{ <<"body">> := Body5 } = Data5,
    #{ <<"args">> := #{ <<"test">> := <<"fest">> } } =
        jsx:decode(Body5, [return_maps]),

    ok.

post(_Config) ->
    {ok, Data0} = erlang_v8_lib:run(<<"
    http.post('http://httpbin.org/post', 'hello').then(function(data) {
        process.return(data);
    });
    ">>),
    #{ <<"body">> := Body0 } = Data0,
    #{ <<"data">> := <<"hello">> } = jsx:decode(Body0, [return_maps]),

    {ok, Data1} = erlang_v8_lib:run(<<"
    http.post('http://httpbin.org/post',
               [{'Content-Type': 'application/json'}],
               'hello').then(function(data) {
        process.return(data);
    });
    ">>),
    #{ <<"body">> := Body1 } = Data1,
    #{ <<"data">> := <<"hello">> } = jsx:decode(Body1, [return_maps]),

    {ok, Data2} = erlang_v8_lib:run(<<"
    http.post('http://httpbin.org/post',
               [{'Content-Type': 'application/json', Connection:
               'keep-alive'}], 'hello').then(function(data) {
        process.return(data);
    });
    ">>),
    #{ <<"body">> := Body2 } = Data2,
    #{ <<"data">> := <<"hello">> } = jsx:decode(Body2, [return_maps]),
    ok.

put(_Config) ->
    {ok, Data0} = erlang_v8_lib:run(<<"
    http.put('http://httpbin.org/put',
               [{'Content-Type': 'application/json'}],
               'hello').then(function(data) {
        process.return(data);
    });
    ">>),
    #{ <<"code">> := 200 } = Data0,
    #{ <<"body">> := Body0 } = Data0,
    #{ <<"data">> := <<"hello">> } = jsx:decode(Body0, [return_maps]),

    ok.

delete(_Config) ->
    {ok, Data0} = erlang_v8_lib:run(<<"
    http.delete('http://httpbin.org/delete',
               [{'Content-Type': 'application/json'}],
               'hello').then(function(data) {
        process.return(data);
    });
    ">>),
    #{ <<"code">> := 200 } = Data0,
    #{ <<"body">> := Body0 } = Data0,
    #{ <<"data">> := <<"hello">> } = jsx:decode(Body0, [return_maps]),

    ok.

head(_Config) ->
    {ok, Data0} = erlang_v8_lib:run(<<"
    http.head('http://httpbin.org').then(function(resp) {
        process.return(resp);
    });
    ">>),
    #{<<"code">> := 200 } = Data0,

    ok.
