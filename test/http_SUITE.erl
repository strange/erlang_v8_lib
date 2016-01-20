-module(http_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([get/1]).
-export([post/1]).

%% Callbacks

all() ->
    [
        get,
        post
    ].

init_per_suite(Config) ->
    application:ensure_all_started(erlang_v8_lib),
    Config.

end_per_suite(_Config) ->
    ok.

%% Tests

get(_Config) ->
    {ok, Data0} = erlang_v8_lib:run(<<"
    http.get('http://httpbin.org/get?test=fest').then(function(data) {
        process.return(data);
    });
    ">>),
    #{ <<"body">> := Body0 } = Data0,
    #{ <<"args">> := #{ <<"test">> := <<"fest">> } } =
        jsx:decode(Body0, [return_maps]),

    {ok, Data1} = erlang_v8_lib:run(<<"
    http.get('http://httpbin.org/status/404').then(function(data) {
        process.return(data);
    });
    ">>),
    #{ <<"code">> := 404 } = Data1,

    {ok, Data2} = erlang_v8_lib:run(<<"
    http.get('http://httpbin.org/status/200').then(function(data) {
        process.return(data);
    });
    ">>),
    true = is_number(maps:get(<<"time">>, Data2)),

    ok.

post(_Config) ->
    {ok, Data0} = erlang_v8_lib:run(<<"
    http.post('http://httpbin.org/post', 'hello').then(function(data) {
        process.return(data);
    });
    ">>),
    #{ <<"body">> := Body0 } = Data0,
    #{ <<"data">> := <<"hello">> } = jsx:decode(Body0, [return_maps]),

    ok.
