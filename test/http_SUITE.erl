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
    Data0 = run_success_case(<<"
    http.get('http://httpbin.org/get?test=fest').then(function(data) {
        process.return(data.body);
    });
    ">>),
    [{<<"test">>, <<"fest">>}] = proplists:get_value(<<"args">>, Data0),

    Data1 = run_success_case(<<"
    http.get('http://httpbin.org/get').then(function(data) {
        process.return(data.body);
    });
    ">>),
    [{}] = proplists:get_value(<<"args">>, Data1),
    ok.

post(_Config) ->
    Data0 = run_success_case(<<"
    http.post('http://httpbin.org/post', 'hello').then(function(data) {
        process.return(data.body);
    });
    ">>),
    <<"hello">> = proplists:get_value(<<"data">>, Data0),

    ok.

run_success_case(Source) ->
    {ok, Data} = erlang_v8_lib:run(Source),
    jsx:decode(Data).
