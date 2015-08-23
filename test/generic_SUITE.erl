-module(generic_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([console_log/1]).

%% Callbacks

all() ->
    [
        console_log
    ].

init_per_suite(Config) ->
    application:ensure_all_started(erlang_v8_lib),
    Config.

end_per_suite(_Config) ->
    ok.

%% Tests

console_log(_Config) ->
    {ok, ok} = erlang_v8_lib:run(<<"console.log('test');">>),
    ok.
