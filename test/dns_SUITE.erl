-module(dns_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([generic/1]).

%% Callbacks

all() ->
    [
     generic
    ].

init_per_suite(Config) ->
    application:ensure_all_started(erlang_v8_lib),
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    {ok, Pid} = erlang_v8_lib_sup:start_link(),
    [{pid, Pid}|Config].

end_per_testcase(_Case, Config) ->
    Pid = proplists:get_value(pid, Config),
    exit(Pid, normal),
    ok.

%% Tests

generic(_Config) ->
    {ok, #{ <<"validInSeconds">> := _ }} =  erlang_v8_lib:run(<<"
        dns.resolve('google.com', 'a')
        .then((x) => process.return(x))
        .catch((x) => process.return(x));
    ">>),
    ok.
