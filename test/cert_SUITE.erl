-module(cert_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([success/1]).
-export([invalid_domain/1]).
-export([invalid_port/1]).

%% Callbacks

all() ->
    [
        %% success,
        %% invalid_domain,
        %% invalid_port
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

success(_Config) ->
    {ok, #{ <<"validInSeconds">> := _ }} =  erlang_v8_lib:run(<<"
        cert.validity('google.com', 443)
        .then((x) => process.return(x));
    ">>),
    ok.

invalid_domain(_Config) ->
    {ok, <<"nxdomain">>} =  erlang_v8_lib:run(<<"
        cert.validity('imadomainthatdoesnotexistwitharandomxxx.com', 443)
        .catch((x) => process.return(x));
    ">>),
    ok.

invalid_port(_Config) ->
    {ok, <<"Invalid TLS", _/binary>>} =  erlang_v8_lib:run(<<"
        cert.validity('google.com', 80)
        .catch((x) => process.return(x));
    ">>),
    ok.
