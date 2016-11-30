-module(erlang_v8_lib_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start(_Type, _Args) ->
    erlang_v8_lib_app:start_link().

stop(_State) ->
    ok.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 3,
        period => 10
    },
    ChildSpecs = [#{
        id => erlang_v8_lib_vm_sup,
        start => {erlang_v8_lib_vm_sup, start_link, []},
        type => supervisor
    }],
    {ok, {SupFlags, ChildSpecs}}.
