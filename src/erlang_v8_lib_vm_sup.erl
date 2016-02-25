-module(erlang_v8_lib_vm_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_child/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE, [Args]).

init(_Args) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 2,
        period => 30
    },
    ChildSpecs = [#{
        id => erlang_v8_vm,
        start => {erlang_v8_vm, start_link, []},
        restart => transient
    }],
    {ok, {SupFlags, ChildSpecs}}.
