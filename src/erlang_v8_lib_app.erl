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
	{ok, {{one_for_one, 3, 10}, []}}.
