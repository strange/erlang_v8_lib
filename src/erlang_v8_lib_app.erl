-module(erlang_v8_lib_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	erlang_v8_lib_sup:start_link().

stop(_State) ->
	ok.
