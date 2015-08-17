-module(erlang_v8_lib_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    application:start(jsx),
    application:start(erlang_v8),
	erlang_v8_lib_sup:start_link().

stop(_State) ->
	ok.
