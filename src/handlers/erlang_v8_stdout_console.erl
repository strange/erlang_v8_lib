-module(erlang_v8_stdout_console).

-export([run/2]).

run([Level, Message], _HandlerContext) ->
    io:format(standard_error, "[Log(~p)]: ~p~n", [Level, Message]),
    ok.
