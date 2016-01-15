-module(erlang_v8_lib_utils).

-export([extend/3]).

extend(N, L1, L2) ->
    compact(lists:keysort(N, L1 ++ L2), []).

compact([], Acc) ->
    lists:reverse(Acc);
compact([{K,_}, {K,V}| Rest], Acc) ->
    compact([{K,V} |Rest],Acc);
compact([X|Rest], Acc) ->
    compact(Rest,[X|Acc]).
