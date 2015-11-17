-module(erlang_v8_hydna).

-export([run/2]).

run([<<"emit">>, Path, Data], #{ domain_name := DomainName }) ->
    hydna_domain:emit_to_channel(DomainName, Path, Data),
    {ok, <<>>}.
