-module(ex_pp).
-export([parse/1]).

parse(Filename) when is_list(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            {ok, Tokens, _} = erl_scan:string(Binary),
            erl_parse:parse_exprs(Tokens);
        {error, Reason} ->
            exit(Reason)
    end.
