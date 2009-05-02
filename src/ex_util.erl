-module(ex_util).
-export([store/3, fetch/2, configure/3, fetch_config/2, evaluate/2]).

-include("excavator.hrl").

store(#state{dictionary=D}=S, K, V) ->
    S#state{dictionary=dict:store(K, V, D)}.
    
fetch(#state{dictionary=D}, K) ->
    case dict:find(K, D) of
        {ok, V} -> V;
        error -> undefined
    end.
    
configure(#state{configuration=C}=S, K, V) ->
    S#state{configuration=dict:store(K, V, C)}.
    
fetch_config(#state{configuration=C}, K) ->
    case dict:find(K, C) of
        {ok, V} -> V;
        error -> undefined
    end.

evaluate(State, Tuple) when is_tuple(Tuple) ->
    list_to_tuple([evaluate(State, I) || I <- tuple_to_list(Tuple)]);
evaluate(State, Key) when is_atom(Key) ->
    case ?FETCH(State, Key) of undefined -> Key; {_, Other} -> Other end;
evaluate(_State, Other) -> Other.
