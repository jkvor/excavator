%% Copyright (c) 2009 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(ex_util).
-export([add/3, store/3, store_value/3, global_store/3, fetch/2, fetch_value/2, configure/3, fetch_config/2, evaluate/2, seconds/0]).

-include("excavator.hrl").

add(#state{dictionary=D}=S, K, V) ->
    V2 =
        case dict:find(K, D) of
            {ok, V1} when is_list(V1) -> lists:append(V1, [V]);
            {ok, V1} -> [V1, V]; 
            error -> [V]
        end,
    S#state{dictionary=dict:store(K, V2, D)}.
    
store(#state{dictionary=D}=S, K, V) ->
    S#state{dictionary=dict:store(K, V, D)}.
    
store_value(S, K, V) ->
    store(S, K, {get_type(V), V}).

global_store(#state{parent=P}=S, K, V) ->
    P1 = case P of
        undefined -> P;
        _ -> global_store(P, K, V)
    end,
    S1 = store(S, K, V),
    S1#state{parent=P1}.
    
fetch(#state{dictionary=D}, K) ->
    case dict:find(K, D) of
        {ok, V} -> V;
        error -> undefined
    end.
    
fetch_value(#state{dictionary=D}, K) ->
    case dict:find(K, D) of
        {ok, {_T, V}} -> strip_types(V);
        error -> undefined
    end.
    
strip_types({Type, Val}) when Type==string;Type==nil;Type==node;Type==list_of_nodes;Type==list_of_strings;Type==mixed ->
    Val;
strip_types(List) when is_list(List) ->
    [strip_types(I) || I <- List];
strip_types(Val) ->
    Val.
    
get_type(undefined) -> nil;
get_type([]) -> nil;
get_type([{A,B,C}|Rest]) when is_binary(A), is_list(B), is_list(C) -> 
    case [undefined || {D,E,F} <- Rest, is_binary(D), is_list(E), is_list(F)] of
        [] -> list_of_nodes;
        _ -> mixed
    end;
get_type([A|Rest]) when is_integer(A) ->
    case [I || I <- Rest, not is_integer(I)] of
        [] -> string;
        _ -> mixed
    end;    
get_type([A|Rest]) when is_list(A) ->
    case [I || I <- Rest, not is_list(I)] of
        [] -> list_of_strings;
        _ -> mixed
    end;
get_type({A,B,C}) when is_binary(A), is_list(B), is_list(C) -> node.
    
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
    ?FETCH_VALUE(State, Key);
evaluate(State, List) when is_list(List) ->
    [evaluate(State, I) || I <- List];
evaluate(_State, Other) -> Other.

seconds() ->
    {_,Secs,Micro} = erlang:now(),
    Secs + (Micro / 1000000).