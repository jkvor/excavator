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
-export([add/3, global_add/3, store/3, global_store/3, fetch/2, configure/3, fetch_config/2, seconds/0, typeof/1]).

-include("excavator.hrl").

add(#state{dictionary=D}=S, K, V) ->
    V2 =
        case dict:find(K, D) of
            {ok, V1} when is_list(V1) -> lists:append(V1, [V]);
            {ok, V1} -> [V1, V]; 
            error -> [V]
        end,
    S#state{dictionary=dict:store(K, V2, D)}.

global_add(#state{parent=P}=S, K, V) ->
    P1 = case P of
        undefined -> P;
        _ -> global_add(P, K, V)
    end,
    S1 = add(S, K, V),
    io:format("S1#state{parent=P1}: ~p~n", [S1#state{parent=P1}]),
    S1#state{parent=P1}.
    
store(#state{dictionary=D}=S, K, V) ->
    S#state{dictionary=dict:store(K, V, D)}.

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

configure(#state{configuration=C}=S, K, V) ->
    S#state{configuration=dict:store(K, V, C)}.
    
fetch_config(#state{configuration=C}, K) ->
    case dict:find(K, C) of
        {ok, V} -> V;
        error -> undefined
    end.

seconds() ->
    {_,Secs,Micro} = erlang:now(),
    Secs + (Micro / 1000000).

typeof({A,B,C}) when is_binary(A), is_list(B), is_list(C) -> node;
typeof(HttpResponse) when is_record(HttpResponse, http_response) -> http_response;
typeof({range, A, B}) when is_integer(A), is_integer(B) -> range;
typeof([H|_]=List) when is_integer(H) ->
    lists:foldl(
        fun (I, string) when is_integer(I) -> string;
            (_, _) -> list
        end, string, List);
typeof(List) when is_list(List) -> list;
typeof(_) -> term.