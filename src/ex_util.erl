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
-export([store/3, fetch/2, fetch_value/2, configure/3, fetch_config/2, evaluate/2, seconds/0]).

-include("excavator.hrl").

store(#state{dictionary=D}=S, K, V) ->
    S#state{dictionary=dict:store(K, V, D)}.
    
fetch(#state{dictionary=D}, K) ->
    case dict:find(K, D) of
        {ok, V} -> V;
        error -> undefined
    end.
    
fetch_value(#state{dictionary=D}, K) ->
    case dict:find(K, D) of
        {ok, {_T, V}} -> V;
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

seconds() ->
    {_,Secs,Micro} = erlang:now(),
    Secs + (Micro / 1000000).