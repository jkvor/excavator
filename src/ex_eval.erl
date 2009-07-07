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
-module(ex_eval).
-export([expand/2]).

-include("excavator.hrl").

%% {first, key}
%% {last, key}
expand(State, {FirstLast, Key}) when (FirstLast==first orelse FirstLast==last) andalso is_atom(Key) ->
    case ?FETCH(State, Key) of
        Values when is_list(Values) ->
            [Val|_] = if FirstLast==first -> Values; true -> lists:reverse(Values) end,
            Val;
        undefined ->
            {expand(State, FirstLast), Key}
    end;
    
%% HTTP requests
expand(State, {http, Method, Url}) when Method==options;Method==get;Method==head;Method==delete;Method==trace ->
    expand(State, {http, Method, Url, [], []});
expand(State, {http, Method, Url, Headers}) when Method==options;Method==get;Method==head;Method==delete;Method==trace ->
    expand(State, {http, Method, Url, Headers, []});
expand(State, {http, Method, Url, Headers, Body}) ->
    Url1 = flatten_url(State, Url),
    ex_web:request(Method, Url1, Headers, Body);
    
%% XPath
expand(State, {xpath, Source, XPath}) ->
    ex_xpath:run(XPath, expand(State, Source));
    
%% Regexp
expand(State, {regexp, Source, Regexp}) ->
    ex_re:run(State, Regexp, expand(State, Source));
    
%% Range
expand(_State, {range, Current, Last}) ->
    {range, Current, Last};
    
%% Key
expand(State, Key) when is_atom(Key) ->
    case ?FETCH(State, Key) of
        undefined -> Key;
        Value -> expand(State, Value)
    end;

%% List
expand(State, List) when is_list(List) ->
    [expand(State, I) || I <- List];
    
%% Tuple
expand(State, Tuple) when is_tuple(Tuple) ->
    list_to_tuple([expand(State, I) || I <- tuple_to_list(Tuple)]);
    
%% Other
expand(_, Other) -> Other.

%% =============================================================================
%% == Internal Functions
%% =============================================================================
flatten_url(State, {Url, Props}) ->
    Url1 = flatten_url(State, Url),
    Props1 = [{K, expand(State, V)} || {K,V} <- Props],
    Props2 = mochiweb_util:urlencode(Props1),
    Url1 ++ "?" ++ Props2;
    
flatten_url(State, Url) ->
    lists:flatten([expand(State, I) || I <- Url]).