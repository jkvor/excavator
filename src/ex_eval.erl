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

%% call(M,F,A)
expand(State, {call, M, F, A}) ->
	apply(expand(State, M), expand(State, F), [expand(State, Arg) || Arg <- A]);

%% {first, key}
%% {last, key}
expand(State, {FirstLast, Values0}) when (FirstLast==first orelse FirstLast==last) ->
    case expand(State, Values0) of
        Values when is_list(Values) ->
            [Val|_] = if FirstLast==first -> Values; true -> lists:reverse(Values) end,
            Val;
        _ ->
            {expand(State, FirstLast), Values0}
    end;

%% Read File
expand(_State, {file, FileLocation}) when is_list(FileLocation) ->
    {ok, Data} = file:read_file(FileLocation),
    binary_to_list(Data);

%% HTTP requests
expand(State, {http_req, Method, Url, Headers, Body}) ->
	ex_throttler:throttle(State, Url),
    Url1 = flatten_url(State, Url),
    ex_web:request(Method, Url1, expand(State, Headers), expand(State, Body));
    
%% HTTP Cookie Value
expand(State, {cookie, CookieName, HttpRespKey}) ->
    case expand(State, HttpRespKey) of
        #http_resp{cookies=Cookies} ->
            case proplists:get_value(CookieName, Cookies) of
                {cookie, _, Fields} ->
                    proplists:get_value(CookieName, Fields);
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end;
         
%% XPath
expand(State, {xpath, Source, XPath}) ->
    ex_xpath:run(expand(State, XPath), expand(State, Source));
    
%% Regexp
expand(State, {regexp, Source, Regexp}) ->
    ex_re:run(State, expand(State, Regexp), expand(State, Source));
    
%% Range
expand(_State, {range, Current, Last}) when (is_integer(Current) orelse is_float(Current)) andalso 
                                            (is_integer(Last) orelse is_float(Last)) ->
    {range, Current, Last, fun(C) -> C+1 end};

expand(_State, {range, Current, Last, Fun}) when is_function(Fun) ->
    {range, Current, Last, Fun};
    
%% Concat
expand(State, {concat, Source}) ->
    case expand(State, Source) of
        List when is_list(List) -> lists:concat(List);
        _ -> {concat, Source}
    end;
    
%% Length
expand(State, {length, Source}) ->
    case expand(State, Source) of
        List when is_list(List) -> length(List);
        _ -> {length, Source}
    end;
    
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
    case ex_util:typeof(Url) of
        string -> Url;
        list -> lists:concat([expand(State, I) || I <- Url])
    end.