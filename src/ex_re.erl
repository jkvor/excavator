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
-module(ex_re).
-export([run/3]).

-include("excavator.hrl").

run(State, Regexp, Term) when is_list(Regexp) ->
    {ok, RE} = re:compile(Regexp),
    run(State, RE, Term);
    
run(State, Regexp, Term) when is_tuple(Regexp) ->
    Subject = stringify(State, Term),
	case re:run(Subject, Regexp, [global]) of
		nomatch -> 
			[];
		{match, Match} ->
			case process(Match, Subject, []) of
				[] -> [];
				[String] when is_list(String) -> String;
				List when is_list(List) -> lists:reverse(List)
			end
	end.
	
stringify(_, HttpResponse) when is_record(HttpResponse, http_response) ->
    HttpResponse#http_response.body;
stringify(_, {A,B,C}) when is_binary(A), is_list(B), is_list(C) ->
	ex_xpath:reassemble({A,B,C});
stringify(S, List) when is_list(List) ->
    lists:flatten([stringify(S, I) || I <- List]);
stringify(S, Tuple) when is_tuple(Tuple) ->
    list_to_tuple([stringify(S, I) || I<- tuple_to_list(Tuple)]);
stringify(S, Key) when is_atom(Key) ->
    case ?FETCH(S, Key)	of
        undefined -> lists:flatten(io_lib:format("~p", [Key]));
        Value -> stringify(S, Value)
    end;
stringify(_, Int) when is_integer(Int) -> Int;
stringify(_, Term) ->
    lists:flatten(io_lib:format("~p", [Term])).

process([], _, Acc) -> Acc;

process([ [ {_,_}, {Start,Length} ] |Tail], Subject, Acc) ->
	Acc1 = [string:substr(Subject, Start+1, Length)|Acc],
	process(Tail, Subject, Acc1);

process(A,_,_) ->
	exit({error, {"Cannot process regexp results", A}}).