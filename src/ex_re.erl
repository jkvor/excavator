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
-export([run/2]).

%% @spec run(Regexp, Subject) -> Result
%%		 Regexp = {re_pattern, _, _, _}
%%		 Subject = {Type, Value}
%%		 Result = {nil, _} | {string, _} | {list_of_strings, _}
run(Regexp, {node, Subject}) when is_tuple(Regexp), is_tuple(Subject) ->
    run(Regexp, ex_xpath:reassemble({node, Subject}));
    
run(Regexp, {list_of_nodes, Nodes}) when is_tuple(Regexp), is_list(Nodes) ->
    String1 = lists:concat([begin 
        {string, String} = ex_xpath:reassemble({node, Subject}), 
        String 
    end || {node, Subject} <- Nodes]),
    run(Regexp, {string, String1});
    
run(Regexp, {list_of_strings, Strings}) when is_tuple(Regexp), is_list(Strings) ->
    String1 = lists:concat([String || {string, String} <- Strings]), 
    run(Regexp, {string, String1});
    
run(Regexp, {string, Subject}) when is_tuple(Regexp), is_list(Subject) ->
	case re:run(Subject, Regexp, [global]) of
		nomatch -> 
			{nil, []};
		{match, Match} ->
			case process(Match, Subject, []) of
				[] -> {nil, []};
				[String] when is_list(String) -> {string, String};
				[String|_] = List when is_list(String) -> {list_of_strings, lists:reverse(List)}
			end
	end.
	
process([], _, Acc) -> Acc;

process([ [ {_,_}, {Start,Length} ] |Tail], Subject, Acc) ->
	Acc1 = [string:substr(Subject, Start+1, Length)|Acc],
	process(Tail, Subject, Acc1);

process(A,_,_) ->
	erlang:error("Cannot process regexp results", [A]).