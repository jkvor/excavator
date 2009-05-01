-module(ex_re).
-export([run/2]).

%% @spec run(Regexp, Subject) -> Result
%%		 Regexp = {re_pattern, _, _, _}
%%		 Subject = {Type, Value}
%%		 Result = {nil, _} | {string, _} | {list_of_strings, _}
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