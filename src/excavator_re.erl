-module(excavator_re).
-export([run/2]).

%% @spec run(Regexp, Subject) -> Result
%%		 Regexp = {re_pattern, _, _, _}
%%		 Subject = [<<"subject">>] | "subject"
%%		 Result = ["captured"|_]
run(Regexp, [Subject]) when is_tuple(Regexp), is_binary(Subject) ->	
	run(Regexp, binary_to_list(Subject));
	
run(Regexp, Subject) when is_tuple(Regexp), is_list(Subject) ->
	Value =
		case re:run(Subject, Regexp) of
			nomatch -> 
				[];
			{match, [_|Captured]} ->
				[string:substr(Subject, Start+1, Length) || {Start, Length} <- Captured]
		end,
	Value.