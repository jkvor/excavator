-module(excavator_re).
-export([
	run/2
]).

run(Regexp, Subject) when is_list(Regexp), is_list(Subject) ->
	case re:compile(Regexp) of
		{ok, MP} ->
			case re:run(Subject, MP) of
				nomatch -> 
					[];
				{match, Captured} ->
					[begin
						string:substr(Subject, Start+1, Length)
					 end || {Start, Length} <- Captured]
			end;
		{error, Reason} -> 
			exit(Reason)
	end.