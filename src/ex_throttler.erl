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
-module(ex_throttler).
-export([throttle/2]).

throttle(State, Url) ->
	case ex_util:fetch_config(State, qps) of
        undefined ->
            ok;
		List when is_list(List) ->
			match_url(Url, List);
        QPS when is_number(QPS) ->
			maybe_sleep(QPS)
    end.

match_url(_, []) -> ok;
match_url(Url, [{Match, QPS}|Tail]) -> 
	case Url of
		Prefix ++ _ -> maybe_sleep(QPS);
		_ -> match_url(Url, Tail)
	end.

maybe_sleep(QPS) ->
	{Queries, Unit} = 
		case QPS of
			I when is_integer(I) ->
				{QPS, 1};
			F when is_float(F) ->
				Multiplier = kellys_crazy_formula(F, F, 1),
				{QPS*Multiplier, Multiplier}
		end,
	if
		length(Times) > Queries ->
			%% we've exceeded the number of allowed qps and we need to wait
			Oldest = tail(Times),
			SleepTime = Oldest - (ex_util:seconds() - Unit),
			timer:sleep(SleepTime),
			%% create a new time window to now and remove times that are outside of it
			Cutoff = (ex_util:seconds() - Unit),
			Times1 = lists:filter(fun(Time) -> Time > Cutoff end, Times),
			State#state{request_times=[ex_util:seconds()|Times1]};
		true ->
			State#state{request_times=[ex_util:seconds()|Times]}
	end.
	
kellys_crazy_formula(Original, Acc, 11) -> exit(qps_value_is_too_freakin_weird__why_dont_you_try_a_nice_whole_number);
kellys_crazy_formula(Original, Acc, Count) when is_integer(Acc) -> Count;
kellys_crazy_formula(Original, Acc, Count) when is_float(Acc) ->
	if
		Acc - trunc(Acc) == Acc ->
			Count;
		true ->
			kellys_crazy_formula(Original, Acc+Original, Count+1)
	end.