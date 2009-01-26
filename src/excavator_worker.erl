-module(excavator_worker).

-export([run/1]).

-include("excavator.hrl").

run(Module) when is_atom(Module) ->
	run(apply(Module, instructions, []));
	
run(Instructions) ->
	ok = process_instructions(Instructions, dict:new()).
	
process_instructions([#assign{ from = #url{ value = URL}, to = To }|Tail], State) ->
	{_,_,Body} = excavator_web:request(get, URL, [], []),
	process_instructions(Tail, dict:store(To, Body, State));
	
process_instructions([#assign{ from = From, to = To, function = #xpath{ value = XPath } }|Tail], State) ->
	FromValue = lookup(From, State),
	ToValue = excavator_xpath:run(XPath, FromValue),
	process_instructions(Tail, dict:store(To, ToValue, State));
	
process_instructions([#assign{ from = From, to = To, function = #regexp{ value = Regexp } }|Tail], State) ->		
	FromValue = lookup(From, State),
	ToValue = excavator_re:run(Regexp, FromValue),
	process_instructions(Tail, dict:store(To, ToValue, State));

process_instructions([#assert{ name = Name, type = Type }=Assert|Tail], State) ->
	case assert(lookup(Name, State),Type) of
		true -> 
			error_logger:info_msg("Assert passed: [~p, ~p]~n", [Name, Type]),
			ok;
		false ->
			erlang:error(assert_failed, [[Assert|Tail], dict:to_list(State)])
	end,
	process_instructions(Tail, State);
	
process_instructions([#commit{ name = _Name, type = _Type }|Tail], State) ->
	%ok = excavator_db:commit(Name, lookup(Name, State), Type);
	process_instructions(Tail, State);
	
process_instructions([#print{ format = Format, args = Args }|Tail], State) ->
	Args1 = [lookup(Arg, State) || Arg <- Args],
	error_logger:info_msg(Format, Args1),
	process_instructions(Tail, State);
	
process_instructions([Head|Tail], State) when is_list(Head) ->
	process_instructions(Head, State),
	process_instructions(Tail, State);
	
process_instructions([], _) -> ok;
	
process_instructions(A, B) ->
	erlang:error(instruction_not_supported, [A, B]).
	
lookup(Key, State) ->
	case dict:find(Key, State) of
		{ok, Value} ->
			Value;
		error ->
			erlang:error(key_not_found, [Key])
	end.
	
assert([_|_], has_list_items) -> true;

assert([{A,B,C}|_], html_tree_has_nodes) when is_binary(A), is_list(B), is_list(C) -> true;

assert(Value, has_text) when is_list(Value), length(Value) > 0 -> true;

assert([Value], has_text) when is_binary(Value), size(Value) > 0 -> true;

assert(_, _) -> false.
