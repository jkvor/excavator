-module(excavator_worker).

-export([run/1]).

-include("excavator.hrl").

run(Module) when is_atom(Module) ->
	run(apply(Module, instructions, []));
	
run(Instructions) when is_list(Instructions) ->
	{ok, _} = run(Instructions, dict:new()), ok.
	
run([], State) -> {ok, State};
run([Head|Tail], State) ->
	{ok, State1} = process_instructions(Head, State),
	run(Tail, State1).
	
%% ==================================================== %%
%% 						  ASSIGN						%%
%% ==================================================== %%
process_instructions(#assign{ from = #url{ value = URL }, to = To }, State) ->
	{_,_,Body} = excavator_web:request(get, URL, [], []),
	{ok, dict:store(To, {string, Body}, State)};
	
process_instructions(#assign{ from = From, to = To, function = #xpath{ value = XPath } }, State) ->
	{FromType, FromValue} = lookup(From, State),
	ToValue = excavator_xpath:run(XPath, {FromType, FromValue}),
	{ok, dict:store(To, ToValue, State)};
	
process_instructions(#assign{ from = From, to = To, function = #regexp{ value = Regexp } }, State) ->		
	{FromType, FromValue} = lookup(From, State),
	ToValue = excavator_re:run(Regexp, {FromType, FromValue}),
	{ok, dict:store(To, ToValue, State)};

%% ==================================================== %%
%% 						  ASSERT						%%
%% ==================================================== %%
process_instructions(#assert{ name = Name, type = Type }=Assert, State) ->
	case assert(lookup(Name, State),Type) of
		true -> 
			error_logger:info_msg("Assert passed: [~p, ~p]~n", [Name, Type]),
			ok;
		false ->
			erlang:error(assert_failed, [Assert, dict:to_list(State)])
	end,
	{ok, State};

%% ==================================================== %%
%% 						   EACH							%%
%% ==================================================== %%
process_instructions(#each{ name = Name, commands = Commands }, State) ->
	case lookup(Name, State) of
		{list_of_strings, Values} ->
			[run(Commands, dict:store(Name, {string, Value}, State)) || Value <- Values];
		{list_of_nodes, Values} ->
			[run(Commands, dict:store(Name, {node, Value}, State)) || Value <- Values]
	end, 
	{ok, State};
	
%% ==================================================== %%
%% 						  COMMIT						%%
%% ==================================================== %%
process_instructions(#commit{ name = _Name, type = _Type }, State) ->
	%ok = excavator_db:commit(Name, lookup(Name, State), Type);
	{ok, State};
	
%% ==================================================== %%
%% 						   PRINT						%%
%% ==================================================== %%
process_instructions(#print{ format = Format, args = Args }, State) ->
	Args1 = [lookup(Arg, State) || Arg <- Args],
	error_logger:info_msg(Format, Args1),
	{ok, State};
	
%% ==================================================== %%
%% 						  SUB LIST						%%
%% ==================================================== %%
process_instructions(List, State) when is_list(List) ->
	{ok, _State1} = run(List, State),
	{ok, State};
		
%% ==================================================== %%
%% 						   ERROR						%%
%% ==================================================== %%
process_instructions(A, B) ->
	erlang:error(instruction_not_supported, [A, B]).
	
lookup(Key, State) ->
	case dict:find(Key, State) of
		{ok, Value} ->
			Value;
		error ->
			erlang:error(key_not_found, [Key])
	end.
	
assert({list_of_strings,[_|_]}, has_list_items) -> true;

assert({list_of_nodes,[_|_]}, has_list_items) -> true;

assert({list_of_nodes,[{A,B,C}|_]}, has_nodes) when is_binary(A), is_list(B), is_list(C) -> true;

assert({_,Value}, has_text) when is_list(Value), length(Value) > 0 -> true;

assert(_, _) -> false.
