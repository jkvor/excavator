-module(excavator_consumer).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, 
		 handle_info/2, terminate/2, code_change/3]).
		
-export([execute/1]).

-include("excavator.hrl").

execute(State) ->
	{ok, State1} = gen_server:call(pg2:get_closest_pid(excavator_consumer_grp), {execute, State}, infinity),
	{ok, State1}.
	
start_link(_) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
	ok = pg2:create(excavator_consumer_grp),
	ok = pg2:join(excavator_consumer_grp, self()),
    {ok, []}.

handle_call({execute, {state, Instructions, Dict, Stack}}, _From, S) ->	
	{reply, process_instructions(Instructions, Dict, Stack), S};
	
handle_call(_Message, _From, State) -> {reply, {error, invalid_call}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% internal

%% ==================================================== %%
%% 						  ASSIGN						%%
%% ==================================================== %%
process_instructions([#assign{ from = #url{ value = URL }, to = To }|Tail], Dict, Stack) ->
	{_,_,Body} = excavator_web:request(get, URL, [], []),
	{ok, {state, Tail, dict:store(To, {string, Body}, Dict), Stack}};
	
process_instructions([#assign{ from = #file{ name = Filename }, to = To }|Tail], Dict, Stack) ->
	case file:read_file(Filename) of
		{ok, Binary} -> 
			Contents = binary_to_list(Binary),
			{ok, {state, Tail, dict:store(To, {string, Contents}, Dict), Stack}};
		{error, Reason} ->
			erlang:error("Error reading file: " ++ Reason, [Filename])
	end;
	
process_instructions([#assign{ from = #range{} = Range, to = To }|Tail], Dict, Stack) ->
	{ok, {state, Tail, dict:store(To, Range, Dict), Stack}};
	
process_instructions([#assign{ from = From, to = To, function = #xpath{ value = XPath } }|Tail], Dict, Stack) ->
	{FromType, FromValue} = lookup(From, Dict),
	ToValue = excavator_xpath:run(XPath, {FromType, FromValue}),
	{ok, {state, Tail, dict:store(To, ToValue, Dict), Stack}};
	
process_instructions([#assign{ from = From, to = To, function = #regexp{ value = Regexp } }|Tail], Dict, Stack) ->		
	{FromType, FromValue} = lookup(From, Dict),
	ToValue = excavator_re:run(Regexp, {FromType, FromValue}),
	{ok, {state, Tail, dict:store(To, ToValue, Dict), Stack}};

%% ==================================================== %%
%% 						  ASSERT						%%
%% ==================================================== %%
process_instructions([#assert{ name = Name, type = Type }=Assert|Tail], Dict, Stack) ->
	case assert(lookup(Name, Dict),Type) of
		true -> 
			?INFO_MSG("Assert passed: [~p, ~p]~n", [Name, Type]),
			ok;
		false ->
			erlang:error(assert_failed, [Assert])
	end,
	{ok, {state, Tail, Dict, Stack}};

%% ==================================================== %%
%% 						   EACH							%%
%% ==================================================== %%	
process_instructions([#each{ name = Name, commands = Commands }|Tail]=Instrs, Dict, Stack) ->
	case lookup(Name, Dict) of
		{_, []} -> 
			{ok, {state, Tail, Dict, Stack}};
		{list_of_strings, [Head|Rest]} ->
			Dict1 = dict:store(Name, {string, Head}, Dict),
			Dict2 = dict:store(Name, {list_of_strings, Rest}, Dict),
			NewStack = [{Dict2, Instrs},{Dict, Tail}|Stack],
			{ok, {state, Commands, Dict1, NewStack}};
		{list_of_nodes, [Head|Rest]} ->
			Dict1 = dict:store(Name, {node, Head}, Dict),
			Dict2 = dict:store(Name, {list_of_nodes, Rest}, Dict),
			NewStack = [{Dict2, Instrs}|Stack],
			{ok, {state, Commands, Dict1, NewStack}};		
		{range, Stop, Stop, _} ->
			Dict1 = dict:store(Name, {string, Stop}, Dict),
			NewStack = [{Dict, Tail}|Stack],
			{ok, {state, Commands, Dict1, NewStack}};
		{range, Current, Stop, Inc_Fun} ->
			Dict1 = dict:store(Name, {string, Current}, Dict),
			NewRange = {range, Inc_Fun(Current), Stop, Inc_Fun},
			Dict2 = dict:store(Name, NewRange, Dict),
			NewStack = [{Dict2, Instrs}|Stack],
			{ok, {state, Commands, Dict1, NewStack}}
	end;
	
%% ==================================================== %%
%% 						  COMMIT						%%
%% ==================================================== %%
process_instructions([#commit{ name = _Name, type = _Type }|Tail], Dict, Stack) ->
	%ok = excavator_db:commit(Name, lookup(Name, State), Type);
	{ok, {state, Tail, Dict, Stack}};
	
%% ==================================================== %%
%% 						   PRINT						%%
%% ==================================================== %%
process_instructions([#print{ format = Format, args = Args }|Tail], Dict, Stack) ->
	Args1 = [lookup(Arg, Dict) || Arg <- Args],
	?INFO_MSG(Format, Args1),
	{ok, {state, Tail, Dict, Stack}};

%% ==================================================== %%
%% 						    POP							%%
%% ==================================================== %%
process_instructions([], _, []) ->
	{ok, done};

process_instructions([], _, [{Dict, Tail}|Stack]) ->
	{ok, {state, Tail, Dict, Stack}};
		
%% ==================================================== %%
%% 						  SUB LIST						%%
%% ==================================================== %%
process_instructions([Head|Tail], Dict, Stack) when is_list(Head) ->
	NewStack = [{Dict, Tail}|Stack],
	{ok, {state, Head, Dict, NewStack}};
				
%% ==================================================== %%
%% 						   ERROR						%%
%% ==================================================== %%
process_instructions(A, B, C) ->
	erlang:error(instruction_not_supported, [A, B, C]).

lookup(Key, Dict) ->
	case dict:find(Key, Dict) of
		{ok, Value} ->
			Value;
		error ->
			erlang:error(key_not_found, [Key])
	end.
	
assert({list_of_strings, Value}, {size, Size}) when is_list(Value), is_integer(Size) -> length(Value) == Size;

assert({list_of_nodes, Value}, {size, Size}) when is_list(Value), is_integer(Size) -> length(Value) == Size;

assert({list_of_strings,[_|_]}, has_list_items) -> true;

assert({list_of_nodes,[_|_]}, has_list_items) -> true;

assert({list_of_nodes,[{A,B,C}|_]}, has_nodes) when is_binary(A), is_list(B), is_list(C) -> true;

assert({_,Value}, has_text) when is_list(Value), length(Value) > 0 -> true;

assert({node, Value}, has_node) when is_tuple(Value) -> true;

assert(_, _) -> false.
