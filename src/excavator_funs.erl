-module(excavator_funs).

-export([assign/2, assert/2, commit/2, print/1, print/2, each/2]).

-include("excavator.hrl").

assign({url, URL}, To) ->
	#assign{ from = #url{ value = URL }, to = To };
	
assign({file, File}, To) ->
	#assign{ from = #file{ name = File }, to = To };
	
assign({range, Start, End}, To) ->
	#assign{ from = #range{ current = Start, stop = End, incr_fun = fun(A) -> A+1 end }, to = To };
	
assign({xpath, From, XPath}, To) ->
	#assign{ function = #xpath{ value = XPath }, from = From, to = To };
	
assign({regexp, From, Regexp}, To) ->
	{ok,RE} = re:compile(Regexp),
	#assign{ function = #regexp{ value = RE }, from = From, to = To };
	
assign(_, _) ->
	exit(assign_type_not_supported).
	
assert(Name, List) when is_list(List) ->
	[assert(Name, ListItem) || ListItem <- List];
	
assert(Name, {size, Size}) ->
	#assert{ name = Name, type = {size, Size} };
	
assert(Name, Type) 
 when Type == has_list_items; Type == has_nodes; Type == has_text; Type == has_node; Type == has_range ->
	#assert{ name = Name, type = Type };

assert(_, _) ->
	exit(assert_type_not_supported).

commit(Name, Type) 
 when Type == int32; Type == string ->
	#commit{ name = Name, type = Type };
	
commit(_, _) ->
	exit(commit_type_not_supported).
	
print(Name) when is_atom(Name) ->
	print(atom_to_list(Name) ++ ": ~p~n", [Name]).
	
print(Format, Args) ->
	#print{ format = Format, args = Args }.
	
each(Name, Commands) ->
	#each{ name = Name, commands = Commands}.