-module(excavator_funs).

-export([assign/2, assert/2, commit/2, print/1, print/2, each/2]).

-include("excavator.hrl").

assign({url, URL}, To) ->
	#assign{ from = #url{ value = URL }, to = To };
	
assign({xpath, From, XPath}, To) ->
	#assign{ function = #xpath{ value = XPath }, from = From, to = To };
	
assign({regexp, From, Regexp}, To) ->
	{ok,RE} = re:compile(Regexp),
	#assign{ function = #regexp{ value = RE }, from = From, to = To };
	
assign(_, _) ->
	exit(assign_type_not_supported).
	
assert(Name, Type) 
 when Type == has_list_items; Type == html_tree_has_nodes; Type == has_text ->
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