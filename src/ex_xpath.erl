-module(ex_xpath).
-export([run/2, reassemble/1]).

%% @spec run(XPath, Subject) -> Result
%%		 XPath = string()
%%		 Subject = {Type, Value}
%%		 Result = {nil, []} | {node, _} | {list_of_nodes, _} | {string, _} | {list_of_strings, _}
run(XPath, {http_response, _, _, Body}) ->
    run(XPath, {string, Body});
    
run(XPath, {string, Subject0}) when is_list(XPath), is_list(Subject0) ->
	case mochiweb_html:parse(Subject0) of
		Subject when is_tuple(Subject) ->
			run(XPath, {node, Subject});
		_ ->
			exit({?MODULE, ?LINE, XPath, Subject0})
	end;
	
run(XPath, {node, Subject}) when is_list(XPath), is_tuple(Subject) ->
	R = mochiweb_xpath:execute(XPath, Subject),
	case R of
		[] -> 
			{nil, []};
		[I|_] = String when is_integer(I) ->
			{string, String};
		List when is_list(List) ->
			lists:foldr(
				fun (Bin, {list_of_strings, Acc}) when is_binary(Bin) ->
						{list_of_strings, [{string, binary_to_list(Bin)}|Acc]};
					(Bin, {string, Acc}) when is_binary(Bin) ->
						{list_of_strings, [{string, binary_to_list(Bin)}, {string, Acc}]};
					(Bin, {undefined, []}) when is_binary(Bin) ->
						{string, binary_to_list(Bin)};
					(Bin, {node, _}=Acc) when is_binary(Bin) ->
						{mixed, [{string, binary_to_list(Bin)},Acc]};
					(Bin, {list_of_nodes, Acc}) when is_binary(Bin) ->
						{mixed, [{string, binary_to_list(Bin)}|Acc]};
					(Bin, {mixed, Acc}) when is_binary(Bin) ->
						{mixed, [{string, binary_to_list(Bin)}|Acc]};
					({_,_,_}=Node, {list_of_nodes, Acc}) ->
						{list_of_nodes, [{node, Node}|Acc]};
					({_,_,_}=Node, {node, Acc}) ->
						{list_of_nodes, [{node, Node}, {node, Acc}]};
					({_,_,_}=Node, {undefined, []}) ->
						{node, Node};
					({_,_,_}=Node, {list_of_strings, Acc}) ->
						{mixed, [{node, Node}|Acc]};
					({_,_,_}=Node, {mixed, Acc}) ->
						{mixed, [{node, Node}|Acc]};
					({_,_,_}=Node, {string, _}=Acc) ->
						{mixed, [{node, Node},Acc]};
					(Other, {Type,Acc}) ->
						exit({?MODULE, ?LINE, Other, {Type,Acc}})
				end, {undefined, []}, List);
		_ ->
			exit({?MODULE, ?LINE, XPath, Subject})
	end.
		
	% 	[Node] when is_tuple(Node) -> 
	% 		{node, Node};
	% 	[Text] when is_list(Text) -> 
	% 		{string, Text};
	% 	[Text] when is_binary(Text) -> 
	% 		{string, binary_to_list(Text)};
	% 	[Node|_] = Nodes when is_tuple(Node) -> 
	% 		{list_of_nodes, Nodes};
	% 	[Text|_] = List when is_list(Text) -> 
	% 		{list_of_strings, List};
	% 	[Text|_] = List when is_binary(Text) -> 
	% 		{list_of_strings, [binary_to_list(Bin) || Bin <- List]};
	% 	String when is_list(String) ->
	% 		{string, String};
	% 	_ -> 
	% 		exit({?MODULE, ?LINE, XPath, Subject})
	% end.
	
reassemble({node, Node}) ->
    {string, binary_to_list(iolist_to_binary(mochiweb_html:to_html(Node)))}.