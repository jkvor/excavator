-module(ex_xpath).
-export([run/2]).


%% @spec run(XPath, Subject) -> Result
%%		 XPath = string()
%%		 Subject = {Type, Value}
%%		 Result = {nil, []} | {node, _} | {list_of_nodes, _} | {string, _} | {list_of_strings, _}
run(XPath, {string, Subject0}) when is_list(XPath), is_list(Subject0) ->
	case mochiweb_html:parse(Subject0) of
		Subject when is_tuple(Subject) ->
			run(XPath, {node, Subject});
		_ ->
			exit({?MODULE, ?LINE, XPath, Subject0})
	end;
	
run(XPath, {node, Subject}) when is_list(XPath), is_tuple(Subject) ->
	case mochiweb_xpath:execute(XPath, Subject) of
		[] -> 
			{nil, []};
		[Node] when is_tuple(Node) -> 
			{node, Node};
		[Text] when is_list(Text) -> 
			{string, Text};
		[Text] when is_binary(Text) -> 
			{string, binary_to_list(Text)};
		[Node|_] = Nodes when is_tuple(Node) -> 
			{list_of_nodes, Nodes};
		[Text|_] = List when is_list(Text) -> 
			{list_of_strings, List};
		[Text|_] = List when is_binary(Text) -> 
			{list_of_strings, [binary_to_list(Bin) || Bin <- List]};
		_ -> 
			exit({?MODULE, ?LINE, XPath, Subject})
	end.