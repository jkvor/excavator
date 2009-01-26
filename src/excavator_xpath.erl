-module(excavator_xpath).
-export([run/2]).

%% @spec run(XPath, Subject) -> Result
%%		 XPath = string()
%%		 Subject = [{_,_,_}] | string()
%%		 Result = [{_,_,_}]
run(XPath, [{A,B,C}=Subject]) when is_list(XPath), is_binary(A), is_list(B), is_list(C) ->
	mochiweb_xpath:execute(XPath, Subject);
	
run(XPath, Subject0) when is_list(XPath), is_list(Subject0) ->
	Subject = mochiweb_html:parse(Subject0),
	run(XPath, [Subject]).