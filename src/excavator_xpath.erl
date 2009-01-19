-module(excavator_xpath).
-export([
	run/2
]).

run(XPath, Subject) when is_list(XPath), is_list(Subject) ->
	XmlElement =
		case xmerl_scan:string(Subject) of
			{X, []} -> X;
			{X, _Rest} -> X;
			Err -> exit(Err)
		end,
	xmerl_path:string(XPath, XmlElement).
