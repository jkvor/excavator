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
-module(ex_xpath).
-export([run/2, reassemble/1]).

-include("excavator.hrl").

run(XPath, {http_response, _, _, Body}) ->
    run(XPath, Body);
    
run(XPath, {A,B,C}) when is_binary(A), is_list(B), is_list(C) ->
    run_internal(XPath, {A,B,C});
    
run(XPath, Subject0) when is_list(XPath), is_list(Subject0) ->
    case ex_util:typeof(Subject0) of
        string -> 
        	case (catch mochiweb_html:parse(Subject0)) of
        	    {'EXIT', {{badmatch,[]},_}} ->
        	        ?ERR_REPORT({?MODULE, ?LINE, XPath, xpath_expression_did_not_match}),
        	        exit({error, xpath_expression_did_not_match});
        	    {'EXIT', Error} ->
        	        exit(Error);
        		Subject when is_tuple(Subject) ->
        			run_internal(XPath, Subject)
        	end;
        list ->
            run_internal(XPath, Subject0)
    end.
            
run_internal(XPath, Subject) ->
	Results = 
	[case Result of
	    Bin when is_binary(Bin) -> binary_to_list(Bin);
	    _ -> Result
	end || Result <- mochiweb_xpath:execute(XPath, Subject)],
	case Results of
	    [Single] -> Single;
	    _ -> Results
	end.
	
reassemble({A,B,C}) when is_binary(A), is_list(B), is_list(C) ->
    binary_to_list(iolist_to_binary(mochiweb_html:to_html({A,B,C}))).