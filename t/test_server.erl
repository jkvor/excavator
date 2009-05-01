-module(test_server).
-behaviour(web_server).

-export([start_link/0]).

%% web_server callbacks
-export([dispatch/2]).

start_link() ->
	web_server:start(?MODULE, [{address, "127.0.0.1"}, {port, 8888}]).

%%====================================================================
%% web_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: dispatch(Req, PathTokens) -> {reply, Status, Headers, Body} | 
%%										  {reply, Module, Function, Args} | 
%%										  undefined
%%			 Req = mochiweb_request()
%%			 PathTokens = list()
%%--------------------------------------------------------------------
dispatch(_, _) ->
	undefined.