-module(excavator_worker).

-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

start_link(_) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
    {ok, []}.

handle_call(_, _From, State) -> {reply, {error, invalid_call}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

