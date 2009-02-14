-module(excavator_mgr).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, 
		 handle_info/2, terminate/2, code_change/3]).

-export([register/2, next/1, stop/1]).

start_link(_) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
	ok = pg2:create(excavator_mgr_grp),
	ok = pg2:join(excavator_mgr_grp, self()),
    {ok, dict:new()}.

handle_call({register, Name, Instructions}, _From, Dict) ->	
	{reply, ok, dict:store(Name, {state, Instructions, dict:new(), []}, Dict)};
	
handle_call({next, Name}, _From, Dict) ->
	case dict:find(Name, Dict) of
		{ok, State} ->
			case excavator_consumer:execute(State) of
				{ok, State1} ->
					{reply, ok, dict:store(Name, State1, Dict)};
				Err ->
					{reply, Err, dict:erase(Name, Dict)}
			end;
		error ->
			{reply, bad_key, Dict}
	end;
	
handle_call({stop, Name}, _From, Dict) ->
	case dict:find(Name, Dict) of
		{ok, State} ->
			{reply, State, dict:erase(Name, Dict)};
		error ->
			{reply, not_found, Dict}
	end;
	
handle_call(_Message, _From, State) -> {reply, {error, invalid_call}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

register(Name, Instructions) when is_atom(Name), is_list(Instructions) ->
	gen_server:call(pg2:get_closest_pid(excavator_mgr_grp), {register, Name, Instructions}, infinity).
	
next(Name) when is_atom(Name) ->
	gen_server:call(pg2:get_closest_pid(excavator_mgr_grp), {load, Name}, infinity).

stop(Name) when is_atom(Name) ->
	gen_server:call(pg2:get_closest_pid(excavator_mgr_grp), {load, Name}, infinity).
