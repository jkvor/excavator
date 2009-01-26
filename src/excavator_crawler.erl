-module(excavator_crawler).

-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3, load/2, load/1]).

start_link(_) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
	ok = pg2:create(excavator_crawler_grp),
	ok = pg2:join(excavator_crawler_grp, self()),
    {ok, dict:new()}.

handle_call({load, Module, Instructions}, _From, Pids) ->
	Pid = spawn(fun() -> excavator_worker:run(Instructions) end),
	{reply, ok, dict:store(Module, Pid, Pids)};
	
handle_call(_, _From, State) -> {reply, {error, invalid_call}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

load(Module, Instructions) when is_atom(Module) ->
	gen_server:call(pg2:get_closest_pid(excavator_crawler_grp), {load, Module, Instructions}, infinity).
	
load(Module) when is_atom(Module) ->
	load(Module, apply(Module, instructions, [])).