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
-module(ex_scheduler).
-behaviour(gen_server).
%% @todo instruction state should be able to run across nodes in a distributed fashion

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).
   
%% api callbacks
-export([start_link/0, add/1, info/1, remove/1]).

-include("excavator.hrl").

%%====================================================================
%% api callbacks
%%====================================================================

%% @spec start_link() -> {ok, pid()}
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec add(Filename | Instrs) -> pid()
%%       Filename = string()
%%       Instrs = list()
%% @doc add a set of instructions to the scheduler and get back 
%% the pid of the spawned scheduler process
add([H|_]=Instrs) when is_tuple(H) ->
    State = #state{instructions=Instrs},
    gen_server:call(?MODULE, {add, State, undefined}, infinity);
    
add(Filename) when is_list(Filename) -> 
    RefreshStateFun = fun() -> #state{instructions=ex_pp:parse(Filename)} end,
    gen_server:call(?MODULE, {add, RefreshStateFun(), RefreshStateFun}, infinity).
    
info(Pid) when is_pid(Pid) ->
    Pid ! {self(), info},
    receive 
        {_Pid, State} -> State 
    after 1000 -> undefined 
    end.
    
remove(Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {remove, Pid}, infinity).
    
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, []}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({add, State, RefreshStateFun}, _From, Pids) ->
    Pid = spawn_link(fun() -> loop(State, RefreshStateFun) end),
    {reply, Pid, [Pid|Pids]};

handle_call({remove, Pid}, _From, Pids) ->
    Pid ! {self(), terminate},
    receive {_, ok} -> ok after 1000 -> exit(Pid, timeout) end,
    Pids1 = lists:delete(Pid, Pids),
    {reply, ok, Pids1};
    
handle_call(Other, _From, State) ->
    error_logger:error_report({?MODULE, ?LINE, unexpected_message, Other}),
    {reply, ok, State}.
 
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.
 
%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.
 
%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, Pids) ->
    [Pid ! {self(), terminate} || Pid <- Pids],
    ok.
%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

next(#state{instructions=[{instr, assign, Http}|_], request_times=[Last|_]=Times}=State, RefreshStateFun) when is_tuple(Http), element(1, Http) == http ->
    case ex_util:fetch_config(State, qps) of
        undefined -> 
            ok;
        QPS when is_integer(QPS) ->
            Now = ex_util:seconds(),
            case (length(Times) >= QPS andalso Last >= Now - 1) of
                true ->
                    timer:sleep(trunc(1000 * (Now - Last)));
                false ->
                    ok
            end
    end,
    run(State, RefreshStateFun);
    
next(State, RefreshStateFun) ->
    run(State, RefreshStateFun).
    
run(State, RefreshStateFun) ->
    case next_state(ex_consumer:execute(State)) of
        done ->
            RefreshStateFun();
        State1 ->
            State1
    end.
    
next_state(#state{instructions=[], parent=undefined}) ->
    done;
next_state(#state{instructions=[], parent=Parent}) when is_record(Parent, state) ->
    next_state(Parent);
next_state(State) when is_record(State, state) ->
    State.
	
loop(State, RefreshStateFun) ->
    receive
        {From, info} ->
            From ! {self(), State},
            loop(State, RefreshStateFun);
        {From, terminate} ->
            From ! {self(), ok}
    after 0 ->
        loop(next(State, RefreshStateFun), RefreshStateFun)
    end.
