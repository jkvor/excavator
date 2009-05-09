-module(ex_engine).
-export([run/1]).

-include("excavator.hrl").

%% when a list of instructions is passed in, create a 
%% new state and begin processing
run(Instructions) when is_list(Instructions) ->
    run(#state{instructions=Instructions});
    
run(#state{instructions=[], parent=undefined}) ->
    ok;

run(#state{instructions=[], parent=Parent}) when is_record(Parent, state) ->
    run(Parent);
    
run(State) when is_record(State, state) ->
    run(ex_consumer:execute(State)).