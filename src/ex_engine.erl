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
        
% run(State) when is_record(State, state) ->
%     {Instr, NextState} = next_state(State),
%     NextState1 = ex_consumer:execute(Instr, NextState),
%     run(NextState1).
%     
% %% finished instructions, no parent state == DONE
% next_state(#state{instructions=[], parent=undefined}) ->
%     ok;
% 
% %% finished instructions, move up one level and process parent state
% next_state(#state{instructions=[], parent=Parent}) when is_record(Parent, state) ->
%     next_state(Parent);
%  
% %% create new state from old state with new instruction set and old state as parent
% next_state(#state{instructions=[{instr, each, [_, _, EachInstrs]}=NextInstr|_]}=S) ->
%     {NextInstr, S#state{instructions=EachInstrs, parent=S}};
%     
% %next_state(#state{instructions=[{instr, onfail, [NextInstr, FailInstrs]}|OldInstrs]}=S) ->
% %    {NextInstr, S#state{instructions=NewInstrs, parent=Parent}};
%  
% %% remove instruction from list and execute
% next_state(#state{instructions=[NextInstr|TailInstrs]}=S) ->
%     {NextInstr, S#state{instructions=TailInstrs}}.
%         
%% CASES:      
%%  fetch, assign, assert, function, commit, configuration: remove instruction from list and execute
%%  each: update source in dictionary
%%        assign current value of source to key
%%        create new state from old state with new instruction set and old state as parent
%%        execute new instruction list

%% internal functions
%keep_instruction({instr, Instr, _}) when Instr==each; Instr==onfail -> true;
%keep_instruction(_) -> false.

% %% finished running
% run(#state{instructions=[], stack=[]}) -> 
%     ok;
% 
% %% finished local instruction set, pop stack
% run(#state{instructions=[], stack=[#state{instructions=[{instr, onfail, _}|Instrs]}=S|_]}) ->
%     run(S#state{instructions=Instrs});
%     
% run(#state{instructions=[], stack=[OldState|_]}) ->
%     io:format("old state: ~p, ~p~n", [OldState#state.instructions, dict:fetch_keys(OldState#state.dictionary)]),
%     run(OldState);
% 
% run(#state{instructions=[Instr|Instrs], stack=Stack}=State) ->
%     NewState0 = 
%         case keep_instruction(Instr) of
%             true -> State#state{instructions=[Instr|Instrs]};
%             false -> State#state{instructions=Instrs}
%         end,
%     case catch ex_consumer:execute(Instr, NewState0) of
%         {'EXIT', Error} ->
%             case Stack of
%                 [#state{instructions=[{instr, onfail, [_, FailInstrs]}|InstrsTail]}=OldState|StackTail] ->
%                     NewStack = [OldState#state{instructions=InstrsTail}|StackTail],
%                     NewState1 = OldState#state{instructions=FailInstrs, stack=NewStack},
%                     run(NewState1);
%                 _ ->
%                     exit(Error) %% perhaps someone else will enjoy this
%             end;
%         NewState1 ->
%             run(NewState1)
%     end.