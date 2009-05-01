-module(ex_engine).
-export([run/1]).

-include("excavator.hrl").

%% when a list of instructions is passed in, create a 
%% new state and begin processing
run(Instructions) when is_list(Instructions) ->
    run(#state{instructions=Instructions});
    
%% finished running
run(#state{instructions=[], stack=[]}) -> 
    ok;

%% finished local instruction set, pop stack
run(#state{instructions=[], stack=[#state{instructions=[{instr, onfail, _}|Instrs]}=S|_]}) ->
    run(S#state{instructions=Instrs});
    
run(#state{instructions=[], stack=[OldState|_]}) ->
    run(OldState);

run(#state{instructions=[Instr|Instrs], stack=Stack}=State) ->
    NewState0 = 
        case keep_instruction(Instr) of
            true -> State#state{instructions=[Instr|Instrs]};
            false -> State#state{instructions=Instrs}
        end,
    case catch ex_consumer:execute(Instr, NewState0) of
        {'EXIT', Error} ->
            case Stack of
                [#state{instructions=[{instr, onfail, [_, FailInstrs]}|InstrsTail]}=OldState|StackTail] ->
                    NewStack = [OldState#state{instructions=InstrsTail}|StackTail],
                    NewState1 = #state{instructions=FailInstrs, stack=NewStack},
                    run(NewState1);
                _ ->
                    exit(Error) %% perhaps someone else will enjoy this
            end;
        NewState1 ->
            run(NewState1)
    end.
    
%% internal functions
keep_instruction({instr, Instr, _}) when Instr==each; Instr==onfail -> true;
keep_instruction(_) -> false.