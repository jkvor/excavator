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
-module(ex_consumer).
-compile(export_all).

-include("excavator.hrl").

%% @spec execute(instr(), State) -> State1
execute(#state{instructions=[{instr, Function, Args}|_]}=State) ->    
    execute(State, [
		{fun print_state/1, mk_f(Function, print)},
		{fun default/1, Function},  
		{fun next_state/1, mk_f(Function, next_state)}
	], Args).
    
execute({stop, State}, _, _) -> State;

execute(State, [], _) -> State;

execute(State, [{Default, Function}|TailFunctions], Args) ->
    case proplists:get_all_values(Function, proplists:get_value(exports, ex_consumer:module_info())) of
        [] ->
            execute(Default(State), TailFunctions, Args);
        Arities ->
            case lists:member(length(Args)+1, Arities) of
                true ->
        			case catch apply(?MODULE, Function, [State|Args]) of
        				{'EXIT', Err} ->
        					execute(handle_failure(State, Err), TailFunctions, Args);
        				NewState when is_record(NewState, state) ->
        					execute(NewState, TailFunctions, Args)
        			end;
        		false ->
        		    execute(Default(State), TailFunctions, Args)
        	end
	end.
	   
%% =============================================================================
%% == Template Functions
%% =============================================================================
assign(State, Key, Term) ->
    ?STORE(State, Key, ?EXPAND(State, Term)).
    
assign_next_state(#state{instructions=[_|TailInstrs]}=S) ->
	S#state{instructions=TailInstrs, request_times=update_request_times(S)}.

assign_print(State, Key, _) ->
	?INFO_MSG(">> assign/3: ~p~n", [Key]),
	State.
	    
%% =============================================================================
gassign(State, Key, Term) ->
    ?GLOBAL_STORE(State, Key, ?EXPAND(State, Term)).

gassign_print(State, Key, _) ->
	?INFO_MSG(">> assign/3: ~p~n", [Key]),
	State.
	
%% =============================================================================
add(State, Key, Term) ->
    ?ADD(State, Key, ?EXPAND(State, Term)).
    
add_print(State, Key, _) ->
	?INFO_MSG(">> add/3: ~p~n", [Key]),
	State.

%% =============================================================================
gadd(State, Key, Term) ->
    ?GLOBAL_ADD(State, Key, ?EXPAND(State, Term)).

gadd_print(State, Key, _) ->
	?INFO_MSG(">> gadd/3: ~p~n", [Key]),
	State.

%% =============================================================================
assert(State, Key, Assertion) ->
    case assert_true(Key, ?EXPAND(State, Key), Assertion) of
        true -> ok;
        false -> exit({assertion_failed, {Key, ?EXPAND(State, Key), Assertion}})
    end,
    State.
	
assert_print(State, Key, Assertion) ->
	?INFO_MSG(">> assert/3 ~p : ~p~n", [Key, Assertion]),
	State.

%% =============================================================================
commit(State, Key, Value) ->
    case ?FETCH_CONFIG(State, commit_callback) of
        undefined ->
            commit(State, Key, Value, {ex_default_storage, store});
        {M,F} ->
            commit(State, Key, Value, {M, F})
    end.
    
commit(State, Key, Value, {CallbackModule, CallbackFunction}) ->
    Value1 = ?EXPAND(State, Value),
    case apply(CallbackModule, CallbackFunction, [Key, Value1]) of
        State1 when is_record(State1, state) ->
            State1;
        _ ->
            State
    end.

%% =============================================================================    
each(State, Key, Source, _) ->
    SourceVals = ?FETCH(State, Source),
    store_next_value(State, Key, Source, SourceVals).
    
each_next_state(#state{instructions=[_|TailInstructions]}=State, _, Source, NewInstrs) ->
	case ?FETCH(State, Source) of
		[] -> %% last Source value; remove "each" instruction from list
			Parent = State#state{instructions=TailInstructions},
			State#state{instructions=NewInstrs, parent=Parent};
		_ -> %% Source still has values for next time around
			State#state{instructions=NewInstrs, parent=State}
	end.
        
each_print(State, Key, Source, _) ->
	?INFO_MSG(">> each/4 ~p in ~p~n", [Key, Source]),
	State.

%% =============================================================================     
condition_next_state(State, Op, TrueInstrs) ->
    condition_next_state(State, Op, TrueInstrs, []).
    
condition_next_state(#state{instructions=[_|TailInstructions]}=State, {op, Op, Left, Right}, TrueInstrs, FalseInstrs) ->
    case is_condition(State, Op, ?EXPAND(State, Left), ?EXPAND(State, Right)) of
        true ->
            Parent = State#state{instructions=TailInstructions},
            State#state{instructions=TrueInstrs, parent=Parent};
        false when FalseInstrs == [] ->
            State#state{instructions=TailInstructions};
        false ->
            Parent = State#state{instructions=TailInstructions},
            State#state{instructions=FalseInstrs, parent=Parent}
    end.
    
condition_print(State, {op, Op, Left, Right}, _) ->
    ?INFO_MSG(">> condition/3 ~p ~p ~p~n", [Left, Op, Right]),
    State.
    
condition_print(State, {op, Op, Left, Right}, _, _) ->
    ?INFO_MSG(">> condition/4 ~p ~p ~p~n", [Left, Op, Right]),
    State.

%% ============================================================================= 
onfail(State, _, _, _) -> State.

onfail_next_state(#state{instructions=[_|TailInstrs]}=S, Error, AttemptInstrs, FailInstrs) ->
	Parent = S#state{instructions=TailInstrs},
	S#state{instructions=AttemptInstrs, parent=Parent, fail={Error, FailInstrs}}.

onfail_print(State, Error, _, _) ->
	?INFO_MSG(">> onfail/4 ~p~n", [Error]),
	State.
	
%% =============================================================================	
configure(State, Key, Value) ->
    ?CONFIGURE(State, Key, Value).
    
%% =============================================================================
function(State, Fun) when is_function(Fun) ->
    case Fun(State) of
        NewState when is_record(NewState, state) ->
            NewState;
        _ ->
            State
    end.

%% =============================================================================    
print(State, Key) ->
    ?INFO_REPORT({print, ?EXPAND(State, Key)}),
    State.
      	       
%% =============================================================================
%% == Internal Functions
%% =============================================================================
    
mk_f(Function, Postfix) ->
	list_to_atom(lists:concat([Function, "_", Postfix])).

print_state(#state{instructions=[{instr, Function, Args}|_]}=State) ->
	?INFO_MSG(">> ~p/~w~n", [Function, length(Args)+1]),
	State.

default(S) -> S.

next_state(#state{instructions=[_|TailInstrs]}=S) ->
	S#state{instructions=TailInstrs}.
	
handle_failure(#state{fail={Err1, FailInstrs}}=S, Err2) ->
	case compare(Err1, Err2) of
		true ->
			{stop, S#state{instructions=FailInstrs, fail=undefined}};
		false ->
			exit(Err2)
	end;

handle_failure(_, Err) -> exit(Err).

is_condition(S, Op, {op, Op1, Left, Right}, B) ->
    is_condition(S, Op, is_condition(S, Op1,?EXPAND(S, Left),?EXPAND(S, Right)), B);
is_condition(S, Op, A, {op, Op1, Left, Right}) ->
    is_condition(S, Op, A, is_condition(S, Op1,?EXPAND(S, Left),?EXPAND(S, Right)));
is_condition(_, 'orelse', A, B) when A orelse B -> true;
is_condition(_, 'andalso', A, B) when A andalso B -> true;
is_condition(_, '==', A, A) -> true;
is_condition(_, '=:=', A, A) -> true;
is_condition(_, '=/=', A, B) when A =/= B -> true;
is_condition(_, '/=', A, B) when A /= B -> true;
is_condition(_, '>', A, B) when A > B -> true;
is_condition(_, '>=', A, B) when A >= B -> true;
is_condition(_, '<', A, B) when A < B -> true;
is_condition(_, '=<', A, B) when A =< B -> true;
is_condition(_, _, _, _) -> false.

compare(Item, Item) -> true;

compare(Item1, Item2) when is_tuple(Item1), is_tuple(Item2) ->
	compare(tuple_to_list(Item1), tuple_to_list(Item2));
	
compare(Item1, Item2) when is_list(Item1), is_list(Item2) ->
	if
		length(Item1) == length(Item2) ->
			lists:foldl(
				fun (_, false) -> false;
					({'_', _}, _) -> true;
					({A, B}, _) -> compare(A,B)
				end, true, lists:zip(Item1, Item2));
		true -> false
	end;
	
compare(_, _) -> false.
    
assert_true(_K, V, string) when is_record(V, http_response) ->
    case V#http_response.body of
        String when is_list(String), length(String) > 0 -> true;
        _ -> false
    end;
assert_true(_K, V, {status, Status}) when is_record(V, http_response) ->
    V#http_response.status == Status;
assert_true(_K, V, list_of_strings) when is_list(V) ->
    case lists:usort([ex_util:typeof(I) || I <- V]) of
        [string] -> true;
        _ -> false
    end;
assert_true(_K, V, list_of_nodes) when is_list(V) ->
    case lists:usort([ex_util:typeof(I) || I <- V]) of
        [node] -> true;
        _ -> false
    end;
assert_true(_K, V, Assertion) ->
    ex_util:typeof(V) == Assertion.

store_next_value(State, Key, Source, {range, Last, Last}) when is_integer(Last) ->
    OldState = ?STORE(State, Source, []),
    ?STORE(OldState, Key, integer_to_list(Last));
    
store_next_value(State, Key, Source, {range, Current, Last}) when is_integer(Current), is_integer(Last) ->
    OldState = ?STORE(State, Source, {range, Current+1, Last}),
    ?STORE(OldState, Key, integer_to_list(Current));
    
store_next_value(_State, _Key, Source, Val) when Val==undefined; Val==[] ->
    exit({?MODULE, ?LINE, fetch_failed, Source, Val});
    
store_next_value(State, Key, Source, [Val]) -> %% last item in list   
	OldState = ?STORE(State, Source, []),
    ?STORE(OldState, Key, Val);
    
store_next_value(State, Key, Source, [Val|Tail]) ->
    OldState = ?STORE(State, Source, Tail), %% insert list tail for source key
    ?STORE(OldState, Key, Val). %% insert item from source
                
update_request_times(#state{request_times=Times}) ->
    Secs = ex_util:seconds(),
    [Secs|lists:filter(
        fun(S) ->
            S >= Secs-1
        end, Times)].