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
	case proplists:get_value(Function, proplists:get_value(exports, ex_consumer:module_info())) of
		Len when Len == length(Args)+1 ->
			execute(apply(?MODULE, Function, [State|Args]), TailFunctions, Args);
		_ ->
			execute(Default(State), TailFunctions, Args)
	end.
	   
%% =============================================================================
%% == Template Functions
%% =============================================================================
fetch(State, Key, {Method, Url}) when Method==options;Method==get;Method==head;Method==delete;Method==trace ->
    fetch(State, Key, {Method, Url, [], []});
fetch(State, Key, {Method, Url, Headers}) when Method==options;Method==get;Method==head;Method==delete;Method==trace ->
    fetch(State, Key, {Method, Url, Headers, []});
fetch(State, Key, {Method, Url, Headers, Body}) ->
    Url1 = lists:flatten([begin
        case I of
            String when is_list(String) -> String; 
            Atom when is_atom(Atom) -> to_string(?FETCH(State, Atom));
            Other -> Other 
        end
    end || I <- Url]),
    %?INFO_MSG("fetching ~p~n", [Url1]),
    Response = ex_web:request(Method, Url1, Headers, Body),
    %?INFO_MSG("response: ~p~n", [Response]),
    ?STORE(State, Key, Response).

fetch_print(State, _Key, Request) ->
	?INFO_MSG(">> fetch/3 ~p~n", [Request]),
	State.
    
%% =============================================================================
assign(State, Key, Term) ->
    ?STORE(State, Key, compute(State, Term)).

assign_print(State, Key, _) ->
	?INFO_MSG(">> assign/3: ~p~n", [Key]),
	State.

%% =============================================================================
assert(State, Key, Assertion) ->
    assert_true(?FETCH(State, Key), Assertion),
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
    Value1 = ?EVALUATE(State, Value),
    spawn(CallbackModule, CallbackFunction, [Key, Value1]),
    State.

%% =============================================================================    
each(State, Key, Source, _) ->
    SourceVals = ?FETCH(State, Source),
    store_next_value(State, Key, Source, SourceVals).
    
each_next_state(#state{instructions=[_|TailInstructions]}=State, _, Source, NewInstrs) ->
	case ?FETCH(State, Source) of
		{_, []} -> %% last Source value; remove "each" instruction from list
			Parent = State#state{instructions=TailInstructions},
			State#state{instructions=NewInstrs, parent=Parent};
		_ -> %% Source still has values for next time around
			State#state{instructions=NewInstrs, parent=State}
	end.
        
each_print(State, Key, Source, _) ->
	?INFO_MSG(">> each/4 ~p in ~p~n", [Key, Source]),
	State.

%% =============================================================================	
configure(State, Key, Value) ->
    ?CONFIGURE(State, Key, Value).
    
%% =============================================================================
function(State, Fun) when is_function(Fun) ->
    Fun(State),
    State.

%% =============================================================================    
print(State, Key) ->
    ?INFO_REPORT({print, ?FETCH(State, Key)}),
    State.
            
%onfail(#state{stack=Stack}=State, AttemptInstrs, _FailInstr) when is_list(AttemptInstrs) ->
%    State#state{instructions=AttemptInstrs, stack=[State|Stack]}.
    
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
	
compute(State, {xpath, Source, XPath}) ->
    ex_xpath:run(XPath, ?FETCH(State, Source));
compute(State, {regexp, Source, Regexp}) ->
    ex_re:run(Regexp, ?FETCH(State, Source));
compute(_State, {range, Current, Last}) ->
    {range, Current, Last}.
    
assert_true({nil, Key}, nil) when Key==[]; Key==undefined -> ok;    
assert_true({string, Key}, string) when is_list(Key), length(Key) > 0 -> ok;
assert_true({node, Key}, node) when is_tuple(Key) -> ok;
assert_true({list_of_strings, Key}, list_of_strings) when is_list(Key) -> [assert_true(Item, string) || Item <- Key], ok;
assert_true({list_of_nodes, Key}, list_of_nodes) when is_list(Key) -> [assert_true(Item, node) || Item <- Key], ok;
assert_true({http_response, _S, _H, Body}, string) when is_list(Body), length(Body) > 0 -> ok;
assert_true({http_response, Status, _H, _B}, {status, Status}) -> ok;
assert_true({mixed, List}, mixed) when is_list(List) -> ok;
assert_true(Key, Assertion) -> exit({?MODULE, assertion_failed, {Key, Assertion}}).

store_next_value(State, Key, Source, {range, Last, Last}) when is_integer(Last) ->
    OldState = ?STORE(State, Source, {nil, []}),
    ?STORE(OldState, Key, {string, integer_to_list(Last)});
    
store_next_value(State, Key, Source, {range, Current, Last}) when is_integer(Current), is_integer(Last) ->
    OldState = ?STORE(State, Source, {range, Current+1, Last}),
    ?STORE(OldState, Key, {string, integer_to_list(Current)});
    
store_next_value(_State, _Key, Source, {_Type, Val}) when Val==undefined; Val==[] ->
    exit({?MODULE, ?LINE, fetch_failed, Source, Val});
    
store_next_value(State, Key, Source, {Type, [Val]}) -> %% last item in list   
	OldState = ?STORE(State, Source, {Type, []}),
    ?STORE(OldState, Key, typify_value(Type, Val));
    
store_next_value(State, Key, Source, {Type, [Val|Tail]}) ->
    OldState = ?STORE(State, Source, {Type, Tail}), %% insert list tail for source key
    ?STORE(OldState, Key, typify_value(Type, Val)). %% insert item from source
            
typify_value(list_of_strings, {string, String}) ->
    {string, String};
typify_value(list_of_strings, String) when is_list(String) ->
    {string, String};
typify_value(list_of_nodes, {node, Node}) ->
    {node, Node};
typify_value(list_of_nodes, Node) ->
    {node, Node}.
    
to_string(List) when is_list(List) -> List;
to_string({string, String}) -> String;
to_string({node, Node}) -> to_string(ex_xpath:reassemble({node, Node})).