-module(ex_consumer).
-export([
    execute/1, 
    fetch/3, 
    assign/3, 
    assert/3, 
    commit/3, 
    commit/4, 
    each_pre/4, each/4, each_post/4, 
    configure/3, 
    function/2, 
    print/2
]).

-include("excavator.hrl").

%% @spec execute(instr(), State) -> State1
execute(#state{instructions=[{instr, Function, Args}|TailInstrs]}=State) ->
    ?INFO_MSG(">> ~p~n", [Function]),    
    Pre = list_to_atom(atom_to_list(Function) ++ "_pre"),
    Post = list_to_atom(atom_to_list(Function) ++ "_post"),
    Functions = [Pre, Function, Post],
    execute({true, State}, Functions, Args, TailInstrs).
    
%% no more functions to call
execute({false, State}, [], _, TailInstrs) -> State#state{instructions=TailInstrs};

%% no more functions to call
execute({_, State}, [], _, _) -> State;

%% stop condition
execute({false, State}, _, _, _) -> State;

%% state returned instead of tuple {true|false, state()}
execute(State, Funs, Args, TIs) when is_record(State, state) -> 
    execute({true, State}, Funs, Args, TIs);
    
%% call function
execute({true, State}, [Function|Tail], Args0, TailInstrs) ->
    Args = [State|Args0],
    case erlang:function_exported(?MODULE, Function, length(Args)) of
        true -> execute(apply(?MODULE, Function, Args), Tail, Args0, TailInstrs);
        false when Tail==[] -> execute({false, State}, Tail, Args0, TailInstrs);
        false -> execute({true, State}, Tail, Args0, TailInstrs)
    end.
   
%% template functions 
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
    
assign(State, Key, Term) ->
    ?STORE(State, Key, compute(State, Term)).

assert(State, Key, Assertion) ->
    assert_true(?FETCH(State, Key), Assertion),
    State.
    
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

each_pre(#state{parent=Parent}=State, _, Source, _) ->
    case ?FETCH(State, Source) of
        {nil, []} ->
            [_ThisInstr|TailInstrs] = Parent#state.instructions,
            io:format("NIL: ~p~n", [TailInstrs]),
            {false, Parent#state{instructions=TailInstrs}};
        Other ->
            io:format("other: ~p~n", [Other]),
            {true, State}
    end.
    
each(State, Key, Source, _) ->
    SourceVals = ?FETCH(State, Source),
    store_next_value(State, Key, Source, SourceVals).
    
each_post(State, _, _, NewInstructions) ->
    {true, State#state{instructions=NewInstructions, parent=State}}.
    
% each(#state{stack=Stack}=State, Key, Source, NewInstructions) ->
%     case ?FETCH(State, Source) of
%         {range, Last, Last} when is_integer(Last) ->
%             NewState = ?STORE(State, Key, {string, integer_to_list(Last)}),
%             NewState#state{instructions=NewInstructions};
%         {range, Current, Last} when is_integer(Current), is_integer(Last) ->
%             OldState = ?STORE(State, Source, {range, Current+1, Last}),
%             NewState = ?STORE(State, Key, {string, integer_to_list(Current)}),
%             NewState#state{instructions=NewInstructions, stack=[OldState|Stack]};
%         {_Type, Val} when Val==undefined; Val==[] ->
%             exit({?MODULE, ?LINE, fetch_failed, Source, Val});
%         {Type, [Val]} -> %% last item in list
%             NewState = ?STORE(State, Key, typify_value(Type, Val)),
%             NewState#state{instructions=NewInstructions};
%         {Type, [Val|Tail]} ->
%             OldState = ?STORE(State, Source, {Type, Tail}), %% insert list tail for source key
%             NewState = ?STORE(State, Key, typify_value(Type, Val)), %% insert item from source
%             NewState#state{instructions=NewInstructions, stack=[OldState|Stack]} %% push old state on to stack of new state
%             %% next instruction processed will be first from Instructions
%     end.
    
configure(State, Key, Value) ->
    ?CONFIGURE(State, Key, Value).
    
function(State, Fun) when is_function(Fun) ->
    Fun(State),
    State.
    
print(State, Key) ->
    ?INFO_REPORT({print, ?FETCH(State, Key)}),
    State.
            
%onfail(#state{stack=Stack}=State, AttemptInstrs, _FailInstr) when is_list(AttemptInstrs) ->
%    State#state{instructions=AttemptInstrs, stack=[State|Stack]}.
    
%% internal functions
compute(State, {xpath, Source, XPath}) ->
    ex_xpath:run(XPath, ?FETCH(State, Source));
compute(State, {regexp, Source, Regexp}) ->
    ex_re:run(Regexp, ?FETCH(State, Source));
compute(_State, {range, Current, Last}) ->
    {range, Current, Last}.
    
assert_true({nil, Key}, nil) when Key==[]; Key==undefined -> ok;    
assert_true({string, Key}, string) when is_list(Key), length(Key) > 0 -> ok;
assert_true({node, Key}, node) when is_tuple(Key) -> ok;
assert_true(Key, node) when is_tuple(Key) -> ok;
assert_true({list_of_strings, Key}, list_of_strings) when is_list(Key) -> [assert_true(Item, string) || Item <- Key], ok;
assert_true({list_of_nodes, Key}, list_of_nodes) when is_list(Key) -> [assert_true(Item, node) || Item <- Key], ok;
assert_true({http_response, _S, _H, Body}, string) when is_list(Body), length(Body) > 0 -> ok;
assert_true({http_response, Status, _H, _B}, {status, Status}) -> ok;
assert_true(Key, Assertion) -> exit({?MODULE, assertion_failed, {Key, Assertion}}).

store_next_value(State, Key, Source, {range, Last, Last}) when is_integer(Last) ->
    OldState = ?STORE(State, Source, {nil, []}),
    ?STORE(OldState, Key, {string, integer_to_list(Last)});
    
store_next_value(State, Key, Source, {range, Current, Last}) when is_integer(Current), is_integer(Last) ->
    OldState = ?STORE(State, Source, {range, Current+1, Last}),
    ?STORE(OldState, Key, {string, integer_to_list(Current)});
    
store_next_value(_State, _Key, Source, {_Type, Val}) when Val==undefined; Val==[] ->
    exit({?MODULE, ?LINE, fetch_failed, Source, Val});
    
store_next_value(State, Key, _, {Type, [Val]}) -> %% last item in list   
    ?STORE(State, Key, typify_value(Type, Val));
    
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