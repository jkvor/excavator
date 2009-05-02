-module(ex_consumer).
-export([execute/2, fetch/3, assign/3, assert/3, commit/3, commit/4, each/4, configure/3, function/2, print/2, onfail/3]).

-include("excavator.hrl").

%% @spec execute(instr(), State) -> State1
execute({instr, Function, Args}, State) ->
    ?INFO_MSG("~p(~p)~n", [Function, [state|Args]]),
    apply(?MODULE, Function, [State|Args]).
   
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
    Response = ex_web:request(Method, Url1, Headers, Body),
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

each(#state{stack=Stack}=State, Key, Source, NewInstructions) ->
    case ?FETCH(State, Source) of
        {_Type, Val} when Val==undefined; Val==[] ->
            exit({?MODULE, ?LINE, fetch_failed, Source, Val});
        {Type, [Val]} -> %% last item in list
            NewState = ?STORE(State, Key, typify_value(Type, Val)),
            NewState#state{instructions=NewInstructions};
        {Type, [Val|Tail]} ->
            OldState = ?STORE(State, Source, {Type, Tail}), %% insert list tail for source key
            NewState = ?STORE(State, Key, typify_value(Type, Val)), %% insert item from source
            NewState#state{instructions=NewInstructions, stack=[OldState|Stack]} %% push old state on to stack of new state
            %% next instruction processed will be first from Instructions
    end.
    
configure(State, Key, Value) ->
    ?CONFIGURE(State, Key, Value).
    
function(State, Fun) when is_function(Fun) ->
    Fun(State),
    State.
    
print(State, Key) ->
    error_logger:info_report({print, ?FETCH(State, Key)}),
    State.
        
onfail(#state{stack=Stack}=State, AttemptInstrs, _FailInstr) when is_list(AttemptInstrs) ->
    State#state{instructions=AttemptInstrs, stack=[State|Stack]}.
    
%% internal functions
compute(State, {xpath, Source, XPath}) ->
    ex_xpath:run(XPath, ?FETCH(State, Source));
compute(State, {regexp, Source, Regexp}) ->
    ex_re:run(Regexp, ?FETCH(State, Source)).
    
assert_true({nil, Key}, nil) when Key==[]; Key==undefined -> ok;    
assert_true({string, Key}, string) when is_list(Key), length(Key) > 0 -> ok;
assert_true({node, Key}, node) when is_tuple(Key) -> ok;
assert_true(Key, node) when is_tuple(Key) -> ok;
assert_true({list_of_strings, Key}, list_of_strings) when is_list(Key) -> [assert_true(Item, string) || Item <- Key], ok;
assert_true({list_of_nodes, Key}, list_of_nodes) when is_list(Key) -> [assert_true(Item, node) || Item <- Key], ok;
assert_true({http_response, _S, _H, Body}, string) when is_list(Body), length(Body) > 0 -> ok;
assert_true({http_response, Status, _H, _B}, {status, Status}) -> ok;
assert_true(Key, Assertion) -> exit({?MODULE, assertion_failed, {Key, Assertion}}).

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