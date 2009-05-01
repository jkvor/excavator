-module(ex_consumer).
-export([execute/2, fetch/3, assign/3, assert/3, commit/3, each/4, configure/3]).

-include("excavator.hrl").

%% @spec execute(instr(), State) -> State1
execute({instr, Function, Args}, State) ->
    apply(?MODULE, Function, [State|Args]).
   
%% template functions 
fetch(State, Key, {Method, Url}) when Method==options;Method==get;Method==head;Method==delete;Method==trace ->
    fetch(State, Key, {Method, Url, [], []});
fetch(State, Key, {Method, Url, Headers}) when Method==options;Method==get;Method==head;Method==delete;Method==trace ->
    fetch(State, Key, {Method, Url, Headers, []});
fetch(State, Key, {Method, Url, Headers, Body}) ->
    Response = ex_web:request(Method, Url, Headers, Body),
    ?STORE(State, Key, Response).
    
assign(State, Key, Term) ->
    ?STORE(State, Key, evaluate(State, Term)).

assert(State, Key, Assertion) ->
    assert_true(?FETCH(State, Key), Assertion),
    State.
    
commit(State, _Key, _Value) ->
    %% commit Key/Value to CouchDB or some disk-based key/value store
    State.

each(#state{stack=Stack, instructions=OldInstructions}=State, Key, Source, NewInstructions) ->
    case ?FETCH(Source) of
        Val when Val==undefined; Val==[] ->
            exit({?MODULE, ?LINE, fetch_failed, Source, Val});
        [Val] -> %% last item in each list
            NewState = ?STORE(State, Key, Val),
            NewState#state{instructions=NewInstructions};
        [Val|Tail] ->
            OldState0 = ?STORE(State, Source, Tail), %% insert list tail for source key
            %% add this instruction to head of list so that when stack is popped 
            %% and this state begins processing again, the each list is the next instruction still
            OldState1 = OldState0#state{instructions=[{instr, each, [Key, Source, NewInstructions]}|OldInstructions]},
            NewState = ?STORE(State, Key, Val),
            NewState#state{instructions=NewInstructions, stack=[OldState1|Stack]} %% push old state on to stack of new state
            %% next instruction processed will be first from Instructions
    end.
    
configure(State, Key, Value) ->
    ?CONFIGURE(State, Key, Value).
    
%% internal functions
evaluate(State, {xpath, Source, XPath}) ->
    ex_xpath:run(XPath, ?FETCH(State, Source));
evaluate(State, {regexp, Source, Regexp}) ->
    ex_re:run(Regexp, ?FETCH(State, Source)).
    
assert_true({nil, Key}, nil) when Key==[]; Key==undefined -> ok;    
assert_true({string, Key}, string) when is_list(Key) -> ok;
assert_true({node, Key}, node) when is_tuple(Key) -> ok;
assert_true({list_of_strings, Key}, list_of_strings) when is_list(Key) -> [assert_true(Item, string) || Item <- Key], ok;
assert_true({list_of_nodes, Key}, list_of_nodes) when is_list(Key) -> [assert_true(Item, node) || Item <- Key], ok;
assert_true(Key, Assertion) -> exit({?MODULE, assertion_failed, {Key, Assertion}}).


