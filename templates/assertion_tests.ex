%% ex_engine:run(ex_pp:parse("templates/assertion_tests.ex", [])).
%% == TESTS ==
%% * assertions
%% * onfail
main() ->
    assert("Hello World", string),
    assert([1,2,3,4,5], string),
    assert([], list_of_strings),
    assert(["Hello", "World"], list_of_strings),
    
    assert([1,a,"asdf"], list),
    
    assert({http_response, 200, [], "<html/>"}, {status, 200}),
    assert({http_response, 200, [], "<html/>"}, string),
    
    assert({<<"html">>, [], []}, node),
    assert([], list_of_nodes),
    assert([{<<"html">>, [], []}], list_of_nodes),
    
    onfail({assertion_failed,{{ack},'_',string}}, [
        gassign(result1a, true),
        assert({ack}, string),
        gassign(result1b, true)
    ], [
        gassign(result1c, true)
    ]),
    function(fun validate_result1/1),
        
    onfail({assertion_failed,{"abc",'_',string}}, [
        gassign(result2a, true),
        assert("abc", string),
        gassign(result2b, true)
    ], [
        gassign(result2c, true)
    ]),
    function(fun validate_result2/1),
        
    onfail({assertion_failed, '_'}, [
        assert(["Hello", 4, "World"], list_of_strings)
    ], [
        gassign(result3, true)
    ]),
    function(fun validate_result3/1),
        
    onfail({assertion_failed, '_'}, [
        assert([{<<"a">>,[],[]}, {a,[],[]}], list_of_nodes)
    ], [
        gassign(result4, true)
    ]),
    function(fun validate_result4/1),
        
    assign(test1, "hello"),
    assert(test1 == "hello"),
    onfail({assertion_failed, '_'}, [
        assert(test1 == "meow")
    ], [
        gassign(result5, true)
    ]),
    function(fun validate_result5/1),

    assign(my_list, ["random", "butter"]),
    assert({first, my_list} == "random"),
    onfail({assertion_failed, '_'}, [
        assert({first, my_list} == "butter")
    ], [
        gassign(result6, true)
    ]),
    function(fun validate_result6/1),

    assign(ackfoo, true),
    assert(ackfoo),
    onfail({assertion_failed, '_'}, [
        assert(ackfoo1)
    ], [
        gassign(result7, true)
    ]),
    function(fun validate_result7/1),
    
    ok.
        
validate_result1(S) ->
    etap:is(ex_util:fetch(S, result1a), true, "assertion onfail ok"),
    etap:is(ex_util:fetch(S, result1b), undefined, "assertion onfail ok"),
    etap:is(ex_util:fetch(S, result1c), true, "assertion onfail ok").

validate_result2(S) ->
    etap:is(ex_util:fetch(S, result2a), true, "assertion onfail ok"),
    etap:is(ex_util:fetch(S, result2b), true, "assertion onfail ok"),
    etap:is(ex_util:fetch(S, result2c), undefined, "assertion onfail ok").
    
validate_result3(S) ->
    etap:is(ex_util:fetch(S, result3), true, "assertion onfail ok").
    
validate_result4(S) ->
    etap:is(ex_util:fetch(S, result4), true, "assertion onfail ok").
    
validate_result5(S) ->
    etap:is(ex_util:fetch(S, result5), true, "assertion onfail ok").
    
validate_result6(S) ->
    etap:is(ex_util:fetch(S, result6), true, "assertion onfail ok").
    
validate_result7(S) ->
    etap:is(ex_util:fetch(S, result7), true, "assertion onfail ok").