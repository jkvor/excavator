%% ex_engine:run(ex_pp:parse("templates/assignment_tests.ex", [])).
%% == TESTS ==
%% * loop drawing from in-place value
%% * loop drawing from in-place range
%% * loop source mutability
main() ->
    each(item, [1,2,3], [
        gadd(results1, item)
    ]),
    function(fun validate_results1/1),
    
    each(item, {range, 1, 3}, [
        gadd(results2, item)
    ]),
    function(fun validate_results2/1),

    assign(items, [4,5,6]),
    
    each(item, items, [
        gadd(results3, item)
    ]),
    function(fun validate_results3a/1),
    
    each(item, items, [
        gadd(results3, item)
    ]),
    function(fun validate_results3b/1),
        
    function(fun validate_original/1),
            
    each(item, {regexp, "a_b_c_d", "([a-z]+)"}, [
        gadd(results4, item)
    ]),
    function(fun validate_results4/1),
        
    each(item, {xpath, "<farm><turkey name=\"glen\" /><turkey name=\"gladis\" /></farm>", "//turkey/@name"}, [
        gadd(results5, item)
    ]),
    function(fun validate_results5/1).
        
validate_results1(S) ->
    Results = ex_util:fetch(S, results1),
    etap:is(Results, [1,2,3], "results match for each loop").
    
validate_results2(S) ->
    Results = ex_util:fetch(S, results2),
    etap:is(Results, [1,2,3], "results match for each loop").
    
validate_results3a(S) ->
    Results = ex_util:fetch(S, results3),
    etap:is(Results, [4,5,6], "results match for each loop").
    
validate_results3b(S) ->
    Results = ex_util:fetch(S, results3),
    etap:is(Results, [4,5,6,4,5,6], "results match for each loop").
    
validate_original(S) ->
    etap:is(ex_util:fetch(S, items), [4,5,6], "original items list matches").

validate_results4(S) ->
    Results = ex_util:fetch(S, results4),
    etap:is(Results, ["a", "b", "c", "d"], "results match for each loop").
    
validate_results5(S) ->
    Results = ex_util:fetch(S, results5),
    etap:is(Results, ["glen", "gladis"], "results match for each loop").