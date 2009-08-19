%% ex_engine:run(ex_pp:parse("templates/assignment_tests.ex", [])).
%% == TESTS ==
%% * loop drawing from in-place value
%% * loop drawing from in-place range
%% * loop source mutability
%% * local and global assignment
%% * first, last commands
%% * conditions
main() ->
    configure(lame, 10),
    configure(qps, 11),
    configure(commit_callback, {my_mod, my_func}),
    function(fun validate_conf/1),
    
    each(item, [1,2,3], [
        gadd(results1, item)
    ]),
    function(fun validate_results1/1),
    
    each(item, range(1, 3), [
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
            
    each(item, regexp("a_b_c_d", "([a-z]+)"), [
        gadd(results4, item)
    ]),
    function(fun validate_results4/1),
        
    each(item, xpath("<farm><turkey name=\"glen\" /><turkey name=\"gladis\" /></farm>", "//turkey/@name"), [
        gadd(results5, item)
    ]),
    function(fun validate_results5/1),
        
    condition(first(results5) == "glen" andalso last(results5) == "gladis", [
        gassign(result6a, true),
        assign(result6b, true)
    ]),
    function(fun validate_results6/1),
      
    assign({result7a, result7b}, {"Hello", "World"}),
    assign([result7c, {result7d, result7e}], ["My", {"Name", "Is"}]),
    function(fun validate_results7/1),
                  
    ok.
 
validate_conf(S) ->
    etap:is(ex_util:fetch_config(S, lame), 10, "unknown config value"),
    etap:is(ex_util:fetch_config(S, qps), 11, "qps config value"),
    etap:is(ex_util:fetch_config(S, commit_callback), {my_mod, my_func}, "commit_callback config value").
       
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
    
validate_results6(S) ->
    etap:is(ex_util:fetch(S, result6a), true, "condition assignment ok"),
    etap:is(ex_util:fetch(S, result6b), undefined, "condition assignment ok").

validate_results7(S) ->
    etap:is(ex_util:fetch(S, result7a), "Hello", "complex term assignment ok"),
    etap:is(ex_util:fetch(S, result7b), "World", "complex term assignment ok"),
    etap:is(ex_util:fetch(S, result7c), "My", "complex term assignment ok"),
    etap:is(ex_util:fetch(S, result7d), "Name", "complex term assignment ok"),
    etap:is(ex_util:fetch(S, result7e), "Is", "complex term assignment ok").
    