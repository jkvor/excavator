%% == TESTS ==
%% * range w/ integer and float values and default increment function
%% * range w/ non-number values and custom increment function
%% * each loop with range and list
%% * global add
main() ->
    assign(items, {range, 1, 5.5}),
    each(item, items, [
        gadd(results1, item)
    ]),
    function(fun validate_results1/1),

    assign(items, {range, "A", "E", fun inc_letter/1}),
    each(item, items, [
        gadd(results2, item)
    ]),
    function(fun validate_results2/1),
        
    assign(items, [fun() -> "unit test" end, "A", donkey, 4]),
    each(item, items, [
        gadd(results3, item)
    ]),
    function(fun validate_results3/1).
    
validate_results1(S) ->
    Results = ex_util:fetch(S, results1),
    etap:is(Results, [1.0, 2.0, 3.0, 4.0, 5.0], "range loop ran ok").

validate_results2(S) ->
    Results = ex_util:fetch(S, results2),
    etap:is(Results, ["A", "B", "C", "D", "E"], "range loop ran ok").
   
validate_results3(S) ->
    [F|Rest] = ex_util:fetch(S, results3),
    etap:ok(is_function(F) andalso F() == "unit test", "each loop ran ok"),
    etap:is(Rest, ["A", donkey, 4], "each loop ran ok").

inc_letter(Letter) -> [lists:nth(1, Letter) + 1].
