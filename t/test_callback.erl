-module(test_callback).
-compile(export_all).

test(test1, Result) ->
    etap:is(Result, [1,2,3], "commit ok");
    
test(test2, Result) ->
    etap:is(Result, ["a", "b", "c"], "commit ok").
    
test(A, B, C) ->
    etap:is({A,B,C}, {[1,2,3], var2, var3}, "commit ok").