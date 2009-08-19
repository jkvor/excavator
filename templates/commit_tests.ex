%% ex_engine:run(ex_pp:parse("templates/commit_tests.ex", [])).
%% == TESTS ==
%% * commit
main() ->
    configure(commit_callback, {test_callback, test}),
    
    assign(var1, [1,2,3]),
    commit(test1, var1),
    
    commit(test2, regexp("a_b_c", "([a-z]+)")),
    
    commit(var1, var2, var3),
    
    ok.