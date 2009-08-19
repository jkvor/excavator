#!/usr/local/bin/escript
%% -*- erlang -*-
%%! -pa ../excavator -pa ebin -sasl errlog_type error -boot start_sasl -noshell

main(_) ->
    etap:plan(unknown),
    case (catch start()) of
        {'EXIT', Err} ->
            io:format("# ~p~n", [Err]),
            etap:bail();
        _ ->
            etap:end_tests()
    end,
    ok.
    
start() ->
    error_logger:tty(false),
    application:start(inets),
    application:start(excavator),
    test_server:start_link(),

    etap:is(ex_engine:run(ex_pp:parse("templates/non_instr_tests.ex")), [a,b,c], "correct response from non_instr_tests.ex"),
    etap:is(ex_engine:run(ex_pp:parse("templates/iteration_tests.ex", [])), ok, "correct response from iteration_tests.ex"),
    etap:is(ex_engine:run(ex_pp:parse("templates/assignment_tests.ex", [])), ok, "correct response from assignment_tests.ex"),
    etap:is(ex_engine:run(ex_pp:parse("templates/assertion_tests.ex", [])), ok, "correct response from assertion_tests.ex"),
    etap:is(ex_engine:run(ex_pp:parse("templates/overloaded.ex", [a])), "ABCD", "overloaded ok"),
    etap:is(ex_engine:run(ex_pp:parse("templates/overloaded.ex", [b])), "ABCE", "overloaded ok"),
    etap:is(ex_engine:run(ex_pp:parse("templates/overloaded.ex", [a, b])), "ABCF", "overloaded ok"),
    
    ok.