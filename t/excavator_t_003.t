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

    Instrs = ex_pp:parse("templates/github.ex", [["jacobvorreuter", "ngerakines"]]),
    ex_engine:run(Instrs),
    ok.