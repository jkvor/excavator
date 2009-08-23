#!/usr/local/bin/escript
%% -*- erlang -*-
%%! -pa ../excavator ./ebin -boot start_sasl -noshell

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
    application:start(inets),
    application:start(excavator),

    Instrs = ex_pp:parse("templates/file_tests.ex", ["t/character.xml"]),
    io:format("Instrs ~p~n", [Instrs]),
    Var = ex_engine:run(Instrs),    
    io:format("Results ~p~n", [Var]),
    
    ok.