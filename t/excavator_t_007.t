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

	Pid1 = test_callback:start_recv(),
    Pid2 = ex_scheduler:add("templates/scheduler_test.ex", [Pid1]),

	State = test_callback:get_state(Pid1),
	io:format("state: ~p~n", [State]),
	
    ok.