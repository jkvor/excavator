#!/usr/local/bin/escript
%% -*- erlang -*-
%%! -pa ebin -sasl errlog_type error -boot start_sasl -noshell

main(_) ->
    etap:plan(6),

	etap_exception:lives_ok(fun() ->
		etap_application:start_ok(inets, "Application 'inets' started"),
		etap_application:load_ok(excavator, "Application 'excavator' loaded"),
		etap_application:start_ok(excavator, "Application 'excavator' started"),
		ok
	end, "apps load"),
	
	etap:is(excavator_worker:run(gracenote_test_001), ok, "run 'gracenote_test_001' ok"),
	
	etap:is(excavator_worker:run(gracenote_test_002), ok, "run 'gracenote_test_002' ok"),
	
    etap:end_tests().
