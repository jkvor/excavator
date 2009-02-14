#!/usr/local/bin/escript
%% -*- erlang -*-
%%! -pa ebin -sasl errlog_type all -boot start_sasl -noshell

-import(excavator_funs, [assign/2, assert/2, commit/2, print/1, print/2, each/2]).

main(_) ->
    etap:plan(5),

	etap_exception:lives_ok(fun() ->
		etap_application:start_ok(inets, "Application 'inets' started"),
		etap_application:load_ok(excavator, "Application 'excavator' loaded"),
		etap_application:start_ok(excavator, "Application 'excavator' started"),
		ok
	end, "apps load"),

	Root_Url = "http://marketplace.xbox.com/en-US/games/catalog.aspx?d=0&r=-1&g=-1&sb=2&ot=0&mt=0&rl=0&p=~p",
	
	Instr =
		[ assign({url, "http://marketplace.xbox.com/en-US/games/catalog.aspx?d=0"}, scope),
		  assert(scope, has_text),
		
		  assign({xpath, scope, "//div[@class='XbcMktResultsSummary']/text()"}, num_results),
		  print(num_results),
		  assert(num_results, has_text),
		
		  assign({regexp, num_results, "[0-9]?-[0-9]? of ([0-9]?) results"}, num_results),
		  print(num_results),
		  assert(num_results, has_text),
		
		  %assign({range, 1, cast(num_results, integer) / 20}, page_num),
		  assign({range, 1, 20}, page_num),
		  assert(page_num, has_range)
		
		  %each(page_num, [
		%	print(page_num)
		 % ])
		],
		
	etap:is(excavator_worker:run(Instr), ok, "run instructions ok"),
	
    etap:end_tests().