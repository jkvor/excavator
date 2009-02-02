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

	Instr =
		[ assign({url, "http://www.gracenote.com/search/charts.php?chart=top10"}, scope),
		  assert(scope, has_text),
		  assign({xpath, scope, "//div[@id='main-content']/div[@class='item chart2']"}, scope),
		  assert(scope, [has_list_items, {size, 10}]),
		
		  each(scope, [
			[ assign({xpath, scope, "//div[@class='content']"}, scope),
			  assert(scope, has_node),
			  assign({xpath, scope, "//div[@class='album-title']//a/text()"}, album_title),
			  assert(album_title, has_text),
			  print(album_title)
			],
			
			[ assign({xpath, scope, "//div[@class='album-image']/a/@href"}, details_url),
			  assert(details_url, has_text),
			  assign({regexp, details_url, "tui_id=(.*)"}, album_uid),
			  assert(album_uid, has_text),
			  print(details_url),
			  print(album_uid)
			]			
		  ])
		],
		
	etap:is(excavator_worker:run(Instr), ok, "run instructions ok"),
	
    etap:end_tests().