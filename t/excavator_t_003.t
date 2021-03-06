#!/usr/local/bin/escript
%% -*- erlang -*-
%%! -pa ../excavator -pa ebin -sasl errlog_type error -boot start_sasl -noshell

-include_lib("excavator/include/excavator.hrl").

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
    
    TestData = [
        {1,["returnthis",
            "jclopes",
            "skeptomai",
            "twonds",
            "jchris",
            "arjunram",
            "archaelus",
            "nitin",
            "jacobvorreuter",
            "jj1bdx",
            "mickael",
            "arnaudsj",
            "dysinger",
            "FrancescoC",
            "williamsjoe"]},
		{2,["dysinger",
			"ryankanno",
			"bascule",
			"twleung",
			"nitin",
			"pib",
			"ngerakines",
			"breakpointer",
			"janl",
			"justinsheehy",
			"moonpolysoft",
			"kevsmith"]},
		{3,["jchris",
			"seancribbs",
			"etrepum",
			"jeffreyrr",
			"janl",
			"williamsjoe",
			"markimbriaco",
			"twleung",
			"sethladd",
			"mickael",
			"mojombo",
			"dysinger",
			"boorad"]}
	],
    
    ValidateUser = fun(S) ->
        PageNum = ex_util:fetch(S, page_num),
        Username = ex_util:fetch(S, username),
		etap:ok(lists:member(Username, proplists:get_value(PageNum, TestData)), "valid user: " ++ Username)
    end,
    
    ValidateResults = fun(S) ->
        PageNum = ex_util:fetch(S, page_num),
        etap:ok(lists:member(PageNum, [1,2,3]), "results ok")
    end,
    
    ValidateOnFail = fun(S) ->
        PageNum = ex_util:fetch(S, page_num),
        etap:ok(lists:member(PageNum, [4]), "fail ok")
    end,
    
    Instrs =
        [   {instr, assign, [page_range, {range, 1, 3}]},
            {instr, each, [page_num, page_range, [
                {instr, assign, [search_result_page, #http_req{url=["http://127.0.0.1:8888/search_twitter_page", page_num, ".html"]}]},
                {instr, assert, [search_result_page, {status, 200}]},
                {instr, assert, [search_result_page, string]},
                {instr, assign, [search_results, {xpath, search_result_page, "//li[@class='result ']"}]},
                {instr, assert, [search_results, list_of_nodes]},
                {instr, each, [search_result, search_results, [
                    {instr, assign, [username, {xpath, search_result, "//div[@class='msg']/a[1]/text()"}]},
                    {instr, print, [username]},
                    {instr, assert, [username, string]},
                    {instr, assign, [msg, {xpath, search_result, "string(//span[starts-with(@class, 'msgtxt')]/*)"}]},
                    {instr, assert, [msg, string]},
                    {instr, function, [ValidateUser]}
                ]]},
                {instr, function, [ValidateResults]}
            ]]}
        ],
        
    ex_engine:run(Instrs),
    
    ok.