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
    %error_logger:tty(false),
    application:start(inets),
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
            "williamsjoe"
    ]}],
    
    ValidateResult = fun(S) ->
        {string, PageNum} = ex_util:fetch(S, page_num),
        {string, Username} = ex_util:fetch(S, username),
        etap:ok(lists:member(Username, proplists:get_value(list_to_integer(PageNum), TestData)), "valid user")
    end,
    
    ValidateResults = fun(S) ->
        {string, PageNum} = ex_util:fetch(S, page_num),
        etap:ok(lists:member(list_to_integer(PageNum), [1,2,3]), "results ok")
    end,
    
    ValidateOnFail = fun(S) ->
        {string, PageNum} = ex_util:fetch(S, page_num),
        etap:ok(lists:member(list_to_integer(PageNum), [4]), "fail ok")
    end,
    
    Instrs =
        [
            {instr, assign, [page_range, {range, 1, 4}]},
            {instr, each, [page_num, page_range, [
                {instr, function, [fun(_) -> io:format("here~n") end]}
            ]]}
        ],
        % [   {instr, assign, [page_range, {range, 1, 1}]},
        %     {instr, each, [page_num, page_range, [
        %         {instr, fetch, [search_result_page, {get, ["http://127.0.0.1:8888/search_twitter_page", page_num, ".html"]}]},
        %         {instr, assert, [search_result_page, {status, 200}]},
        %         {instr, assert, [search_result_page, string]},
        %         {instr, assign, [search_results, {xpath, search_result_page, "//li[@class='result ']"}]},
        %         {instr, assert, [search_results, list_of_nodes]},
        %         {instr, each, [search_result, search_results, [
        %             {instr, assign, [msg, {xpath, search_result, "//span[@class='msgtxt en']/text()"}]},
        %             {instr, assert, [msg, string]},
        %             {instr, assign, [username, {xpath, search_result, "//div[@class='msg']/a[1]/text()"}]},
        %             {instr, assert, [username, string]},
        %             {instr, function, [ValidateResult]}
        %         ]]},
        %         {instr, function, [ValidateResults]}
        %     ]]}
        % ],
        
    ex_engine:run(Instrs),
    
    ok.
    
compile_re(Regexp) ->
    {ok, RE} = re:compile(Regexp), RE.