#!/usr/local/bin/escript
%% -*- erlang -*-
%%! -pa ebin -sasl errlog_type error -boot start_sasl -noshell

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
    test_server:start_link(),
    Instrs =
        [   {instr, configure, [qps, 10]},
            {instr, fetch, [artist_page, {get, "http://127.0.0.1:8888/gracenote_albums.html"}]},
            {instr, assert, [artist_page, {status, 200}]},
            {instr, assert, [artist_page, string]},
            {instr, assign, [albums, {xpath, artist_page, "//div[@class='album-meta-data-wrapper']"}]},
            {instr, assert, [albums, list_of_nodes]},
            {instr, each, [album, albums, [
                {instr, assign, [album_href, {xpath, album, "//a[1]/@href"}]},
                {instr, assign, [album_id, {regexp, album_href, compile_re("tui_id=(.*)tui")}]},
                {instr, assert, [album_id, string]},
                {instr, fetch, [album_page, {get, ["http://127.0.0.1:8888/gracenote_album_", album_id, ".html"]}]},
                {instr, onfail, [
                    [   {instr, assert, [album_page, {status, 200}]},
                        {instr, assert, [album_page, string]},
                        {instr, assign, [album_name_node, {xpath, album_page, "//div[@class='album-name']"}]},
                        {instr, assert, [album_name_node, node]},
                        {instr, assign, [album_name, {regexp, album_name_node, compile_re(" &gt; (.*)</div>")}]},
                        {instr, assert, [album_name, string]},
                        {instr, commit, [{album, beatles}, {album_id, album_name}]},
                        {instr, print, [album_name]}
                    ],
                    [{instr, function, [fun(S) -> io:format("This shit fAiLeD~n") end]}]
                ]}
            ]]}
        ],
        
    ex_engine:run(Instrs),
    
    ok.
    
compile_re(Regexp) ->
    {ok, RE} = re:compile(Regexp), RE.