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
    test_server:start_link(),

    GoodAlbumIDs = ["b917d0542e3ab9a7", "2021ab38530960de", "4177542aee34ad84"],
    GoodAlbumNames = ["The Beatles (White Album) [Disc 1]", "The Beatles (White Album) [Disc 2]", "With The Beatles"],
    BadAlbumIDs  = ["9246d2023c3bee56"],
    
    ValidateAlbumID = fun(S) ->
        {string, AID} = ex_util:fetch(S, album_id),
        {http_response, Status, _, _} = ex_util:fetch(S, album_page),
        Result =
            case [lists:member(AID, GoodAlbumIDs), lists:member(AID, BadAlbumIDs)] of
                [true, false] ->
                    Status == 200;
                [false, true] ->
                    Status == 404
            end,
        etap:ok(Result, "album_id ok")
    end,
    
    ValidateOnFail = fun(S) ->
        {string, AID} = ex_util:fetch(S, album_id),
        etap:ok(not lists:member(AID, GoodAlbumIDs) andalso lists:member(AID, BadAlbumIDs), "failed ok")
    end,
    
    ValidateCommitData = fun(S) ->
        Commit = ex_util:evaluate(S, {album_id, album_name}),
        etap:ok(lists:member(Commit, lists:zip(GoodAlbumIDs, GoodAlbumNames)), "commit data ok")
    end,
        
    Instrs =
        [   {instr, configure, [qps, 10]}, %% not implemented
            {instr, configure, [commit_callback, {ex_default_storage, store}]}, %% not implemented
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
                {instr, function, [ValidateAlbumID]},
                {instr, onfail, [
					{assertion_failed, {album_page, '_', {status, 200}}},
                    [   {instr, assert, [album_page, {status, 200}]},
                        {instr, assert, [album_page, string]},
                        {instr, assign, [album_name_node, {xpath, album_page, "//div[@class='album-name']"}]},
                        {instr, assert, [album_name_node, node]},
                        {instr, assign, [album_name, {regexp, album_name_node, compile_re(" &gt; (.*)</div>")}]},
                        {instr, assert, [album_name, string]},
                        {instr, commit, [{album, beatles}, {album_id, album_name}]},
                        {instr, function, [ValidateCommitData]}
                    ],
                    [{instr, function, [ValidateOnFail]}]
                ]}
            ]]}
        ],
        
    ex_engine:run(Instrs),
    
    ok.
    
compile_re(Regexp) ->
    {ok, RE} = re:compile(Regexp), RE.