%% ex_engine:run(ex_pp:parse("templates/github-public-activity.ex", ["jacobvorreuter"])).
main(Username) ->
    configure(commit_callback, {io, format}),

    %% fetch user's public activity
    assign(public_activity, {http, get, ["http://github.com/", Username, ".atom"]}),
    assert(public_activity, {status, 200}),

    each(entry, {xpath, public_activity, "//entry"}, [

        %% pull entry_id fields
        assign(entry_id, {xpath, entry, "//id/text()"}),
        assert(entry_id, string),
    
        %% pull event_type fields
        assign(event_type, {regexp, entry_id, "tag:github.com,[0-9]{4}:(.*)/[0-9]*"}),
        assert(event_type, string),

        %% process PUSH and ISSUES events
        condition(event_type == "PushEvent", [
    
            %% pull published date
            assign(published, {xpath, entry, "//published/text()"}),
            assert(published, string),
                
            %% call commit function
            commit("committing for ~s: ~s, ~s~n", [Username, event_type, published])
        ])
    ]).