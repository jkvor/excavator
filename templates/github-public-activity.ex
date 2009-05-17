%% ex_engine:run(ex_pp:parse("templates/github-public-activity.ex", ["jacobvorreuter"])).
main(User) ->
    assign(user, User),
    
    %% fetch user's public activity
    fetch(public_activity, {get, ["http://github.com/", user, ".atom"]}),
    assert(public_activity, {status, 200}),
    
    %% pull entry nodes
    assign(entries, {xpath, public_activity, "//entry"}),
    assert(entries, list_of_nodes),
    
    each(entry, entries, [
    
        %% pull entry_id fields
        assign(entry_id, {xpath, entry, "//id/text()"}),
        assert(entry_id, string),
        
        %% pull event_type fields
        assign(event_type, {regexp, entry_id, "tag:github.com,[0-9]{4}:(.*)/[0-9]*"}),
        assert(event_type, string),

        %% process PUSH and ISSUES events
        condition((event_type == "PushEvent") orelse (event_type == "IssuesEvent"), [
        
            %% pull published date
            assign(published, {xpath, entry, "//published/text()"}),
            assert(published, string),
            
            function(fun(S) ->
                U = ex_util:fetch_value(S, user),
                P = ex_util:fetch_value(S, published),
                E = ex_util:fetch_value(S, event_type),
                io:format("processing ~p ~p-~p~n", [U, P, E])
            end),
            
            %% commit published date and event type
            commit({user, published}, {user, published, event_type})
        ])
    ]).