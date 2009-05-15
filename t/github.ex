main(User) ->
    assign(user, {string, User}),
    fetch(public_activity, {get, ["http://127.0.0.1:8888/github-", user, ".xml"]}),
    assert(public_activity, {status, 200}),
    assign(entries, {xpath, public_activity, "//entry"}),
    assert(entries, list_of_nodes),
    each(entry, entries, [
        assign(entry_id, {xpath, entry, "//id/text()"}),
        assert(entry_id, string),
        assign(event_type, {regexp, entry_id, "tag:github.com,[0-9]{4}:(.*)/[0-9]*"}),
        assert(event_type, string),
        condition((event_type == "PushEvent") orelse (event_type == "IssuesEvent"), [
            assign(published, {xpath, entry, "//published/text()"}),
            assert(published, string),
            
            assign(title, {xpath, entry, "//title/text()"}),
            assert(title, string),
            
            commit({user, published}, {user, published, event_type}),
            
            assign(author, {xpath, entry, "//author/name/text()"}),
            
            function(fun validate_supported_event_types/1)
        ], [
            function(fun validate_unsupported_event_types/1)
        ])
    ]).
    
validate_supported_event_types(S) ->
    {string, ET} = ex_util:fetch(S, event_type),
    {string, Auth} = ex_util:fetch(S, author),
    etap:ok(ET == "PushEvent" orelse ET == "IssuesEvent", "event type matches"),
    etap:is(Auth, "JacobVorreuter", "author matches").

validate_unsupported_event_types(S) -> 
    {string, ET} = ex_util:fetch(S, event_type), 
    etap:ok(ET =/= "PushEvent" andalso ET =/= "IssuesEvent", "unsupported event type matches").