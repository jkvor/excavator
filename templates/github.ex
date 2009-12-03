main(Users) ->
    each(user, Users, [
        %% fetch user's public activity
        assign(public_activity, http(["http://127.0.0.1:8888/github-", user, ".xml"])),
        assert(public_activity, {status, 200}),
        
        %% pull entry nodes
        assign(entries, xpath(public_activity, "//entry")),
        assert(entries, list_of_nodes),
        
        each(entry, entries, [
        
            %% pull entry_id fields
            assign(entry_id, xpath(entry, "//id/text()")),
            assert(entry_id, string),
            
            %% pull event_type fields
            assign(event_type, regexp(entry_id, "tag:github.com,[0-9]{4}:(.*)/[0-9]*")),
            assert(event_type, string),
            
            %% process PUSH and ISSUES events
            condition((event_type == "PushEvent") orelse (event_type == "IssuesEvent"), [
            
                %% pull published date
                assign(published, xpath(entry, "//published/text()")),
                assert(published, string),

                %% pull author field
                assign(author, xpath(entry, "//author/name/text()")),
                assert(author, string),
                
                %% commit published date and event type
                commit({user, published}, {user, published, event_type}),
                
                %% run etap test
                function(fun validate_supported_event_types/1)
            ], [
                %% run etap test
                function(fun validate_unsupported_event_types/1)
            ])
        ])
    ]).
    
validate_supported_event_types(S) ->
    ET = ex_util:fetch(S, event_type),
    Auth = ex_util:fetch(S, author),
    U = ex_util:fetch(S, user),
    etap:ok(ET == "PushEvent" orelse ET == "IssuesEvent", "event type matches"),
    etap:is(string:to_lower(Auth), string:to_lower(U), "author matches").

validate_unsupported_event_types(S) -> 
    ET = ex_util:fetch(S, event_type),
    etap:ok(ET =/= "PushEvent" andalso ET =/= "IssuesEvent", "unsupported event type matches").