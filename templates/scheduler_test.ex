main() ->
	configure(sleep_time, 10 * 1000),
	
	assign(public_activity, request(get, "http://127.0.0.1:8888/github-jacobvorreuter.xml")),
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
	        call(test_callback, scheduler_commit, [published, author, event_type])
	    ])
	]),

	ok.