-module(gracenote_test_002).
-export([instructions/0]).
-import(excavator_funs, [assign/2, assert/2, commit/2, print/1, print/2, each/2]).

-define(ROOT_URL, "http://www.gracenote.com/search/album_details.php?tui_id=f7ca1dae67cb62e2&tui_tag=").

instructions() ->
	[ assign({url, ?ROOT_URL}, scope),
	  assert(scope, has_text),

	  [ assign({xpath, scope, "//div[@class='track_name']/text()"}, scope), %% returns multiple text elements
	    print(scope),
	    assert(scope, has_list_items),
	
		each(scope, [
			print(scope),
			assert(scope, has_text)
		])
	  ],
	
  	  [ assign({regexp, scope, "<div\sclass=\"track_name\">([^<]*)</div>"}, scope),
		print(scope),
		assert(scope, has_list_items)
	  ]
	].