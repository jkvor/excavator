-module(gracenote_test_001).
-export([instructions/0]).
-import(excavator_funs, [assign/2, assert/2, commit/2, print/1, print/2]).

-define(ROOT_URL, "http://www.gracenote.com/search/album_details.php?tui_id=f7ca1dae67cb62e2&tui_tag=").

instructions() ->
	[ assign({url, ?ROOT_URL}, scope),
	  print(scope),
	  assert(scope, has_list_items),

	  [ assign({xpath, scope, "//div[@class='album-name']/text()"}, scope),
	    print(scope),
	    assert(scope, has_text),
	
		assign({regexp, scope, "\s\>\s(.*)"}, album_name),
		print(album_name),
		assert(album_name, has_text)
	  ]
	].