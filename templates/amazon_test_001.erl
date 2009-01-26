-module(amazon_test_001).
-export([instructions/0]).
-import(excavator_funs, [assign/2, assert/2, commit/2, print/1, print/2]).

-define(ROOT_URL, "http://www.amazon.com/s/qid=1232337093/ref=sr_pg_2/182-6187393-2698830?ie=UTF8&rs=14545&rh=n%3A75%2Cn%3A14545&page=2").

instructions() ->
	[ assign({url, ?ROOT_URL}, scope),
	  print(scope),
	  assert(scope, has_list_items),
	
	  [ assign({xpath, scope, "//div[@id='searchTemplate']"}, scope),
	    print(scope),
	    assert(scope, html_tree_has_nodes),
	
		[ assign({xpath, scope, "//div[@class='resultCount']/text()"}, scope),
		  print(scope),
		  assert(scope, has_text) %,
		  
		  %assign({regexp, scope, "Showing [0-9]+ - [0-9]+ of ([0-9,]+) Results"}, total_results),
		  %assert(total_results, has_text),
		  %commit(total_results, int32)
		]
	  ]
	].