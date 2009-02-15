-module(sevenxseven_test_001).
-export([instructions/0]).
-import(excavator_funs, [assign/2, assert/2, commit/2, print/1, print/2]).

-define(ROOT_URL, "http://www.7x7.com/content/eat-drink/big-eat-sf-100-things-try-you-die").

instructions() ->
	[ assign({url, ?ROOT_URL}, scope),
	  assert(scope, has_text),
	
	  assign({xpath, scope, "//div[@class='body']/p"}, paragraphs),
	  assert(paragraphs, has_list_items),
	  each(paragraphs, [
	   assign({xpath, paragraphs, "//strong[2]/text()"}, restaurant),
		print(restaurant)
	  ])
	],

