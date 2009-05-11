-module(ex_scheduler).
-export([add/1]).

%% @spec add()
add(Filename) -> 
	spawn_link(fun() -> loop(Filename) end).
	
loop(Filename) ->
	io:format("running instructions for ~w~n", [Filename]),
	Instrs = ex_pp:parse(Filename),
	ok = ex_engine:run(Instrs),
	loop(Filename).