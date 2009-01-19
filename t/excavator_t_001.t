#!/usr/local/bin/escript
%% -*- erlang -*-
%%! -pa ebin

main(_) ->
    etap:plan(1),
	etap:is(true, true, "Test Test"),
    etap:end_tests().
