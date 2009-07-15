## About
Excavator is an Erlang application for ingesting data from various sources (APIs, data feeds, web content, etc). Excavator runs off of template files written in Erlang. Templates contain custom meta functions that are processed into instruction sets for the excavator engine to ingest. A full listing of these custom functions and their definitions can be found below in the __Template Functions__ section.

## Dependencies

<http://github.com/ngerakines/mochiweb>

<http://github.com/JacobVorreuter/mochixpath>

<http://github.com/JacobVorreuter/dynamic_compile>

<http://github.com/ngerakines/etap> (required only if running test suite)

<http://github.com/JacobVorreuter/mochiweb_server_behavior> (required only if running test suite)

## Install

	$ make && sudo make install
	
## Sample Template

__templates/github-public-activity.ex__

	main(Username) ->
	    configure(commit_callback, {io, format}),

	    %% fetch user's public activity
	    assign(public_activity, {http, get, ["http://github.com/", Username, ".atom"]}),
	    assert(public_activity, {status, 200}),

	    each(entry, {xpath, public_activity, "//entry"}, [

	        %% pull entry_id fields
	        assign(entry_id, {xpath, entry, "//id/text()"}),
	        assert(entry_id, string),
    
	        %% pull event_type fields
	        assign(event_type, {regexp, entry_id, "tag:github.com,[0-9]{4}:(.*)/[0-9]*"}),
	        assert(event_type, string),

	        %% process PUSH and ISSUES events
	        condition(event_type == "PushEvent", [
    
	            %% pull published date
	            assign(published, {xpath, entry, "//published/text()"}),
	            assert(published, string),
                
	            %% call commit function (io:format/2 in this case)
	            commit("committing for ~s: ~s, ~s~n", [Username, event_type, published])
	        ])
	    ]).
	
## Running Github Example

	$ erl -boot excavator
	...
	Eshell V5.7.1  (abort with ^G)
	1> ex_loglevel:set(error).
	{module,ex_logger}
	2> ex_engine:run(ex_pp:parse("templates/github-public-activity.ex", ["jacobvorreuter"])).
	
	=PROGRESS REPORT==== 15-Jul-2009::15:46:49 ===
	          supervisor: {local,inet_gethost_native_sup}
	             started: [{pid,<0.75.0>},{mfa,{inet_gethost_native,init,[[]]}}]

	=PROGRESS REPORT==== 15-Jul-2009::15:46:49 ===
	          supervisor: {local,kernel_safe_sup}
	             started: [{pid,<0.74.0>},
	                       {name,inet_gethost_native_sup},
	                       {mfa,{inet_gethost_native,start_link,[]}},
	                       {restart_type,temporary},
	                       {shutdown,1000},
	                       {child_type,worker}]
	committing for jacobvorreuter: PushEvent, 2009-07-15T14:47:00-07:00
	committing for jacobvorreuter: PushEvent, 2009-07-15T12:41:50-07:00
	...
	committing for jacobvorreuter: PushEvent, 2009-06-20T18:30:50-07:00
	ok
	
## Running Tests

	$ make test
	sh ebin/excavator.app.in 0.0.1
	mkdir -p ebin/
	(cd src;make)
	make[1]: Nothing to be done for `all'.
	(cd t;make)
	make[1]: Nothing to be done for `all'.
	prove t/*.t
	t/excavator_t_001....ok                                                      
	t/excavator_t_002....ok                                                      
	t/excavator_t_003....ok                                                      
	t/excavator_t_004....ok                                                      
	t/excavator_t_005....ok                                                      
	All tests successful.
	Files=5, Tests=212,  3 wallclock secs ( 1.97 cusr +  0.51 csys =  2.48 CPU)

## Template Functions
* __configure__: set a configuration parameter. The following examples illustrate supported conifguration parameters:

		configure(qps, 10) %% queries-per-second
		configure(commit_callback, {my_monkey_maker, new_monkey})
* __assign__: assign a static value to a state key in the current scope

		assign(user_ids, {xpath, users_xml, "//id/text()"})
		assign(user_ids, {regexp, users_txt, "id=([0-9]+)"}),
		assign(pokemons, {http, get, "http://yummymeatwhiz.com/?page_id=223"})
		assign(new_pokemon, {http, post, "http://yummymeatwhiz.com/?page_id=223", [{application_id, "monkeybrains"}], <<"Let me show you it">>})
* __gassign__: the same as assign, but associates value with state key in the global scope
* __add__: similar to assign, but instead of overwriting the value of a state key it will append the value and build up a list

		add(user_ids, {xpath, user_xml, "//id/text()"})
* __gadd__: the same as add, but global
* __assert__: assert that the value associated with a state key meets certain criteria. The following assertions are supported:

		assert(user_id, string)
		assert(user_ids, list_of_strings)
		assert(user_info, node)
		assert(user_xml, list_of_nodes)
		assert(users, mixed)
		assert(user, nil)
		assert(user_page, {status, 200}),
* __print___: evaluates a term and prints its value through the error_logger

		print(username)
		print("Hello World")
* __commit___: calls either the default commit mfa, or the commit_callback module and function set as a configuration parameter with the arguments given. 
 
		commit(username, profile_url, list_of_friends)
		commit(username)
* __each__: iterate over a list or range
		each(day, ["Monday", "Tuesday", "Wednesday"], [
			print(day)
		])

		each(page_num, {range, 1, 10}, [
			print(page_num)
		])
		
		assign(users, {xpath, friend_list, "//friend"}),
		each(user, users, [
			print(user)
		])
* __condition__: execute a block of code based on a boolean condition. State keys will be evaluated and replaced with their corresponding values before the condition is evaluated.

		condition(username == "monkey", [
			print("Hello monkey")
		])
		condition(names == [], [
			print("list of names is empty")
		], [
			print(names)
		])
* __onfail__: suppress exceptions of a certain type inside a block of code

		onfail({assertion_failed, {users_xml, '_', {status, 200}}}, [
			assign(pokemons, {http, get, "http://yummymeatwhiz.com/?page_id=223"})
			assert(pokemons, {status, 200}),
			print("if the pokemons request did not return successfully, this instruction will not execute")
		]),
		print("this instruction will always execute")
* __function__: provides a mechanism for executing custom logic. The instruction expects a single parameter, which is a function or arity 1. The function's argument is the state object.

		function(fun(State) -> io:format("POKEMONS!! ~p~n", [ex_util:fetch_value(State, pokemons)]) end)
	
## License
	%% Copyright (c) 2009 Jacob Vorreuter <jacob.vorreuter@gmail.com>
	%% 
	%% Permission is hereby granted, free of charge, to any person
	%% obtaining a copy of this software and associated documentation
	%% files (the "Software"), to deal in the Software without
	%% restriction, including without limitation the rights to use,
	%% copy, modify, merge, publish, distribute, sublicense, and/or sell
	%% copies of the Software, and to permit persons to whom the
	%% Software is furnished to do so, subject to the following
	%% conditions:
	%% 
	%% The above copyright notice and this permission notice shall be
	%% included in all copies or substantial portions of the Software.
	%% 
	%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
	%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
	%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
	%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
	%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
	%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
	%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
	%% OTHER DEALINGS IN THE SOFTWARE.