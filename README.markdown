## About
excavator is an Erlang application for ingesting data from various 
sources (APIs, data feeds, web content, etc).

## Dependencies
<http://github.com/ngerakines/mochiweb>

<http://github.com/JacobVorreuter/mochixpath>

<http://github.com/JacobVorreuter/dynamic_compile>

<http://github.com/ngerakines/etap> (required only if running test suite)

<http://github.com/JacobVorreuter/mochiweb_server_behavior> (required only if running test suite)

## Templates
excavator runs off of template files written in Erlang. Templates contain custom meta functions that are processed into instruction sets for the excavator engine to ingest. A full listing of these custom functions and their definitions can be found below in the __Template Functions__ section.

## Template Functions
* __configure__: set a configuration parameter. The following examples illustrate supported conifguration parameters:

		configure(qps, 10) %% queries-per-second
		configure(commit_callback, {my_monkey_maker, new_monkey})
* __assign__: assign a static value of result of an instruction to a state key in the current scope

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
* __commit___: calls either the default commit mfa, or the commit_callback module and function set as a configuration parameter with a commit_key/state_key pair as the arguments. The commit_key is used to overload the commit function and allow multiple commit points in a template. The state_key is used to pull the associated value and pass that in as the second argument to the commit function.
 
		commit({user, friend_profile}, profile_url)
		commit(username, username)
* __each__: iterate over a list associated with a state key

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

## Example Templates
The convention when creating excavator templates is to use the .ex extension. The file must contain a main/1 function and valid erlang code.

	main([Username, Environment]) ->
		assign(username, Username),
		assert(username, string),
		
		condition(Environment == [], [
			gassign(pokemons_page, {http, get, ["http://yummymeatwhiz.com/", username, "/pokemons"]})
		], [
			gassign(pokemons_page, {http, get, ["http://", env, ".yummymeatwhiz.com/", username, "/pokemons"]})
		]),
		assert(pokemons_page, {status, 200}),
		
		assign(pokemons, {xpath, pokemons_page, "//div[@class='pokemon']"}),
		assert(pokemons, list_of_nodes),
		
		each(pokemon, pokemons, [
			assign(name, {xpath, pokemon, "//div[@class='name']/text()"}),
			assert(name, string),
			commit(pokemon_name, name)
		]),
		
		condition(name == undefined, [
			%% name will always be undefined because its
			%% value was assigned in a child scope. Had
			%% name been assigned using the gassign instruction,
			%% a value would be present in the current scope.
			print("name is undefined")
		]).

## Getting started

	$ make rel
	$ sudo make install
	$ erl -name excavator@`hostname` +W w +A 1 +Ktrue -boot excavator
		
## Running excavator
To initiate a single run of a template you must parse the template and then feed the instructions to the excavator engine:

	(excavator@idk.local)1> ex_engine:run(ex_pp:parse("templates/github.ex", ["jvorreuter", "ngerakines"])).
		
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