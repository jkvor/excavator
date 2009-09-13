#!/usr/local/bin/escript
%% -*- erlang -*-
%%! -pa ../excavator ./ebin -boot start_sasl -noshell

main(_) ->
    etap:plan(unknown),
    case (catch start()) of
        {'EXIT', Err} ->
            io:format("# ~p~n", [Err]),
            etap:bail();
        _ ->
            etap:end_tests()
    end,
    ok.
    
start() ->
	error_logger:tty(false),
    application:start(inets),
    application:start(excavator),

    Instrs = ex_pp:parse("templates/file_tests.ex", ["t/character.xml"]),
    {Name, Stats, Lines} = ex_engine:run(Instrs),

	etap:is(Name, "Korale", "name matches ok"),
	etap:is(Stats, ["814","1103","9","401","20"], "stats match ok"),
	
	OrigLines = [
	 "<character>\n",
	 "    <name>Korale</name>\n",
	 "    <realm>Medivh</realm>\n",
	 "    <realm_class>US</realm_class>\n",
	 "    <race>Human</race>\n",
	 "    <class>Priest</class>\n",
	 "    <stats>\n",
	 "        <int>814</int>\n",
	 "        <wis>1103</wis>\n",
	 "        <str>9</str>\n",
	 "        <sta>401</sta>\n",
	 "        <agi>20</agi>\n",
	 "    </stats>\n"
	],
	etap:is(Lines, OrigLines, "file lines match ok"),

    ok.