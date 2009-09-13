%% ex_engine:run(ex_pp:parse("templates/file_tests.ex", ["t/character.xml"])).
main(Filename) ->
    assign(character_xml, read_file(Filename)),
    assign(name, xpath(character_xml, "/character/name/text()")),
    assign(stats, xpath(character_xml, "/character/stats/*/text()")),

	assign(file_handle, open_file(Filename, [read])),
	each(line, file_handle, [
		gadd(lines, line)
	]),
	
    {name, stats, lines}.
