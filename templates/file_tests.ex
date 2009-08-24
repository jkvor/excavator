%% ex_engine:run(ex_pp:parse("templates/file_tests.ex", ["t/character.xml"])).
main(Filename) ->
    assign(character_xml, read_file(Filename)),
    assign(name, xpath(character_xml, "/character/name/text()")),
    assign(stats, xpath(character_xml, "/character/stats/*/text()")),
    {name, stats}.
