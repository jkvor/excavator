%% ex_engine:run(ex_pp:parse("templates/file_tests.ex", ["t/character.xml"])).
main(Filename) ->
    assign(character_xml, {file, Filename}),
    assign(name, {xpath, character_xml, "/character/name"}),
    assign(stats, {xpath, character_xml, "/character/stats/*"}),
    {name, stats}.
