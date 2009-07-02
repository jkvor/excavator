% ex_engine:run(ex_pp:parse("t/truly-free.ex", [])).
main() ->
    configure(commit_callback, {truly_free, commit_links}),
    
    fetch(truly_free_page, {get, "http://truly-free.org"}),
    assert(truly_free_page, {status, 200}),

    assign(book_links, {xpath, truly_free_page, "//p/a/@href"}),
    assert(book_links, list_of_strings),
    
    each(link, book_links, [
        print(link),
        assign(link2, {regexp, link, "zip$"}),
        print(link2)
    ]).
