[   throttle(qps, 10),
    
    fetch(beatles_artist_page, "http://gracenote.com/artists/the-beatles"),
    assert(beatles_artist_page, list(char())),

    assign(beatles_albums, xpath(beatles_artist_page, "//div[@class='album']")),
    assert(beatles_albums, list(node())),

    each(album, beatles_albums, [
        assign(album_id, regexp(album, "id=([0-9]*)")),
        assert(album_id, list(char())),
        
        fetch(beatles_album_page, ["http://gracenote.com/albums/", album_id]),
        assert(beatles_album_page, list(char())),
        
        assign(album_name, xpath(beatles_album_page, "//div[@class='name']/text()")),
        assert(album_name, list(char())),
        
        commit({album, beatles}, {album_id, album_name})
    ]
].

[   {instr, configure, [qps, 10]},
    {instr, fetch, [beatles_artist_page, {get, "http://gracenote.com/artists/the-beatles"}]},
    {instr, assert, [beatles_artist_page, string]},
    {instr, assign, [beatles_albums, {xpath, beatles_artist_page, "//div[@class='album']"}]},
    {instr, assert, [beatles_albums, list_of_nodes]},
    {instr, each, [album, beatles_albums, [
        {instr, assign, [album_id, {regexp, album, "id=([0-9]*)"}]},
        {instr, assert, [album_id, string]},
        {instr, fetch, [beatles_album_page, ["http://gracenote.com/albums/", album_id]]},
        {instr, assert, [beatles_album_page, string]},
        {instr, assign, [album_name, {xpath, beatles_album_page, "//div[@class='name']/text()"}]},
        {instr, assert, [album_name, string]},
        {instr, commit, [{album, beatles}, {album_id, album_name}]}
    ]]}
].