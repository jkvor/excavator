[   configure(qps, 10),
    configure(commit_callback, {ex_default_storage, store}),
    
    fetch(beatles_artist_page, "http://gracenote.com/artists/the-beatles"),
    assert(beatles_artist_page, string),

    assign(beatles_albums, xpath(beatles_artist_page, "//div[@class='album']")),
    assert(beatles_albums, list_of_nodes),

    each(album, beatles_albums, [
        assign(album_id, regexp(album, "id=([0-9]*)")),
        assert(album_id, string),
        
        fetch(beatles_album_page, ["http://gracenote.com/albums/", album_id]),
        assert(beatles_album_page, string),
        
        assign(album_name, xpath(beatles_album_page, "//div[@class='name']/text()")),
        assert(album_name, string),
        
        commit({album, beatles}, {album_id, album_name})
    ]
].