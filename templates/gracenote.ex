main() ->
	configure(qps, 10),
	configure(commit_callback, {ex_default_storage, store}),
	fetch(artist_page, {get, "http://127.0.0.1:8888/gracenote_albums.html"}),
	assert(artist_page, {status, 200}),
	assert(artist_page, string),
	assign(albums, {xpath, artist_page, "//div[@class='album-meta-data-wrapper']"}),
	assert(albums, list_of_nodes),
	each(album, albums, [
		assign(album_href, {xpath, album, "//a[1]/@href"}),
		assign(album_id, {regexp, album_href, "tui_id=(.*)tui"}),
		assert(album_id, string),
		fetch(album_page, {get, ["http://127.0.0.1:8888/gracenote_album_", album_id, ".html"]}),
		onfail(
			{assertion_failed, {album_page, '_', {status, 200}}},
			[	assert(album_page, {status, 200}),
				assert(album_page, string),
				assign(album_name_node, {xpath, album_page, "//div[@class='album-name']"}),
				assert(album_name_node, node),
				assign(album_name, {regexp, album_name_node, " &gt; (.*)</div>"}),
				assert(album_name, string),
				commit({album, beatles}, {album_id, album_name})])
	]).