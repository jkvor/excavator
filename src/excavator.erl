-module(excavator).

-behaviour(application).

-export([start/2,stop/1, init/1]).
-export([stop/0, build_rel/0]).

stop() ->
    [Node | _] = init:get_plain_arguments(),
    Resp = rpc:call(list_to_atom(Node), init, stop, []),
    Resp.

start(_, _) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_) ->
    ok.

init(_) ->
    {ok, {{one_for_one, 10, 10}, [
        {excavator_crawler, {excavator_crawler, start_link, [none]}, permanent, 5000, worker, [excavator_crawler]}
    ]}}.

build_rel() ->
    {ok, FD} = file:open("excavator.rel", [write]),
    RootDir = code:root_dir(),
    Patterns = [
        {RootDir ++ "/", "erts-*"},
        {RootDir ++ "/lib/", "kernel-*"},
        {RootDir ++ "/lib/", "stdlib-*"},
        {RootDir ++ "/lib/", "sasl-*"},
        {RootDir ++ "/lib/", "crypto-*"},
		{RootDir ++ "/lib/", "inets-*"}
    ],
    [Erts, Kerne, Stdl, Sasl, Crypto, Inets] = [begin
        [R | _ ] = filelib:wildcard(P, D),
        [_ | [Ra] ] = string:tokens(R, "-"),
        Ra
    end || {D, P} <- Patterns],
    RelInfo = {release,
        {"excavator", "0.1.1"},
        {erts, Erts}, [
            {kernel, Kerne},
            {stdlib, Stdl},
            {sasl, Sasl},
            {crypto, Crypto},
			{inets, Inets},
            {excavator, "0.1.1"}
        ]
    },
    io:format(FD, "~p.", [RelInfo]),
    file:close(FD),
    systools:make_script("excavator", [local]),
    ok.