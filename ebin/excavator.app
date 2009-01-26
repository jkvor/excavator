{application, excavator, [
    {description, "Erlang Data Miner"},
    {vsn, "0.1.1"},
    {modules, [
        excavator,
		excavator_crawler
    ]},
    {registered, []},
    {mod, {excavator, []}},
    {applications, [kernel, stdlib, inets]}
]}.
