{application, excavator, [
    {description, "Erlang Data Miner"},
    {vsn, "0.1.1"},
    {modules, [
        excavator,
		ex_consumer,
		ex_default_storage,
		ex_engine,
		ex_pp,
		ex_re,
		ex_scheduler,
		ex_util,
		ex_web,
		ex_xpath
    ]},
    {registered, []},
    {mod, {excavator, []}},
    {applications, [kernel, stdlib, inets]}
]}.
