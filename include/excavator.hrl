-record(state, {
    instructions=[], %% [instr()]
    dictionary=dict:new(), %% term store
    parent, %% state()
	fail, %% {Error::tuple(), FailInstructions::list()}
	request_times = [], %% [seconds()]
    configuration=dict:new()
}).

% -define(INFO_MSG, fun(Format0, Args0) -> io:format(Format0, Args0) end).
% -define(INFO_REPORT, fun(Args0) -> io:format("~p~n", [Args0]) end).
% -define(ERR_MSG, fun(Format0, Args0) -> io:format(Format0, Args0) end).

-define(INFO_REPORT, fun(Args0) -> error_logger:info_report(Args0) end).
-define(INFO_MSG, fun(Format0, Args0) -> error_logger:info_msg(Format0, Args0) end).
-define(ERROR_MSG, fun(Format0, Args0) -> error_logger:error_msg(Format0, Args0) end).

% -define(INFO_MSG, fun(_, _) -> ok end).
% -define(INFO_REPORT, fun(_) -> ok end).
% -define(ERR_MSG, fun(_, _) -> ok end).

-define(STORE, fun ex_util:store/3).
-define(FETCH, fun ex_util:fetch/2).
-define(CONFIGURE, fun ex_util:configure/3).
-define(FETCH_CONFIG, fun ex_util:fetch_config/2).    
-define(EVALUATE, fun ex_util:evaluate/2).